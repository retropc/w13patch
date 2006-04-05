unit Compress;

{
  Inno Setup
  Copyright (C) 1997-2004 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Abstract compression classes, and some generic compression-related functions

  $jrsoftware: issrc/Projects/Compress.pas,v 1.6 2004/02/28 02:16:16 jr Exp $
}

interface

uses
  SysUtils, Classes;

type
  ECompressError = class(Exception);
  ECompressDataError = class(ECompressError);
  ECompressInternalError = class(ECompressError);

  TCompressorProgressProc = procedure(BytesProcessed: cardinal) of object;
  TCompressorWriteProc = procedure(const Buffer; Count: Longint) of object;
  TCustomCompressorClass = class of TCustomCompressor;
  TCustomCompressor = class
  private
    FProgressProc: TCompressorProgressProc;
    FWriteProc: TCompressorWriteProc;
  protected
    property ProgressProc: TCompressorProgressProc read FProgressProc;
    property WriteProc: TCompressorWriteProc read FWriteProc;
  public
    constructor Create(AWriteProc: TCompressorWriteProc;
      AProgressProc: TCompressorProgressProc; CompressionLevel: Integer); virtual;
    procedure Compress(const Buffer; Count: Longint); virtual; abstract;
    procedure Finish; virtual; abstract;
  end;

  TDecompressorReadProc = function(var Buffer; Count: Longint): Longint of object;
  TCustomDecompressorClass = class of TCustomDecompressor;
  TCustomDecompressor = class
  private
    FReadProc: TDecompressorReadProc;
  protected
    property ReadProc: TDecompressorReadProc read FReadProc;
  public
    constructor Create(AReadProc: TDecompressorReadProc); virtual;
    function DecompressInto(var Buffer; Count: Longint): longint; virtual; abstract;
    procedure Reset; virtual; abstract;
  end;

  TCompressedBlockWriter = class(TStream)
  private
    FCompressor: TCustomCompressor;
    FFile: TStream;
    FStartPos: Int64;
    FTotalBytesStored: Cardinal;
    FInBufferCount, FOutBufferCount: Cardinal;
    FInBuffer, FOutBuffer: array[0..4095] of Byte;
    procedure CompressorWriteProc(const Buffer; Count: Longint);
    procedure DoCompress(const Buf; var Count: Cardinal);
    procedure FlushOutputBuffer;
  public
    constructor Create(AFile: TStream; ACompressorClass: TCustomCompressorClass; CompressionLevel: Integer; AProgressProc: TCompressorProgressProc = nil); 
    destructor Destroy; override;
    procedure Finish;
    function Write(const Buffer; Count: longint): longint; override;
    function Read(var Buffer; Count: longint): longint; override;
    function CopyFrom(Source: TStream; Count: Int64): Int64;
    function Seek(AOffset: Longint; AOrigin: Word): Longint; override;
  end;

  TCompressedBlockReader = class(TStream)
  private
    FDecompressor: TCustomDecompressor;
    FFile: TStream;
    FInBytesLeft: Cardinal;
    FInitialized: Boolean;
    FInBufferNext: Cardinal;
    FInBufferAvail: Cardinal;
    FInBuffer: array[0..4095] of Byte;
    function DecompressorReadProc(var Buffer; Count: Longint): Longint;
    procedure ReadChunk;
  public
//    constructor Create(AFile: TStream; ADecompressorClass: TCustomDecompressorClass);
    constructor Create(AFile: TStream; ADecompressorClass: TCustomDecompressorClass; const ADataLength: int64);
    destructor Destroy; override;
    function Write(const Buffer; Count: longint): longint; override;
    function Read(var Buffer; Count: longint): longint; override;
    function Seek(AOffset: Longint; AOrigin: Word): Longint; override;
  end;

function GetCRC32(const Buf; BufSize: Cardinal): Longint;
procedure TransformCallInstructions(var Buf; Size: Integer; const Encode: Boolean);
function UpdateCRC32(CurCRC: Longint; const Buf; BufSize: Cardinal): Longint;

implementation

const
  SCompressedBlockDataError = 'Compressed block is corrupted';

var
  CRC32TableInited: Boolean;
  CRC32Table: array[Byte] of Longint;

procedure InitCRC32Table;
var
  CRC: Longint;
  I, N: Integer;
begin
  for I := 0 to 255 do begin
    CRC := I;
    for N := 0 to 7 do begin
      if Odd(CRC) then
        CRC := (CRC shr 1) xor Longint($EDB88320)
      else
        CRC := CRC shr 1;
    end;
    Crc32Table[I] := CRC;
  end;
end;

function UpdateCRC32(CurCRC: Longint; const Buf; BufSize: Cardinal): Longint;
var
  P: ^Byte;
begin
  if not CRC32TableInited then begin
    InitCRC32Table;
    CRC32TableInited := True;
  end;
  P := @Buf;
  while BufSize <> 0 do begin
    CurCRC := CRC32Table[Lo(CurCRC) xor P^] xor (CurCRC shr 8);
    Dec(BufSize);
    Inc(P);
  end;
  Result := CurCRC;
end;

function GetCRC32(const Buf; BufSize: Cardinal): Longint;
begin
  Result := UpdateCRC32(Longint($FFFFFFFF), Buf, BufSize) xor Longint($FFFFFFFF);
end;

procedure TransformCallInstructions(var Buf; Size: Integer; const Encode: Boolean);
{ Transforms addresses in relative CALL or JMP instructions to absolute ones
  if Encode is True, or the inverse if Encode is False.
  This transformation can lead to a higher compression ratio when compressing
  32-bit x86 code. }
type
  PByteArray = ^TByteArray;
  TByteArray = array[0..$7FFFFFFE] of Byte;
var
  P: PByteArray;
  I: Integer;
begin
  if Size < 5 then
    Exit;
  Dec(Size, 4);
  P := @Buf;
  I := 0;
  while I < Size do begin
    { Does it appear to be a CALL or JMP instruction with a relative 32-bit
      address? }
    if (P[I] = $E8) or (P[I] = $E9) then begin
      { Change the address to be relative to the beginning of the buffer,
        instead of relative to the next instruction. If decoding, do the
        opposite. }
      Inc(I, 5);
      if Encode then
        Inc(Longint((@P[I-4])^), I)
      else
        Dec(Longint((@P[I-4])^), I);
    end
    else
      Inc(I);
  end;
end;

{ TCustomCompressor }

constructor TCustomCompressor.Create(AWriteProc: TCompressorWriteProc;
  AProgressProc: TCompressorProgressProc; CompressionLevel: Integer);
begin
  inherited Create;
  FWriteProc := AWriteProc;
  FProgressProc := AProgressProc;
end;

{ TCustomDecompressor }

constructor TCustomDecompressor.Create(AReadProc: TDecompressorReadProc);
begin
  inherited Create;
  FReadProc := AReadProc;
end;

{ TCompressedBlockWriter }

type
  TCompressedBlockHeader = packed record
    StoredSize: LongWord;   { Total bytes written, including the CRCs }
    Compressed: Boolean;    { True if data is compressed, False if not }
  end;

constructor TCompressedBlockWriter.Create(AFile: TStream;
  ACompressorClass: TCustomCompressorClass; CompressionLevel: Integer; AProgressProc: TCompressorProgressProc = nil);
begin
  inherited Create;

  FFile := AFile;
  if Assigned(ACompressorClass) and (CompressionLevel <> 0) then
    FCompressor := ACompressorClass.Create(CompressorWriteProc, AProgressProc, CompressionLevel);
  FStartPos := AFile.Position;
end;

destructor TCompressedBlockWriter.Destroy;
begin
  FCompressor.Free;
  inherited;
end;

procedure TCompressedBlockWriter.FlushOutputBuffer;
{ Flushes contents of FOutBuffer into the file, with a preceding CRC }
var
  CRC: Longint;
begin
  CRC := GetCRC32(FOutBuffer, FOutBufferCount);
  FFile.WriteBuffer(CRC, SizeOf(CRC));
  Inc(FTotalBytesStored, SizeOf(CRC));
  FFile.WriteBuffer(FOutBuffer, FOutBufferCount);
  Inc(FTotalBytesStored, FOutBufferCount);
  FOutBufferCount := 0;
end;

procedure TCompressedBlockWriter.CompressorWriteProc(const Buffer; Count: Longint);
var
  P: ^Byte;
  Bytes: Cardinal;
begin
  P := @Buffer;
  while Count > 0 do begin
    Bytes := Count;
    if Bytes > SizeOf(FOutBuffer) - FOutBufferCount then
      Bytes := SizeOf(FOutBuffer) - FOutBufferCount;
    Move(P^, FOutBuffer[FOutBufferCount], Bytes);
    Inc(FOutBufferCount, Bytes);
    if FOutBufferCount = SizeOf(FOutBuffer) then
      FlushOutputBuffer;
    Dec(Count, Bytes);
    Inc(P, Bytes);
  end;
end;

procedure TCompressedBlockWriter.DoCompress(const Buf; var Count: Cardinal);
begin
  if Count > 0 then begin
    if Assigned(FCompressor) then
      FCompressor.Compress(Buf, Count)
    else
      CompressorWriteProc(Buf, Count);
  end;
  Count := 0;
end;

function TCompressedBlockWriter.Write(const Buffer; Count: longint): longint;
var
  P: ^Byte;
  Bytes: Cardinal;
begin
  { Writes are buffered strictly as an optimization, to avoid feeding tiny
    blocks to the compressor }
  P := @Buffer;
  while Count > 0 do begin
    Bytes := Count;
    if Bytes > SizeOf(FInBuffer) - FInBufferCount then
      Bytes := SizeOf(FInBuffer) - FInBufferCount;
    Move(P^, FInBuffer[FInBufferCount], Bytes);
    Inc(FInBufferCount, Bytes);
    if FInBufferCount = SizeOf(FInBuffer) then
      DoCompress(FInBuffer, FInBufferCount);
    Dec(Count, Bytes);
    Inc(P, Bytes);
  end;
  Result := Count;
end;

procedure TCompressedBlockWriter.Finish;
begin
  DoCompress(FInBuffer, FInBufferCount);
  if Assigned(FCompressor) then
    FCompressor.Finish;
  if FOutBufferCount > 0 then
    FlushOutputBuffer;

end;

function TCompressedBlockWriter.Read(var Buffer; Count: Integer): longint;
begin
  raise Exception.Create('Unable to read TCompressedBlockWriter!');
end;

function TCompressedBlockWriter.Seek(AOffset: Integer;
  AOrigin: Word): Longint;
begin
  raise Exception.Create('Cannot seek on TCompressedBlockWriter!');
end;

{ TCompressedBlockReader }

constructor TCompressedBlockReader.Create(AFile: TStream;
  ADecompressorClass: TCustomDecompressorClass; const ADataLength: int64);
var
  Hdr: TCompressedBlockHeader;
begin
  inherited Create;

  FFile := AFile;

{  if (AFile.Read(HdrCRC, SizeOf(HdrCRC)) <> SizeOf(HdrCRC)) or
     (AFile.Read(Hdr, SizeOf(Hdr)) <> SizeOf(Hdr)) then
    raise ECompressDataError.Create(SCompressedBlockDataError);
  if HdrCRC <> GetCRC32(Hdr, SizeOf(Hdr)) then
    raise ECompressDataError.Create(SCompressedBlockDataError);
}
  Hdr.StoredSize := ADataLength;
  Hdr.Compressed := true;
{  P := AFile.Position;
  Inc(P, Hdr.StoredSize);
  if P > AFile.Size then
    raise ECompressDataError.Create(SCompressedBlockDataError);}
  if Hdr.Compressed then
    FDecompressor := ADecompressorClass.Create(DecompressorReadProc);
  FInBytesLeft := Hdr.StoredSize;
  FInitialized := True;
end;

destructor TCompressedBlockReader.Destroy;
var
  P: Int64;
begin
  FDecompressor.Free;
  if FInitialized then begin
    { Must seek ahead if the caller didn't read everything that was originally
      compressed, or if it did read everything but zlib is in a "CHECK" state
      (i.e. it didn't read and verify the trailing adler32 yet due to lack of
      input bytes). }
    P := FFile.Position;
    Inc(P, FInBytesLeft);
    FFile.Seek(P, soBeginning);
  end;
  inherited;
end;

procedure TCompressedBlockReader.ReadChunk;
var
  CRC: Longint;
  Len: Cardinal;
begin
  { Read chunk CRC }
  if FInBytesLeft < SizeOf(CRC) + 1 then
    raise ECompressDataError.Create(SCompressedBlockDataError);
  FFile.ReadBuffer(CRC, SizeOf(CRC));
  Dec(FInBytesLeft, SizeOf(CRC));

  { Read chunk data }
  Len := FInBytesLeft;
  if Len > SizeOf(FInBuffer) then
    Len := SizeOf(FInBuffer);
  FFile.ReadBuffer(FInBuffer, Len);
  Dec(FInBytesLeft, Len);
  FInBufferNext := 0;
  FInBufferAvail := Len;
  if CRC <> GetCRC32(FInBuffer, Len) then
    raise ECompressDataError.Create(SCompressedBlockDataError);
end;

function TCompressedBlockReader.DecompressorReadProc(var Buffer;
  Count: Longint): Longint;
var
  P: ^Byte;
  Bytes: Cardinal;
begin
  Result := 0;
  P := @Buffer;
  while Count > 0 do begin
    if FInBufferAvail = 0 then begin
      if FInBytesLeft = 0 then
        Break;
      ReadChunk;
    end;
    Bytes := Count;
    if Bytes > FInBufferAvail then
      Bytes := FInBufferAvail;
    Move(FInBuffer[FInBufferNext], P^, Bytes);
    Inc(FInBufferNext, Bytes);
    Dec(FInBufferAvail, Bytes);
    Inc(P, Bytes);
    Dec(Count, Bytes);
    Inc(Result, Bytes);
  end;
end;

function TCompressedBlockReader.Read(var Buffer; Count: longint): longint;
begin
  Result := 0;
  if Assigned(FDecompressor) then
    Result := FDecompressor.DecompressInto(Buffer, Count)
  else begin
    { Not compressed -- call DecompressorReadProc directly }
    if DecompressorReadProc(Buffer, Count) <> Count then
      raise ECompressDataError.Create(SCompressedBlockDataError);
  end;
end;

function TCompressedBlockWriter.CopyFrom(Source: TStream; Count: Int64): Int64;
const
  MaxBufSize = $F000;
var
  BufSize, N: Integer;
  Buffer: PChar;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end;
  Result := Count;
  if Count > MaxBufSize then BufSize := MaxBufSize else BufSize := Count;
  GetMem(Buffer, BufSize);
  try
    while Count <> 0 do
    begin
      if Count > BufSize then N := BufSize else N := Count;
      Source.Read(Buffer^, N);
      Write(Buffer^, N);
      Dec(Count, N);
    end;
    Finish;
  finally
    FreeMem(Buffer, BufSize);
  end;
end;

function TCompressedBlockReader.Write(const Buffer;
  Count: Integer): longint;
begin
  raise Exception.Create('Unable to write to TCompressedBlockRead!');
end;

function TCompressedBlockReader.Seek(AOffset: Integer;
  AOrigin: Word): Longint;
begin
  raise Exception.Create('Cannot seek on TCompressedBlockReader!');
end;

end.
