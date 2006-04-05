unit Int64Em;

{
  Inno Setup
  Copyright (C) 1998-2003 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Declaration of the Integer64 type - which represents an *unsigned* 64-bit
  integer value - and functions for manipulating Integer64's.
  (We can't use the Int64 type since it's only available in Delphi 4 and
  later.)

  $jrsoftware: issrc/Projects/Int64Em.pas,v 1.7 2004/01/14 01:44:49 jr Exp $
}

interface

{$I VERSION.INC}

type
  {$IFNDEF IS_D4}
  LongWord = Cardinal;
  {$ENDIF}

  Integer64 = packed record
    Lo, Hi: LongWord;
  end;

function Compare64(const N1, N2: Integer64): Integer;
procedure Dec64(var X: Integer64; N: LongWord);
procedure Dec6464(var X: Integer64; const N: Integer64);
function Div64(var X: Integer64; const Divisor: LongWord): LongWord;
procedure Inc64(var X: Integer64; N: LongWord);
procedure Inc6464(var X: Integer64; const N: Integer64);
function Mod64(const X: Integer64; const Divisor: LongWord): LongWord;
procedure Mul64(var X: Integer64; N: LongWord);
procedure Multiply32x32to64(N1, N2: LongWord; var X: Integer64);
procedure Shr64(var X: Integer64; Count: LongWord);
function StrToInteger64(const S: String; var X: Integer64): Boolean;

implementation

uses
  SysUtils;

function Compare64(const N1, N2: Integer64): Integer;
{ If N1 = N2, returns 0.
  If N1 > N2, returns 1.
  If N1 < N2, returns -1. }
asm
  { Compare high words }
  mov  ecx, [eax+4]
  cmp  ecx, [edx+4]
  ja   @@return1
  jb   @@returnminus1
  { High words equal; compare low words }
  mov  ecx, [eax]
  cmp  ecx, [edx]
  ja   @@return1
  jb   @@returnminus1
  jmp  @@return0
@@return1:
  xor  eax, eax
  inc  eax
  jmp  @@exit
@@returnminus1:
  or   eax, -1
  jmp  @@exit
@@return0:
  xor  eax,eax
@@exit:
end;

procedure Dec64(var X: Integer64; N: LongWord);
asm
  sub  [eax], edx
  sbb  dword ptr [eax+4], 0
end;

procedure Dec6464(var X: Integer64; const N: Integer64);
asm
  mov  ecx, [edx]
  sub  [eax], ecx
  mov  ecx, [edx+4]
  sbb  [eax+4], ecx
end;

procedure Inc64(var X: Integer64; N: LongWord);
asm
  add  [eax], edx
  adc  dword ptr [eax+4], 0
end;

procedure Inc6464(var X: Integer64; const N: Integer64);
asm
  mov  ecx, [edx]
  add  [eax], ecx
  mov  ecx, [edx+4]
  adc  [eax+4], ecx
end;

procedure Multiply32x32to64(N1, N2: LongWord; var X: Integer64);
{ Multiplies two 32-bit unsigned integers together and places the result
  in X. }
asm
  mul  edx    { Multiplies EAX by EDX, places 64-bit result in EDX:EAX }
  mov  [ecx], eax
  mov  [ecx+4], edx
end;

procedure Mul64(var X: Integer64; N: LongWord);
{ Multiplies X by N, and overwrites X with the result. }
asm
  push esi
  mov  esi, eax
  mov  ecx, edx

  mov  eax, [esi+4]
  mul  edx
  mov  [esi+4], eax

  mov  eax, [esi]
  mov  edx, ecx
  mul  edx
  mov  [esi], eax
  add  [esi+4], edx

  pop  esi
end;

function Div64(var X: Integer64; const Divisor: LongWord): LongWord;
{ Divides X by Divisor, and overwrites X with the quotient. Returns the
  remainder. }
asm
  push ebx
  push esi
  mov  esi, eax
  mov  ecx, edx

  mov  eax, [esi]
  mov  ebx, [esi+4]

  { Divide EBX:EAX by ECX. Quotient is stored in EBX:EAX, remainder in EDX. }
  xchg eax, ebx
  xor  edx, edx
  div  ecx
  xchg eax, ebx
  div  ecx

  mov  [esi], eax
  mov  [esi+4], ebx
  mov  eax, edx

  pop  esi
  pop  ebx
end;

function Mod64(const X: Integer64; const Divisor: LongWord): LongWord;
{ Divides X by Divisor and returns the remainder. Unlike Div64, X is left
  intact. }
asm
  push ebx
  push esi
  mov  esi, eax
  mov  ecx, edx

  mov  eax, [esi]
  mov  ebx, [esi+4]

  { Divide EBX:EAX by ECX. Quotient is stored in EBX:EAX, remainder in EDX. }
  xchg eax, ebx
  xor  edx, edx
  div  ecx
  xchg eax, ebx
  div  ecx

  mov  eax, edx

  pop  esi
  pop  ebx
end;

procedure Shr64(var X: Integer64; Count: Cardinal);
{ Unsigned SHR of an Integer64 }
asm
  mov  ecx, edx
  push esi
  mov  esi, eax
  mov  eax, [esi]
  mov  edx, [esi+4]

  cmp  ecx, 32
  jb   @@below32
  cmp  ecx, 64
  jb   @@below64
  xor  edx, edx
  xor  eax, eax
  jmp  @@exit

@@below64:
  mov  eax, edx
  xor  edx, edx
  shr  eax, cl
  jmp  @@exit

@@below32:
  shrd eax, edx, cl
  shr  edx, cl

@@exit:
  mov  [esi], eax
  mov  [esi+4], edx
  pop  esi
end;

function StrToInteger64(const S: String; var X: Integer64): Boolean;
{ Converts a string containing a decimal number into an Integer64. Returns
  True if successful. }
var
  I: Integer;
begin
  X.Lo := 0;
  X.Hi := 0;
  Result := False;
  if S <> '' then begin
    for I := 1 to Length(S) do begin
      if not(S[I] in ['0'..'9']) then
        Exit;
      try
        Comp(X) := (Comp(X) * 10) + (Ord(S[I]) - Ord('0'));
      except
        on EMathError do  { overflow? }
          Exit;
      end;
    end;
    Result := True;
  end;
end;

end.
