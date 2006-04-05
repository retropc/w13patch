unit Tokeniser;

interface

uses
  Classes;

type
  TTokenList = class(TStringList)
  public
    function Tokenise(const Data: string; const Delimiter: char; const Max: integer = -1): integer;
    function Recombine(const Delimiter: char; const Start: integer = 0) : string;
  end;


implementation

function TTokenList.Tokenise(const Data: string; const Delimiter: char; const Max: integer = -1): integer;
var P, L, LD:integer;
    S: boolean;
    TV: string;
begin
  Result := -1;
  S := false;
  L := 1;
  LD := length(Data);
  Clear;
  for P:=1 to LD do
  begin
    if S then
    begin
      if Data[P] <> Delimiter then
      begin
        L := P;
        S := false;
        if ((Max <> -1) and (Count >= Max)) then
        begin
          Result := P;
          break;
        end;
      end
      else
        L := P + 1;
    end
    else
      if Data[P] = Delimiter then
      begin
        if P-L > 0 then
          Add(Copy(Data, L, P-L));
        L := P + 1;
        S := true;
      end;
  end;
  TV := Copy(Data, L, LD-L + 1);
  if TV <> '' then
    Add(TV);
end;

function TTokenList.Recombine(const Delimiter: char; const Start: integer = 0) : string;
var I, Max: integer;
begin
  Result := '';
  Max := Count - 1;
  for I := Start to Max do
    if I < Max then
      Result := Result + Strings[I] + ' '
    else
      Result := Result + Strings[I];
end;

end.
