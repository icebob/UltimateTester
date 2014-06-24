unit uCommon;

interface

uses
  Windows, SysUtils, Forms, Classes;


var
  ExecPath:String;

function CutAt(var xStr: string; xSub: string; const IgnoreCase: boolean = false): string; overload;
function CutAt(var xStr: AnsiString; xSub: AnsiString; const IgnoreCase: boolean = false): AnsiString; overload;

function GetFiles(path, mask: string; SubDirs: Boolean): TStrings;

implementation

function CutAt(var xStr: string; xSub: string; const IgnoreCase: boolean = false): string;
var
  p: integer;
begin
  if IgnoreCase then
    p := Pos(LowerCase(xSub), LowerCase(xStr))
  else
    p := Pos(xSub, xStr);
  if p = 0 then
    p := Length(xStr) + 1;
  Result := Copy(xStr, 1, p - 1);
  system.Delete(xStr, 1, Length(Result + xSub));
end;

function CutAt(var xStr: AnsiString; xSub: AnsiString; const IgnoreCase: boolean = false): AnsiString;
var
  p: integer;
begin
  if IgnoreCase then
    p := Pos(AnsiLowerCase(xSub), AnsiLowerCase(xStr))
  else
    p := Pos(xSub, xStr);
  if p = 0 then
    p := Length(xStr) + 1;
  Result := Copy(xStr, 1, p - 1);
  system.Delete(xStr, 1, Length(Result + xSub));
end;

function WindowsDirFixup(APath: string): string;
var
  S: string;

  function ReplaceStr(const S, Srch, Replace: string): string;
  var
    I: integer;
    Source: string;
  begin
    Source := S;
    Result := '';
    repeat
      I := pos(Srch, Source);
      if I > 0 then
      begin
        Result := Result + Copy(Source, 1, I - 1) + Replace;
        Source := Copy(Source, I + Length(Srch), MaxInt);
      end
      else
        Result := Result + Source;
    until I <= 0;
  end;

begin
  S := ReplaceStr(APath, '/', '\');
  if Pos('\\',S)<>1 then
    S := ReplaceStr(S, '\\', '\');
  if S[Length(S)] <> '\' then
    S := S + '\';
  Result := S;
end;

function GetFiles(path, mask: string; SubDirs: Boolean): TStrings;

  procedure FindRecursive(ActDir, Searched: string; SList: TStrings);
  var
    done: integer;
    S, findrec: TSearchRec;
    d: string;
    fullpath: string;
  begin
    try
      done := 0;
      chdir(ActDir);
      FindFirst(Searched, faAnyFile + faHidden, findrec);
      repeat
        GetDir(0, d);
        if Length(findrec.name) <> 0 then
        begin
          fullpath := WindowsDirFixup(d) + findrec.name;
          if {(pos('\.', fullpath) = 0) and }(findrec.attr <> faDirectory) then
            SList.Add(fullpath);
        end;
      until FindNext(findrec) <> 0;
      FindClose(findrec);
      FindFirst('*', faDirectory + faHidden, S);
      if (S.name = '.') then
        done := FindNext(S);
      if (S.name = '..') then
        done := FindNext(S);
      while done = 0 do
      begin
//      if (SubDirs) and ((S.attr = 8208) or (S.attr = 48) or (S.attr = 16) or (S.attr = 17)) then

        if DirectoryExists(d + '\' + S.name) and (SubDirs) then
          FindRecursive(S.name, Searched, SList);
        done := FindNext(S);
      end;
      chdir('..');
    except
    end;
    FindClose(S);
  end;

begin
  Result := Tstringlist.Create;
  FindRecursive(path, mask, Result);
end;


initialization
  ExecPath := ExtractFilePath(Application.exename);

end.
