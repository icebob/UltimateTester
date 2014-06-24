unit TestModule;

interface

uses
  SysUtils, Windows, Classes, Forms, Generics.Collections;

type
  TTestModule = class(TObject)
  private
    FModuleName: string;
    FInstanceID: integer;
    procedure ProcessCommandLineParameters;
    procedure SetInstanceID(const Value: integer);
  protected
    FCommandLineParameters: TDictionary<string, string>;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;

    function HasOption(key: String): boolean; overload;
    function HasOption(shortKey, longKey: String): boolean; overload;
    function GetOptionValue(key: String; defaultValue: string = ''): string; overload;
    function GetOptionValue(shortKey, longKey: String; defaultValue: string = ''): string; overload;

  published
    property ModuleName: string read FModuleName write FModuleName;
    property InstanceID: integer read FInstanceID write SetInstanceID;
  end;

var
  SelfModule: TTestModule = nil;
  ConsoleMode: boolean = false;

const
  RES_OK = 1;
  RES_ERROR = -1;

//function AttachConsole(dwProcessID: Integer): Boolean; stdcall; external 'kernel32.dll';
//function FreeConsole(): Boolean; stdcall; external 'kernel32.dll';

procedure Log(Msg: string); overload;
procedure Log(Msg: string; Args: array of const); overload;

procedure LogResult(res: integer; count: integer; time: integer; score: integer);

function StrPos(const Str, SubStr: string; const bCaseSensitive: boolean = true): integer;

implementation

procedure Log(Msg: string); overload;
var
  s: string;
begin
  s := Format('%s - %s', [FormatDateTime('hh:nn:ss.zzz', Now), Msg]);
  OutputDebugString(PWideChar(s));
  if ConsoleMode then
  begin
    WriteLn(s);
    Flush(Output);
  end;
end;

procedure Log(Msg: string; Args: array of const); overload;
begin
  Log(Format(Msg, Args));
end;

procedure LogResult(res: integer; count: integer; time: integer; score: integer);
begin
  Log('$$ %d %d %d %d', [res, count, time, score]);
end;

function StrPos(const Str, SubStr: string; const bCaseSensitive: boolean = true): integer;
begin
  if bCaseSensitive then
    result := Pos(LowerCase(SubStr), LowerCase(Str))
  else
    result := Pos(SubStr, Str);
end;

{ TTestModule }

constructor TTestModule.Create;
var
  I: Integer;
  key: string;
begin
  FModuleName := 'Base';
  FInstanceID := -1;
  ConsoleMode := true; //AttachConsole(-1);

  FCommandLineParameters := TDictionary<string, string>.Create;

  Log('');
  Log('Create %s test module...', [FModuleName]);

  ProcessCommandLineParameters();
  Log('Instance ID: %d', [FInstanceID]);

  Log('Parameters:');
  Log('-----------');
  for key in FCommandLineParameters.Keys do
  begin
    Log('  %s=%s', [key, FCommandLineParameters[key]]);
  end;
  Log('');
end;

destructor TTestModule.Destroy;
begin
  Log('Destroy test module...');




  if ConsoleMode then
  begin
    ConsoleMode := false;
    //FreeConsole();
  end;

  inherited;
end;

function TTestModule.GetOptionValue(shortKey, longKey: String; defaultValue: string = ''): string;
begin
  if not FCommandLineParameters.TryGetValue(shortKey, result) then
    if not FCommandLineParameters.TryGetValue(longKey, result) then
      result := defaultValue;
end;

function TTestModule.GetOptionValue(key: String; defaultValue: string = ''): string;
begin
  if not FCommandLineParameters.TryGetValue(key, result) then
    result := defaultValue;
end;

function TTestModule.HasOption(key: String): boolean;
begin
  result := FCommandLineParameters.ContainsKey(LowerCase(key));
end;

function TTestModule.HasOption(shortKey, longKey: String): boolean;
begin
  result := FCommandLineParameters.ContainsKey(LowerCase(shortKey));
  if not result then
    result := FCommandLineParameters.ContainsKey(LowerCase(longKey));
end;

procedure TTestModule.ProcessCommandLineParameters;
var
  I: integer;
  s: string;
  pName, pValue: string;
begin
  FCommandLineParameters.Clear;

  I := 1;
  while I <= ParamCount do
  begin
    s := ParamStr(I);
    if Length(s) > 1 then
    begin
      if s[1] in ['-', '/'] then
      begin
        if (Length(s) > 2) and (s[2] = '-') then  // extra -- levágása
          pName := Copy(s, 3, Length(s))
        else
          pName := Copy(s, 2, Length(s));

        pValue := '';

        // Érték ellenõrzés
        if I < ParamCount then
        begin
          s := ParamStr(I + 1);
          if not ((Length(s) > 0) and (s[1] in ['-', '/'])) then
          begin
            pValue := s;
            Inc(I);
          end;
        end;

        if LowerCase(pName) = 'instance' then
          FInstanceID := StrToIntDef(pValue, FInstanceID)
        else
          FCommandLineParameters.Add(LowerCase(pName), pValue);

      end;
    end;
    Inc(I);
  end;
end;


procedure TTestModule.SetInstanceID(const Value: integer);
begin
  FInstanceID := Value;
end;

end.
