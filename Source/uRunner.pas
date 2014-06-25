unit uRunner;

interface

uses
  SysUtils, Windows, Classes, IceXML, Generics.Collections,
  JCLSysUtils, SyncObjs, ExtCtrls;

type
  TModuleUnit = class(TObject)
  private
    FRestartTimer: TTimer;
    FRunning: boolean;
    FProgramName: string;
    FInstanceID: integer;
    FParameters: string;
    FOrigParameters: string;
    FStatus: string;
    FConsoleBuffer: AnsiString;
    FRunScore: integer;
    FRunCount: integer;
    FRunStatus: integer;
    FRunLastTime: integer;
    FAddInstanceIDToParameters: boolean;
    FResponseRegexpFilter: string;
    FUseAutoRestart: boolean;
    FAutoRestartTime: string;
    FUseResponseFilter: boolean;
    procedure SetRunning(const Value: boolean);
    procedure SetInstanceID(const Value: integer);
    procedure SetParameters(const Value: string);
    procedure SetProgramName(const Value: string);
    procedure SetOrigParameters(const Value: string);
    procedure SetStatus(const Value: string);
    procedure SetRunCount(const Value: integer);
    procedure SetRunLastTime(const Value: integer);
    procedure SetRunScore(const Value: integer);
    procedure SetRunStatus(const Value: integer);
    procedure SetAddInstanceIDToParameters(const Value: boolean);
    procedure SetAutoRestartTime(const Value: string);
    procedure SetResponseRegexpFilter(const Value: string);
    procedure SetUseAutoRestart(const Value: boolean);
    procedure SetUseResponseFilter(const Value: boolean);
    procedure BeginAutoRestart;
    procedure FRestartTimerTick(Sender: TObject);
    function GetModuleState: string;
    function GetIsWaiting: boolean;
  protected
    FAutoRestartMinTime: integer;
    FAutoRestartMaxTime: integer;

    procedure RunCmd(cmd, parameters: string);
    procedure OutputHandler(const Text: string);
    procedure ProcessConsoleMessages(Data: AnsiString);

    function GetParameters: string;
    procedure InnerStart;
    procedure AddMessage(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    procedure StartWithCmd;

    class function CreateFromXML(Item: TXMLItem): TModuleUnit;
    procedure SaveToXML(Item: TXMLItem);

    property Status: string read FStatus write SetStatus;
    property ModuleState: string read GetModuleState;
    property IsWaiting: boolean read GetIsWaiting;

    property Running: boolean read FRunning write SetRunning;
    property ProgramName: string read FProgramName write SetProgramName;
    property OrigParameters: string read FOrigParameters write SetOrigParameters;
    property Parameters: string read FParameters write SetParameters;
    property InstanceID: integer read FInstanceID write SetInstanceID;
    property AddInstanceIDToParameters: boolean read FAddInstanceIDToParameters write SetAddInstanceIDToParameters;

    property UseResponseFilter: boolean read FUseResponseFilter write SetUseResponseFilter;
    property ResponseRegexpFilter: string read FResponseRegexpFilter write SetResponseRegexpFilter;

    property UseAutoRestart: boolean read FUseAutoRestart write SetUseAutoRestart;
    property AutoRestartTime: string read FAutoRestartTime write SetAutoRestartTime;

    property RunStatus: integer read FRunStatus write SetRunStatus;
    property RunCount: integer read FRunCount write SetRunCount;
    property RunLastTime: integer read FRunLastTime write SetRunLastTime;
    property RunScore: integer read FRunScore write SetRunScore;
  end;

  TOnChanged = procedure (Sender: TObject; Module: TModuleUnit) of object;

  TRunner = class(TObject)
  private
    FLock: TCriticalSection;
    FMessageQueue: TStringList;
    FModules: TObjectList<TModuleUnit>;
    FOnChanged: TOnChanged;
    FProjectFile: string;
    FProjectName: string;
    procedure SetOnChanged(const Value: TOnChanged);
    function GetModuleCount: integer;
    procedure SetProjectFile(const Value: string);
    procedure SetProjectName(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Lock; inline;
    procedure UnLock; inline;

    procedure AddMessage(Module: TModuleUnit; Msg: string);
    function GetLastMessages(): TStringList;

    function AddModule(programName, parameters: string; instanceID: integer; addInstanceToParams: boolean): TModuleUnit;

    procedure StartAll;
    procedure StopAll;

    procedure LoadFromProjectFile;
    procedure SaveToProjectFile;

    procedure DoChanged(Module: TModuleUnit = nil);
    function GetModuleByIndex(Index: integer): TModuleUnit;
    function GetModuleIndex(Module: TModuleUnit): integer;
    procedure DeleteByIndex(Index: integer);
    function GetMaxInstanceID: integer;

    property ProjectName: string read FProjectName write SetProjectName;
    property ProjectFile: string read FProjectFile write SetProjectFile;
    property OnChanged: TOnChanged read FOnChanged write SetOnChanged;
    property ModuleCount: integer read GetModuleCount;
  end;

const
  MODULES_PATH   = 'Modules\';
  PROJECTS_PATH  = 'Projects\';

var
  Runner: TRunner = nil;

implementation

uses
  Forms, Dialogs, Math, ShellAPI, StrUtils, uCommon, RegularExpressions;

{ TModuleUnit }

//------------------------------------------------------------------------------
constructor TModuleUnit.Create;
//------------------------------------------------------------------------------
begin
  FRestartTimer := TTimer.Create(nil);
  with FRestartTimer do
  begin
    Enabled := False;
    OnTimer := FRestartTimerTick;
  end;
  FRunning := false;
  FProgramName := '';
  FInstanceID := 0;
  FOrigParameters := '';
  FParameters := '';
  FStatus := '';
  FConsoleBuffer := '';

  FUseResponseFilter := false;
  FResponseRegexpFilter := '';
  FUseAutoRestart := false;
  FAutoRestartTime := '5';
  FAutoRestartMinTime := 5;
  FAutoRestartMinTime := 5;
end;

//------------------------------------------------------------------------------
class function TModuleUnit.CreateFromXML(Item: TXMLItem): TModuleUnit;
//------------------------------------------------------------------------------
begin
  result                            := TModuleUnit.Create;
  result.ProgramName                := Item.Attr['program'];
  result.InstanceID                 := Item.GetParamAsInt('instance', 0);
  result.Parameters                 := Item.Attr['parameters'];
  result.AddInstanceIDToParameters  := Item.GetParamAsInt('addinstancetoparam', 0) = 1;

  result.UseResponseFilter          := Item.GetParamAsInt('useresponsefilter', 0) = 1;
  result.ResponseRegexpFilter       := Item.Attr['responseregexp'];

  result.UseAutoRestart             := Item.GetParamAsInt('useautorestart', 0) = 1;
  result.AutoRestartTime            := Item.Attr['autorestarttime'];
end;

//------------------------------------------------------------------------------
destructor TModuleUnit.Destroy;
//------------------------------------------------------------------------------
begin
  if Running then
    Stop;

  FreeAndNil(FRestartTimer);

  inherited;
end;

//------------------------------------------------------------------------------
procedure TModuleUnit.SaveToXML(Item: TXMLItem);
//------------------------------------------------------------------------------
begin
  Item.Attr['program']            := FProgramName;
  Item.Attr['parameters']         := FOrigParameters;
  Item.Attr['instance']           := FInstanceID;
  Item.Attr['addinstancetoparam'] := integer(FAddInstanceIDToParameters);

  Item.Attr['useresponsefilter']  := integer(FUseResponseFilter);
  Item.Attr['responseregexp']     := FResponseRegexpFilter;

  Item.Attr['useautorestart']     := integer(FUseAutoRestart);
  Item.Attr['autorestarttime']    := FAutoRestartTime;
end;

procedure TModuleUnit.SetAddInstanceIDToParameters(const Value: boolean);
begin
  FAddInstanceIDToParameters := Value;
end;

procedure TModuleUnit.SetAutoRestartTime(const Value: string);
var
  time: string;
begin
  FAutoRestartTime := Value;

  time := Value;
  if StrPos(time, '-') > 0 then
  begin
    FAutoRestartMinTime := StrToIntDef(CutAt(time, '-'), 10) * 1000;
    FAutoRestartMaxTime := StrToIntDef(time, 0) * 1000;
    Randomize;
  end
  else
    FAutoRestartMinTime := StrToIntDef(time, 10) * 1000;
end;

procedure TModuleUnit.SetInstanceID(const Value: integer);
begin
  FInstanceID := Value;
end;

procedure TModuleUnit.SetOrigParameters(const Value: string);
begin
  FOrigParameters := Value;
end;

//------------------------------------------------------------------------------
procedure TModuleUnit.SetParameters(const Value: string);
//------------------------------------------------------------------------------
begin
  FOrigParameters := Value;
  FParameters := StringReplace(Value, '%INSTANCE%', IntToStr(FInstanceID), [rfReplaceAll, rfIgnoreCase]);

end;

procedure TModuleUnit.SetProgramName(const Value: string);
begin
  FProgramName := Value;
end;

procedure TModuleUnit.SetResponseRegexpFilter(const Value: string);
begin
  FResponseRegexpFilter := Value;
end;

procedure TModuleUnit.SetRunCount(const Value: integer);
begin
  FRunCount := Value;
end;

procedure TModuleUnit.SetRunLastTime(const Value: integer);
begin
  FRunLastTime := Value;
end;

//------------------------------------------------------------------------------
procedure TModuleUnit.SetRunning(const Value: boolean);
//------------------------------------------------------------------------------
begin
  FRunning := Value;

  if Assigned(Runner) then
    Runner.DoChanged(Self);
end;

procedure TModuleUnit.SetRunScore(const Value: integer);
begin
  FRunScore := Value;
end;

procedure TModuleUnit.SetRunStatus(const Value: integer);
begin
  FRunStatus := Value;
end;

//------------------------------------------------------------------------------
procedure TModuleUnit.SetStatus(const Value: string);
//------------------------------------------------------------------------------
begin
  FStatus := Value;
  AddMessage(Value);
end;

//------------------------------------------------------------------------------
procedure TModuleUnit.AddMessage(const Value: string);
//------------------------------------------------------------------------------
begin
  if Assigned(Runner) then
  begin
    Runner.AddMessage(Self, Value);
    Runner.DoChanged(Self);
  end;
end;

procedure TModuleUnit.SetUseAutoRestart(const Value: boolean);
begin
  FUseAutoRestart := Value;
end;

procedure TModuleUnit.SetUseResponseFilter(const Value: boolean);
begin
  FUseResponseFilter := Value;
end;

//------------------------------------------------------------------------------
procedure TModuleUnit.Start;
//------------------------------------------------------------------------------
var
  thr: TThread;

begin
  if Running then Exit;

  Status := 'Starting module...';
  SetRunning(true);

  InnerStart;
end;

//------------------------------------------------------------------------------
procedure TModuleUnit.InnerStart;
//------------------------------------------------------------------------------
var
  thr: TThread;
begin
  Inc(FRunCount);
  thr := TThread.CreateAnonymousThread( procedure()
  begin
    RunCmd(ProgramName, GetParameters);
  end);
  thr.Start;
end;


//------------------------------------------------------------------------------
function TModuleUnit.GetIsWaiting: boolean;
begin
  result := FRestartTimer.Enabled;
end;

function TModuleUnit.GetModuleState: string;
begin
  if Running then
  begin
    if IsWaiting then
      result := 'Waiting'
    else
      result := 'Running';
  end
  else
    result := '';
end;

function TModuleUnit.GetParameters: string;
//------------------------------------------------------------------------------
begin
  result := FParameters;
  if AddInstanceIDToParameters and (FInstanceID > 0) then
    result := result + Format(' --instance %d', [FInstanceID]);
end;

//------------------------------------------------------------------------------
procedure TModuleUnit.StartWithCmd;
//------------------------------------------------------------------------------
begin
  ShellExecute(Application.Handle, nil, 'cmd.exe', PWideChar('/c ' + ExecPath + 'Modules\' + ProgramName + ' ' + GetParameters), nil, SW_SHOWNORMAL);
end;

//------------------------------------------------------------------------------
procedure TModuleUnit.Stop;
//------------------------------------------------------------------------------
begin
  if not Running then Exit;

  FRestartTimer.Enabled := false;
  SetRunning(false);

  AddMessage('Module stopped.');
  Status := '';
end;

//------------------------------------------------------------------------------
procedure TModuleUnit.ProcessConsoleMessages(Data: AnsiString);
//------------------------------------------------------------------------------
var
  Line: AnsiString;
  c: integer;
begin
  FConsoleBuffer := FConsoleBuffer + Data;

  while Pos(#10, FConsoleBuffer) > 0 do
  begin
    Line := Trim(CutAt(FConsoleBuffer, #10));

    if Pos('$$', Line) > 0 then
    begin
      CutAt(Line, '$$ ');
      Line := Trim(Line);
      FRunStatus    := StrToIntDef(CutAt(Line, ' '), 0);

      c := StrToIntDef(CutAt(Line, ' '), -1);
      if c <> -1 then FRunCount     := c;

      FRunLastTime  := StrToIntDef(CutAt(Line, ' '), 0);
      FRunScore     := StrToIntDef(CutAt(Line, ' '), 0);

      if Assigned(Runner) then
        Runner.DoChanged(Self);

    end
    else
      OutputHandler(string(Line));
  end;
end;

//------------------------------------------------------------------------------
procedure TModuleUnit.OutputHandler(const Text: string);
//------------------------------------------------------------------------------
var
  re: TRegEx;
  match: TMatch;
  I: Integer;
begin
  if UseResponseFilter and (ResponseRegexpFilter <> '') then
  begin
    re := TRegEx.Create(ResponseRegexpFilter, [roSingleLine]);
    match := re.Match(Text);

    while match.Success do
    begin
      if match.Groups.Count > 1 then
      begin
        for I := 1 to match.Groups.Count - 1 do
        begin
          Status := match.Groups[I].Value;
        end;
      end
      else
        Status := match.Value;

      match := match.NextMatch;
    end;


  end
  else
    Status := Text;
end;

//------------------------------------------------------------------------------
procedure TModuleUnit.RunCmd(cmd, parameters: string);
//------------------------------------------------------------------------------
const
  BufferSize = 127;
var
  Security            : TSecurityAttributes;
  ReadPipe,WritePipe  : THandle;
  start               : TStartUpInfo;
  ProcessInfo         : TProcessInformation;
  Buffer, B2              : PAnsichar;
  BytesRead           : DWORD;
  Apprunning,
  BytesLeftThisMessage,
  TotalBytesAvail : integer;
  modulePath: string;
  cmdFileName: string;
  exitStartTime: Cardinal;
  ExitCode: Cardinal;
begin
  modulePath := ExecPath + MODULES_PATH;
  exitStartTime := 0;

  with Security do
  begin
    nlength              := SizeOf(TSecurityAttributes);
    binherithandle       := true;
    lpsecuritydescriptor := nil;
  end;

  if CreatePipe (ReadPipe, WritePipe, @Security, 0) then
  begin
    try
      // Redirect In- and Output through STARTUPINFO structure

      FillChar(Start,Sizeof(Start),#0);
      start.cb          := SizeOf(start);
      start.hStdInput   := GetStdHandle(STD_INPUT_HANDLE);
      //start.hStdInput   := ReadPipe;
      start.hStdOutput  := WritePipe;
      start.hStdError   := WritePipe;
      start.dwFlags     := STARTF_USESTDHANDLES + STARTF_USESHOWWINDOW;
      start.wShowWindow := SW_HIDE;

      if Pos(':', cmd) = 2 then
        cmdFileName := cmd
      else
        cmdFileName := IncludeTrailingBackslash(modulePath) + cmd;

      // Create a Console Child Process with redirected input and output
      if CreateProcess(nil      ,PWideChar(cmdFileName + ' ' + parameters),
                       @Security,@Security,
                       true     ,CREATE_NO_WINDOW or NORMAL_PRIORITY_CLASS,
                       nil      ,PWideChar(modulePath),
                       start    ,ProcessInfo) then
      begin
        try
          Buffer  := AllocMem(BufferSize + 1);

          repeat
            if PeekNamedPipe(ReadPipe, @Buffer[0], BufferSize, @BytesRead, @TotalBytesAvail, @BytesLeftThisMessage) then
            begin
              if BytesRead > 0 then
                ReadFile(ReadPipe,Buffer[0],BytesRead,BytesRead,nil);

              if BytesRead > 0 then
              begin
                Buffer[BytesRead]:= #0;
                OemToCharA(Buffer, Buffer);
                ProcessConsoleMessages(Buffer);
              end;
            end;

            Application.ProcessMessages;

            // wait for end of child process
            Apprunning := WaitForSingleObject(ProcessInfo.hProcess,100);

            {GetExitCodeProcess(pi.hProcess, ExitCode);
                if ExitCode<>STILL_ACTIVE then
                  Break;         }

            if Apprunning <> WAIT_TIMEOUT then
            begin
              // nem lépünk ki egybõl, mert akkor nem látjuk az utolsó üzeneteket (curl, node, phantomjs...etc)

              if exitStartTime = 0 then
                exitStartTime := GetTickCount()
              else
                // 2 másodpercet várunk, miután kilépett az app. Eddig még kiolvassuk a pipe-ból a messageeket, aztán leállítjuk a szálat.
                if (GetTickCount - exitStartTime) > 2 * 1000 then
                begin
                  GetExitCodeProcess(ProcessInfo.hProcess, ExitCode);
                  AddMessage(Format('---- Application exited (code: %d)', [ExitCode]));

                  if UseAutoRestart then
                  begin
                    BeginAutoRestart;
                    break;
                  end;

                  Stop;
                end;

            end;

          until (not Running);

        finally
          FreeMem(Buffer);

          TerminateProcess(ProcessInfo.hProcess, 0);
          CloseHandle(ProcessInfo.hProcess);
          CloseHandle(ProcessInfo.hThread);
        end;
      end
      else
      begin
        Stop;
        SetStatus('ERROR - ' + SysErrorMessage(GetLastError()));
      end;

    finally
      CloseHandle(ReadPipe);
      CloseHandle(WritePipe);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TModuleUnit.BeginAutoRestart;
//------------------------------------------------------------------------------
begin
  with FRestartTimer do
  begin
    if FAutoRestartMaxTime = 0 then
      Interval := FAutoRestartMinTime
    else
      Interval := FAutoRestartMinTime + Random(FAutoRestartMaxTime - FAutoRestartMinTime);

    Enabled := true;
    AddMessage(Format('Waiting %d ms for restart...', [Interval]));
  end;
end;

//------------------------------------------------------------------------------
procedure TModuleUnit.FRestartTimerTick(Sender: TObject);
//------------------------------------------------------------------------------
begin
  FRestartTimer.Enabled := false;
  AddMessage('Restarting module...');
  InnerStart;
end;

{ TRunner }

//------------------------------------------------------------------------------
procedure TRunner.AddMessage(Module: TModuleUnit; Msg: string);
//------------------------------------------------------------------------------
var
  Str: string;
begin
  Lock;
  try
    if Assigned(Module) then
      Str := Format('[%s:%.3d] %s', [ExtractFileName(Module.ProgramName), Module.InstanceID, Msg])
    else
      Str := Msg;

    FMessageQueue.Add(Str);
  finally
    UnLock;
  end;
end;

//------------------------------------------------------------------------------
function TRunner.AddModule(programName, parameters: string;
  instanceID: integer; addInstanceToParams: boolean): TModuleUnit;
//------------------------------------------------------------------------------
begin
  result := TModuleUnit.Create;
  result.ProgramName := programName;
  result.InstanceId := instanceID;
  result.AddInstanceIDToParameters := addInstanceToParams;
  result.Parameters := parameters;

  FModules.Add(result);
end;

//------------------------------------------------------------------------------
constructor TRunner.Create;
//------------------------------------------------------------------------------
begin
  FLock := TCriticalSection.Create;

  FMessageQueue := TStringList.Create;

  FModules := TObjectList<TModuleUnit>.Create;
end;

//------------------------------------------------------------------------------
procedure TRunner.DeleteByIndex(Index: integer);
//------------------------------------------------------------------------------
var
  module: TModuleUnit;
begin
  module := GetModuleByIndex(Index);
  if Assigned(module) then
    FModules.Remove(module);

  SaveToProjectFile;
end;

destructor TRunner.Destroy;
begin
  StopAll;

  FreeAndNil(FModules);

  FreeAndNil(FMessageQueue);

  FreeAndNil(FLock);

  inherited;
end;

//------------------------------------------------------------------------------
procedure TRunner.DoChanged(Module: TModuleUnit = nil);
//------------------------------------------------------------------------------
begin
  if Assigned(OnChanged) then
    OnChanged(Self, Module);
end;

//------------------------------------------------------------------------------
function TRunner.GetLastMessages: TStringList;
//------------------------------------------------------------------------------
begin
  result := TStringList.Create;
  Lock;
  try
    result.AddStrings(FMessageQueue);
    FMessageQueue.Clear;
  finally
    UnLock;
  end;
end;

//------------------------------------------------------------------------------
function TRunner.GetMaxInstanceID: integer;
//------------------------------------------------------------------------------
var
  module: TModuleUnit;
begin
  result := 0;
  for module in FModules do
    result := Max(module.InstanceID, result);
end;

//------------------------------------------------------------------------------
function TRunner.GetModuleByIndex(Index: integer): TModuleUnit;
//------------------------------------------------------------------------------
begin
  if (Index >= 0) and (Index < FModules.Count) then
    result := FModules[Index]
  else
    result := nil;
end;

//------------------------------------------------------------------------------
function TRunner.GetModuleCount: integer;
//------------------------------------------------------------------------------
begin
  result := FModules.Count;
end;

//------------------------------------------------------------------------------
function TRunner.GetModuleIndex(Module: TModuleUnit): integer;
//------------------------------------------------------------------------------
var
  I: Integer;
begin
  result := -1;
  for I := 0 to FModules.Count - 1 do
    if Module = FModules[I] then
      Exit(I);
end;

//------------------------------------------------------------------------------
procedure TRunner.LoadFromProjectFile;
//------------------------------------------------------------------------------
var
  xml: TIceXML;
  Settings: TXMLItem;
  I: Integer;
  module: TModuleUnit;
begin
  FModules.Clear;

  xml := TIceXML.Create(nil);
  try
    xml.EncodeType := 'UTF-8';
    xml.FileName := FProjectFile;

    if FileExists(xml.Filename) then
    begin
      xml.LoadFromFile;
      ProjectName := xml.Root.GetParamValue('name', 'Unnamed');
      Settings := xml.Root.GetItemEx('Modules', true);
      for I := 0 to Settings.Count - 1 do
      begin
        module := TModuleUnit.CreateFromXML(Settings[I]);

        if Assigned(module) then
          FModules.Add(module);
      end;

    end;
  finally
    xml.Free;
  end;
  DoChanged();
end;

//------------------------------------------------------------------------------
procedure TRunner.SaveToProjectFile;
//------------------------------------------------------------------------------
var
  I: Integer;
  module: TModuleUnit;
  xml: TIceXML;
  Settings: TXMLItem;
begin
  xml := TIceXML.Create(nil);
  try
    xml.EncodeType := 'UTF-8';
    xml.FileName := FProjectFile;

    if xml.FileName = '' then
      xml.FileName := ExecPath + PROJECTS_PATH + 'Default.utproject';

    if FileExists(xml.FileName) then
      xml.LoadFromFile
    else
      xml.Root.Name := 'UltimateTesterProject';

    xml.Root.SetParamValue('name', ProjectName);

    Settings := xml.Root.GetItemEx('Modules', true);
    Settings.ClearChildrens;

    for module in FModules do
      module.SaveToXML(Settings.New('Module'));

    xml.SaveToFile;
  finally
    xml.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TRunner.SetOnChanged(const Value: TOnChanged);
//------------------------------------------------------------------------------
begin
  FOnChanged := Value;
end;

//------------------------------------------------------------------------------
procedure TRunner.SetProjectFile(const Value: string);
//------------------------------------------------------------------------------
begin
  FProjectFile := Value;
  FProjectName := ChangeFileExt(ExtractFileName(FProjectFile), '');
end;

procedure TRunner.SetProjectName(const Value: string);
begin
  FProjectName := Value;
end;

//------------------------------------------------------------------------------
procedure TRunner.StartAll;
//------------------------------------------------------------------------------
var
  module: TModuleUnit;
begin
  for module in FModules do
    if not module.Running then
      module.Start;
end;

//------------------------------------------------------------------------------
procedure TRunner.StopAll;
//------------------------------------------------------------------------------
var
  module: TModuleUnit;
begin
  for module in FModules do
    if module.Running then
      module.Stop;
end;

procedure TRunner.Lock;
begin
  FLock.Enter;
end;

procedure TRunner.UnLock;
begin
  FLock.Leave;
end;

end.
