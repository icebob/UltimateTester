unit uMain;

interface

uses
  SysUtils, Classes, IcePack, TestModule, ExtCtrls, IdHTTP;

type
  THttpPostTest = class(TTestModule)
  private
    FURL: string;
    FData: string;
    FDataFile: string;
    FShowResponse: boolean;
    FTimer: TTimer;
    FBaseTime: integer;
    FMaxTime: integer;
    function GetNextInterval: integer;
  protected
    procedure DoHTTPPost;
    procedure FTimerTick(Sender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Start; override;
    procedure Stop; override;
  end;


  TDM = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DM: TDM;
  HttpPostTest: THttpPostTest;
  Counter: integer = 1;

implementation

uses
  Performance;

{$R *.dfm}

procedure TDM.DataModuleCreate(Sender: TObject);
begin
  HttpPostTest := THttpPostTest.Create;
  HttpPostTest.Start;
end;

procedure TDM.DataModuleDestroy(Sender: TObject);
begin
  HttpPostTest.Stop;
  FreeAndNil(HttpPostTest);
end;

{ THttpPostTest }

constructor THttpPostTest.Create;
var
  time: string;
begin
  inherited;

  ModuleName := 'HTTP Post';

  FURL          := GetOptionValue('url', 'url', 'http://127.0.0.1/');
  FDataFile     := GetOptionValue('f', 'file', '');
  FData         := GetOptionValue('d', 'data', '');
  FShowResponse := HasOption('showresponse');

  time := GetOptionValue('t', 'time', '');
  if StrPos(time, '-') > 0 then
  begin
    FBaseTime := StrToIntDef(CutAt(time, '-'), 10) * 1000;
    FMaxTime := StrToIntDef(time, 0) * 1000;
    Randomize;
  end
  else
    FBaseTime := StrToIntDef(time, 10) * 1000;

  FTimer := TTimer.Create(nil);
  with FTimer do
  begin
    Enabled   := false;
    OnTimer   := FTimerTick;
  end;
end;

destructor THttpPostTest.Destroy;
begin
  FreeAndNil(FTimer);

  inherited;
end;

function THttpPostTest.GetNextInterval: integer;
begin
  if FMaxTime = 0 then
    result := FBaseTime
  else
    result := FBaseTime + Random(FMaxTime - FBaseTime);
end;

procedure THttpPostTest.FTimerTick(Sender: TObject);
begin
  FTimer.Enabled := false;
  try
    DoHTTPPost;

    Inc(Counter);
  finally
    FTimer.Interval := GetNextInterval;
    FTimer.Enabled := true;
  end;
end;

procedure THttpPostTest.DoHTTPPost;
var
  http: TIdHTTP;
  response: string;
  FList: TStrings;
  perf: TPerformance;
begin
  FList := TStringList.Create;
  http := TIdHTTP.Create(nil);
  perf := TPerformance.Create(nil);
  try
    http.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/35.0.1916.153 Safari/537.36';
    try
      Log('POST to %s...', [FURL]);
      if FileExists(FDataFile) then
        response := http.Post(FURL, FDataFile)
      else
      begin
        FList.Text := FData;
        response := http.Post(FURL, FList);
      end;
      perf.Start;

      if FShowResponse then
        Log('Response: ' + response);


      Log('OK. Received: %15s. Time: %s', [ByteToString(Length(response)), perf.Stop]);
      LogResult(RES_OK, Counter, Round(perf.Duration * 1000), 10);

    except on E: Exception do
      begin
        Log('ERROR! ' + E.Message);
        LogResult(RES_ERROR, Counter, Round(perf.Duration * 1000), 1);
      end;
    end;
  finally
    http.Free;
    FList.Free;
  end;
end;


procedure THttpPostTest.Start;
begin
  inherited;

  FTimer.Interval := GetNextInterval;
  FTimer.Enabled := true;
  FTimerTick(FTimer);
end;

procedure THttpPostTest.Stop;
begin
  inherited;

  FTimer.Enabled := false;
end;

end.
