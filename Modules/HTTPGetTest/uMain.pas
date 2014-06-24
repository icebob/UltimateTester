unit uMain;

interface

uses
  SysUtils, Classes, IcePack, TestModule, ExtCtrls, IdHTTP;

type
  THttpGetTest = class(TTestModule)
  private
    FURL: string;
    FShowResponse: boolean;
    FTimer: TTimer;
    FBaseTime: integer;
    FMaxTime: integer;
    function GetNextInterval: integer;
  protected
    procedure DoHTTPGet;
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
  HttpGetTest: THttpGetTest;
  Counter: integer = 1;

implementation

uses
  Performance;

{$R *.dfm}

procedure TDM.DataModuleCreate(Sender: TObject);
begin
  HttpGetTest := THttpGetTest.Create;
  HttpGetTest.Start;
end;

procedure TDM.DataModuleDestroy(Sender: TObject);
begin
  HttpGetTest.Stop;
  FreeAndNil(HttpGetTest);
end;

{ THttpGetTest }

constructor THttpGetTest.Create;
var
  time: string;
begin
  inherited;

  ModuleName := 'HTTP GET';

  FURL  := GetOptionValue('url', 'url', 'http://127.0.0.1/');
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

destructor THttpGetTest.Destroy;
begin
  FreeAndNil(FTimer);

  inherited;
end;

function THttpGetTest.GetNextInterval: integer;
begin
  if FMaxTime = 0 then
    result := FBaseTime
  else
    result := FBaseTime + Random(FMaxTime - FBaseTime);
end;

procedure THttpGetTest.FTimerTick(Sender: TObject);
begin
  FTimer.Enabled := false;
  try
    DoHTTPGet;

    Inc(Counter);
  finally
    FTimer.Interval := GetNextInterval;
    FTimer.Enabled := true;
  end;
end;

procedure THttpGetTest.DoHTTPGet;
var
  http: TIdHTTP;
  response: string;
  perf: TPerformance;
begin
  http := TIdHTTP.Create(nil);
  perf := TPerformance.Create(nil);
  try
    http.HandleRedirects := true;
    http.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/35.0.1916.153 Safari/537.36';
    try
      Log('GET %s...', [FURL]);
      perf.Start;

      response := http.Get(FURL);

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
    perf.Free;
    http.Free;
  end;
end;


procedure THttpGetTest.Start;
begin
  inherited;

  FTimer.Interval := GetNextInterval;
  FTimer.Enabled  := true;
  FTimerTick(FTimer);
end;

procedure THttpGetTest.Stop;
begin
  inherited;

  FTimer.Enabled := false;
end;

end.
