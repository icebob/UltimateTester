program HTTPGetTest;

{$APPTYPE CONSOLE}

uses
  Forms,
  Windows,
  SysUtils,
  uMain in 'uMain.pas' {DM: TDataModule},
  TestModule in '..\TestModule.pas';

begin
  try
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.ShowMainForm := False;
    Application.CreateForm(TDM, DM);
  //Application.Run;
    while not Application.Terminated do
    begin
      Application.ProcessMessages;
      Sleep(100);
    end;
    DM.Free;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

