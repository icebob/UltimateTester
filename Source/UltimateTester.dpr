program UltimateTester;

uses
  ExceptionLog,
  Forms,
  uMain in 'uMain.pas' {MainForm},
  uModuleEdit in 'uModuleEdit.pas' {ModuleForm},
  uRunner in 'uRunner.pas',
  uCommon in 'uCommon.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Ultimate Tester';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TModuleForm, ModuleForm);
  Application.Run;
end.
