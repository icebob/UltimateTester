unit uModuleEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls, Buttons, Grids, ValEdit, StrUtils;

type
  TModuleForm = class(TForm)
    eParameters: TLabeledEdit;
    eInstanceNumber: TLabeledEdit;
    bOK: TButton;
    bCancel: TButton;
    eProgramName: TComboBox;
    Label2: TLabel;
    Bevel1: TBevel;
    bSearch: TSpeedButton;
    OD: TOpenDialog;
    eInstanceCount: TLabeledEdit;
    GroupBox1: TGroupBox;
    ParameterList: TValueListEditor;
    cbAddInstanceID: TCheckBox;
    procedure bOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bSearchClick(Sender: TObject);
    procedure eParametersChange(Sender: TObject);
    procedure ParameterListStringsChange(Sender: TObject);
  private
    FMultiEdit: boolean;
    FEditMode: boolean;
    procedure SetEditMode(const Value: boolean);
    procedure SetMultiEdit(const Value: boolean);
    procedure LoadParameterList(Params: string);
    function GetParametersFromParamList: string;
    { Private declarations }
  public
    { Public declarations }
    property EditMode: boolean read FEditMode write SetEditMode;
    property MultiEdit: boolean read FMultiEdit write SetMultiEdit;

    procedure LoadModules;
  end;

var
  ModuleForm: TModuleForm;

implementation

uses
  uRunner, uCommon;

{$R *.dfm}

procedure TModuleForm.bOKClick(Sender: TObject);
begin
  if eProgramName.Text = '' then
  begin
    MessageDlg('Please enter program name!', mtError, [mbOK], 0);

    ModalResult := mrNone;
    Exit;
  end;
end;

procedure TModuleForm.bSearchClick(Sender: TObject);
begin
  OD.InitialDir := ExecPath + MODULES_PATH;
  if OD.Execute then
  begin
    if ExtractFilePath(OD.FileName) = ExecPath + MODULES_PATH then
      eProgramName.Text := ExtractFileName(OD.FileName)
    else
      eProgramName.Text := OD.FileName;
  end;
end;

procedure TModuleForm.eParametersChange(Sender: TObject);
begin
  LoadParameterList(eParameters.Text);
end;

procedure TModuleForm.LoadParameterList(Params: string);
var
  p: TStrings;
  s: string;
  I: integer;
  pName, pValue: string;
begin
  ParameterList.OnStringsChange := nil;
  ParameterList.Strings.BeginUpdate;
  try
    ParameterList.Strings.Clear;

    p := TStringList.Create;
    try
      ExtractStrings([' '],[], PWideChar(Params), p);

      I := 0;
      while I < P.Count do
      begin
        s := P[I];
        if Length(s) > 1 then
        begin
          if s[1] in ['-', '/'] then
          begin
            pName := s;
            pValue := '';

            // Validating
            if I < P.Count - 1 then
            begin
              s := P[I + 1];
              if not ((Length(s) > 0) and (s[1] in ['-', '/'])) then
              begin
                pValue := s;
                Inc(I);
              end;
            end;

            ParameterList.Values[pName] := pValue;
          end
          else
            ParameterList.Values[s] := '';
        end
        else
          ParameterList.Values[s] := '';

        Inc(I);
      end;

    finally
      p.Free;
    end;
  finally
    ParameterList.Strings.EndUpdate;
    ParameterList.Refresh;
    ParameterList.OnStringsChange := ParameterListStringsChange;
  end;
end;

function TModuleForm.GetParametersFromParamList(): string;
var
  I: Integer;
begin
  result := '';

  for I := 0 to ParameterList.Strings.Count - 1 do
  begin
    if ParameterList.Strings.Names[I] = '' then continue;

    if Length(Result) > 0 then result := result + ' ';

    result := result + ParameterList.Strings.Names[I] + ' ' + ParameterList.Strings.ValueFromIndex[I];
  end;
end;

procedure TModuleForm.ParameterListStringsChange(Sender: TObject);
begin
  eParameters.OnChange := nil;
  try
    eParameters.Text := GetParametersFromParamList();
  finally
    eParameters.OnChange := eParametersChange;
  end;
end;

procedure TModuleForm.FormCreate(Sender: TObject);
begin
  EditMode := false;
  MultiEdit := false;
  LoadModules;
end;

procedure TModuleForm.FormShow(Sender: TObject);
begin
  eInstanceCount.Enabled := not EditMode;
  eInstanceNumber.Enabled := not MultiEdit;
end;

procedure TModuleForm.LoadModules;
var
  Files: TStrings;
  I: Integer;
begin
  eProgramName.Items.Clear;
  Files := GetFiles(ExecPath + MODULES_PATH, '*.exe', false);
  for I := 0 to Files.Count - 1 do
  begin
    eProgramName.Items.Add(ExtractFileName(Files[I]));
  end;
end;

procedure TModuleForm.SetEditMode(const Value: boolean);
begin
  FEditMode := Value;
end;

procedure TModuleForm.SetMultiEdit(const Value: boolean);
begin
  FMultiEdit := Value;
end;

end.
