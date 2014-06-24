unit uMain;

(*
  Icons: https://www.iconfinder.com/iconsets/snipicons
*)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, ExtCtrls, StdCtrls, uRunner, IceXML, Menus, uCommon,
  ImgList, PngImageList, ActnList, ActnMan, ActnCtrls, ComCtrls, ToolWin,
  StdStyleActnCtrls;

type
  TMainForm = class(TForm)
    Panel3: TPanel;
    VList: TVirtualStringTree;
    Settings: TIceXML;
    pmVList: TPopupMenu;
    Start1: TMenuItem;
    Stop1: TMenuItem;
    N1: TMenuItem;
    Edit1: TMenuItem;
    Delete1: TMenuItem;
    statusImages: TPngImageList;
    ExecuteinCMD1: TMenuItem;
    aAddModules: TAction;
    aStart: TAction;
    aStop: TAction;
    aStartAll: TAction;
    aStopAll: TAction;
    aExecCmd: TAction;
    aEdit: TAction;
    aDelete: TAction;
    ActionManager1: TActionManager;
    ilToolbar: TPngImageList;
    ilToolbarLarge: TPngImageList;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    MessageTimer: TTimer;
    BottomPanel: TGroupBox;
    mLog: TMemo;
    Splitter1: TSplitter;
    pmMessages: TPopupMenu;
    aAutoScroll: TAction;
    Autoscroll1: TMenuItem;
    aLoadProject: TAction;
    aSaveProject: TAction;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    OD: TOpenDialog;
    SD: TSaveDialog;
    aMessageClear: TAction;
    Clear1: TMenuItem;
    ToolButton10: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure VListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VListDblClick(Sender: TObject);
    procedure VListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure VListGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure aAddModulesExecute(Sender: TObject);
    procedure aStartAllExecute(Sender: TObject);
    procedure aStopAllExecute(Sender: TObject);
    procedure aStartExecute(Sender: TObject);
    procedure aStopExecute(Sender: TObject);
    procedure aEditExecute(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure aExecCmdExecute(Sender: TObject);
    procedure MessageTimerTimer(Sender: TObject);
    procedure Autoscroll1Click(Sender: TObject);
    procedure aAutoScrollExecute(Sender: TObject);
    procedure aLoadProjectExecute(Sender: TObject);
    procedure aSaveProjectExecute(Sender: TObject);
    procedure aMessageClearExecute(Sender: TObject);
    procedure VListGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
  private
    FMessagesAutoScroll: boolean;
    procedure LoadSettings;
    procedure SaveSettings;
    { Private declarations }
  public
    { Public declarations }
    property MessagesAutoScroll: boolean read FMessagesAutoScroll write FMessagesAutoScroll;
    procedure RunnerChanged(Sender: TObject; Module: TModuleUnit);
  end;

var
  MainForm: TMainForm;

implementation

uses uModuleEdit, Generics.Collections, Math, StrUtils;

{$R *.dfm}

//------------------------------------------------------------------------------
procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
//------------------------------------------------------------------------------
begin
  MessageTimer.Enabled := False;

  Runner.OnChanged := nil;

  SaveSettings;

  FreeAndNil(Runner);
end;

//------------------------------------------------------------------------------
procedure TMainForm.FormCreate(Sender: TObject);
//------------------------------------------------------------------------------
begin
  ForceDirectories(ExecPath + MODULES_PATH);
  ForceDirectories(ExecPath + PROJECTS_PATH);

  Runner := TRunner.Create;
  Runner.OnChanged := RunnerChanged;

  FMessagesAutoScroll   := true;
  MessageTimer.Enabled  := true;

  LoadSettings;
end;

//------------------------------------------------------------------------------
procedure TMainForm.LoadSettings;
//------------------------------------------------------------------------------
var
  I: integer;
  Item: TXMLItem;
  prjFile: string;
begin
  Settings.Root.Name := 'UltimateTester';
  Settings.FileName := ExecPath + 'Configuration.xml';

  if FileExists(Settings.FileName) then
  begin
    Settings.LoadFromFile;
  end
  else
    Settings.SaveToFile;


  Self.Left := StrToIntDef(Settings.Root.GetItemParamValue('Main.Window', 'Left'), Left);
  Self.Top := StrToIntDef(Settings.Root.GetItemParamValue('Main.Window', 'Top'), Top);
  Self.Width := StrToIntDef(Settings.Root.GetItemParamValue('Main.Window', 'Width'), Width);
  Self.Height := StrToIntDef(Settings.Root.GetItemParamValue('Main.Window', 'Height'), Height);

  if Settings.Root.GetItemParamValue('Main.Window', 'State', 'Normal') = 'Maximized' then
    Self.WindowState := wsMaximized
  else
    Self.WindowState := wsNormal;

  Item := Settings.Root.GetItemEx('Main.VList', false);
  if Assigned(Item) then
    for I := 0 to VList.Header.Columns.Count - 1 do
    begin
      VList.Header.Columns[I].Position := StrToIntDef(Item.GetItemParamValue('Columns.'+IntToStr(I), 'Position'), VList.Header.Columns[I].Position);
      VList.Header.Columns[I].Width := StrToIntDef(Item.GetItemParamValue('Columns.'+IntToStr(I), 'Width'), VList.Header.Columns[I].Width);
    end;

  BottomPanel.Height := StrToIntDef(Settings.Root.GetItemParamValue('Main.Panels', 'BottomPanel'), BottomPanel.Height);
  Splitter1.Visible := true;

  prjFile := Settings.Root.GetItemValue('LastProjectFile', '');

  if Pos(':', prjFile) = 2 then
    Runner.ProjectFile := prjFile
  else
    Runner.ProjectFile := ExecPath + PROJECTS_PATH + prjFile;

  Runner.LoadFromProjectFile;

  Caption := Format('%s - %s', [Application.Title, Runner.ProjectName]);
end;

//------------------------------------------------------------------------------
procedure TMainForm.SaveSettings;
//------------------------------------------------------------------------------
var
  I: integer;
  Item: TXMLItem;
  prjFile: string;
begin
  Runner.SaveToProjectFile;

  if ExtractFilePath(Runner.ProjectFile) = ExecPath + PROJECTS_PATH then
    prjFile := ExtractFileName(Runner.ProjectFile)
  else
    prjFile := Runner.ProjectFile;

  Settings.Root.SetItemValue('LastProjectFile', prjFile);

  Settings.Root.SetItemParamValue('Main.Window', 'Left', Self.Left);
  Settings.Root.SetItemParamValue('Main.Window', 'Top', Self.Top);
  Settings.Root.SetItemParamValue('Main.Window', 'Width', Self.Width);
  Settings.Root.SetItemParamValue('Main.Window', 'Height', Self.Height);

  if Self.WindowState = wsMaximized then
    Settings.Root.SetItemParamValue('Main.Window', 'State', 'Maximized')
  else
    Settings.Root.SetItemParamValue('Main.Window', 'State', 'Normal');

  Item := Settings.Root.GetItemEx('Main.VList', true);
  for I := 0 to VList.Header.Columns.Count - 1 do
  begin
    Item.SetItemParamValue('Columns.'+IntToStr(I), 'Position', VList.Header.Columns[I].Position);
    Item.SetItemParamValue('Columns.'+IntToStr(I), 'Width', VList.Header.Columns[I].Width);
  end;

  Settings.Root.SetItemParamValue('Main.Panels', 'BottomPanel', BottomPanel.Height);

  Settings.SaveToFile;
end;


//------------------------------------------------------------------------------
procedure TMainForm.RunnerChanged(Sender: TObject; Module: TModuleUnit);
//------------------------------------------------------------------------------
var
  index: integer;
  node: PVirtualNode;
begin
  if VList.RootNodeCount <> Runner.ModuleCount then
  begin
    VList.RootNodeCount := Runner.ModuleCount;
    VList.Invalidate;
  end
  else if Assigned(Module) then
  begin
    // only refresh one row
    index := Runner.GetModuleIndex(Module);
    if (index >= 0) and (index < VList.RootNodeCount) then
    begin
      node := VList.GetFirst;
      while Assigned(node) do
      begin
        if node.Index = index then
        begin
          VList.InvalidateNode(node);
          break;
        end;

        node := VList.GetNext(node);
      end;

    end;
  end
  else
    VList.Invalidate;

end;

//------------------------------------------------------------------------------
procedure TMainForm.aStartExecute(Sender: TObject);
//------------------------------------------------------------------------------
var
  Node: PVirtualNode;
  module: TModuleUnit;
begin
  if (VList.SelectedCount = 0) and (VList.RootNodeCount > 0) then
    VList.Selected[VList.GetFirst()] := true;

  Node := VList.GetFirstSelected();
  while Assigned(Node) do
  begin
    module := Runner.GetModuleByIndex(Node.Index);

    if Assigned(module) and (not module.Running) then
      module.Start;

    Node := VList.GetNextSelected(Node);
  end;
end;

//------------------------------------------------------------------------------
procedure TMainForm.aStopExecute(Sender: TObject);
//------------------------------------------------------------------------------
var
  Node: PVirtualNode;
  module: TModuleUnit;
begin
  if (VList.SelectedCount = 0) and (VList.RootNodeCount > 0) then
    VList.Selected[VList.GetFirst()] := true;

  Node := VList.GetFirstSelected();
  while Assigned(Node) do
  begin
    module := Runner.GetModuleByIndex(Node.Index);

    if Assigned(module) and module.Running then
      module.Stop;

    Node := VList.GetNextSelected(Node);
  end;
end;

procedure TMainForm.Autoscroll1Click(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------
procedure TMainForm.aExecCmdExecute(Sender: TObject);
//------------------------------------------------------------------------------
var
  Node: PVirtualNode;
  module: TModuleUnit;
begin
  Node := VList.GetFirstSelected();
  while Assigned(Node) do
  begin
    module := Runner.GetModuleByIndex(Node.Index);

    if Assigned(module) then
      module.StartWithCmd;

    Node := VList.GetNextSelected(Node);
  end;
end;

//------------------------------------------------------------------------------
procedure TMainForm.aLoadProjectExecute(Sender: TObject);
//------------------------------------------------------------------------------
begin
  OD.InitialDir := ExecPath + PROJECTS_PATH;
  if OD.Execute then
  begin
    Runner.StopAll;

    mLog.Lines.Clear;
    Runner.AddMessage(nil, Format('Loading project %s...', [ExtractFileName(OD.FileName)]));
    Runner.ProjectFile := OD.FileName;
    Runner.LoadFromProjectFile;

    Caption := Format('%s - %s', [Application.Title, Runner.ProjectName]);
  end;
end;

//------------------------------------------------------------------------------
procedure TMainForm.aMessageClearExecute(Sender: TObject);
//------------------------------------------------------------------------------
begin
  mLog.Lines.Clear;
end;

//------------------------------------------------------------------------------
procedure TMainForm.aSaveProjectExecute(Sender: TObject);
//------------------------------------------------------------------------------
begin
  SD.InitialDir := ExecPath + PROJECTS_PATH;
  if SD.Execute then
  begin
    Runner.AddMessage(nil, Format('Saving project %s...', [ExtractFileName(SD.FileName)]));
    Runner.ProjectFile := SD.FileName;
    Runner.SaveToProjectFile;

    Caption := Format('%s - %s', [Application.Title, Runner.ProjectName]);
  end;
end;

//------------------------------------------------------------------------------
procedure TMainForm.aAddModulesExecute(Sender: TObject);
//------------------------------------------------------------------------------
var
  module: TModuleUnit;
  Node: PVirtualNode;
  I: integer;
  InstanceID, Cnt: integer;
begin

  Node := VList.GetFirstSelected;
  if Assigned(Node) then
  begin
    module := Runner.GetModuleByIndex(Node.Index);
    if Assigned(module) then
    begin
      ModuleForm.eProgramName.Text := module.ProgramName;
      ModuleForm.eParameters.Text := module.OrigParameters;
      ModuleForm.eInstanceNumber.Text := IntToStr(Runner.GetMaxInstanceID + 1);
      ModuleForm.cbAddInstanceID.Checked := module.AddInstanceIDToParameters;
    end;
  end;

  ModuleForm.EditMode := false;
  ModuleForm.MultiEdit := false;

  if ModuleForm.ShowModal = mrOK then
  begin
    InstanceID := StrToIntDef(ModuleForm.eInstanceNumber.Text, 1);
    Cnt := StrToIntDef(ModuleForm.eInstanceCount.Text, 1);

    for I := 1 to Cnt do
    begin
      Runner.AddModule(ModuleForm.eProgramName.Text, ModuleForm.eParameters.Text, InstanceID, ModuleForm.cbAddInstanceID.Checked);
      Inc(InstanceID, 1);
    end;

    Runner.DoChanged;
  end;
end;


//------------------------------------------------------------------------------
procedure TMainForm.aStartAllExecute(Sender: TObject);
//------------------------------------------------------------------------------
begin
  Runner.StartAll;
end;

//------------------------------------------------------------------------------
procedure TMainForm.aStopAllExecute(Sender: TObject);
//------------------------------------------------------------------------------
begin
  Runner.StopAll;
end;

//------------------------------------------------------------------------------
procedure TMainForm.aAutoScrollExecute(Sender: TObject);
begin
  MessagesAutoScroll := not MessagesAutoScroll;
  aAutoScroll.Checked := MessagesAutoScroll;
end;

procedure TMainForm.aDeleteExecute(Sender: TObject);
//------------------------------------------------------------------------------
var
  Node: PVirtualNode;
  module: TModuleUnit;
  indexList: TList<integer>;
  I: Integer;
begin
  indexList := TList<integer>.Create;
  try
    Node := VList.GetFirstSelected();
    while Assigned(Node) do
    begin
      indexList.Add(Node.Index);

      Node := VList.GetNextSelected(Node);
    end;

    if MessageDlg(Format('Do you want to delete %d modules?', [indexList.Count]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      for I := indexList.Count - 1 downto 0 do
        Runner.DeleteByIndex(indexList[I]);

  finally
    indexList.Free;
  end;
  Runner.DoChanged;
  SaveSettings;
end;

//------------------------------------------------------------------------------
procedure TMainForm.aEditExecute(Sender: TObject);
//------------------------------------------------------------------------------
var
  Node: PVirtualNode;
  module: TModuleUnit;

  programName, parameters: string;
  cnt: integer;
begin
  programName := ''; parameters := ''; cnt := 0;

  Node := VList.GetFirstSelected();
  while Assigned(Node) do
  begin
    module := Runner.GetModuleByIndex(Node.Index);

    if Assigned(module) then
    begin
      if programName = '' then
        programName := module.ProgramName;
      if parameters = '' then
        parameters := module.OrigParameters;
      Inc(cnt);

      module.Stop;
    end;

    Node := VList.GetNextSelected(Node);
  end;

  if cnt > 0 then
  begin
    ModuleForm.EditMode := true;
    ModuleForm.MultiEdit := cnt > 1;

    ModuleForm.eProgramName.Text := programName;
    ModuleForm.eParameters.Text := parameters;
    if Assigned(module) then
      ModuleForm.cbAddInstanceID.Checked := module.AddInstanceIDToParameters
    else
      ModuleForm.cbAddInstanceID.Checked := true;

    if cnt = 1 then
      ModuleForm.eInstanceNumber.Text := IntToStr(module.InstanceID);

    if ModuleForm.ShowModal = mrOk then
    begin

      Node := VList.GetFirstSelected();
      while Assigned(Node) do
      begin
        module := Runner.GetModuleByIndex(Node.Index);

        if Assigned(module) then
        begin
          module.ProgramName := ModuleForm.eProgramName.Text;
          if cnt = 1 then
            module.InstanceID := StrToIntDef(ModuleForm.eInstanceNumber.Text, 1);
          module.Parameters := ModuleForm.eParameters.Text;
          module.AddInstanceIDToParameters := ModuleForm.cbAddInstanceID.Checked;
        end;

        Node := VList.GetNextSelected(Node);
      end;

      Runner.DoChanged;
      SaveSettings;
    end;
  end;
end;


//------------------------------------------------------------------------------
procedure TMainForm.VListDblClick(Sender: TObject);
//------------------------------------------------------------------------------
begin
  aEdit.Execute;
end;

//------------------------------------------------------------------------------
procedure TMainForm.VListGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
  var HintText: string);
var
  module: TModuleUnit;
begin
  module := Runner.GetModuleByIndex(Node.Index);
  if Assigned(module) then
  begin
    case Column of
      0: HintText := ExtractFileName(module.ProgramName);
      1: HintText := IntToStr(module.InstanceID);
      2: HintText := module.Parameters;
      4: HintText := module.Status;
    end;
  end;
end;

procedure TMainForm.VListGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
//------------------------------------------------------------------------------
var
  module: TModuleUnit;
begin
  module := Runner.GetModuleByIndex(Node.Index);
  if Assigned(module) then
  begin
    if Column = 0 then
    begin
      if module.Running then
        ImageIndex := 1
      else
        ImageIndex := 0;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TMainForm.VListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
//------------------------------------------------------------------------------
var
  module: TModuleUnit;
begin
  module := Runner.GetModuleByIndex(Node.Index);
  if Assigned(module) then
  begin
    case Column of
      0: CellText := ExtractFileName(module.ProgramName);
      1: CellText := IntToStr(module.InstanceID);
      2: CellText := module.Parameters;
      3: CellText := IfThen(module.Running, 'Running', '');
      4: CellText := module.Status;
      5: CellText := IntToStr(module.RunCount);
      6: begin
        if module.RunLastTime > 1000 then
          CellText := FormatFloat('0.####', module.RunLastTime / 1000.0) + ' sec'
        else
          CellText := Format('%d msec', [module.RunLastTime]);
      end;
      7: CellText := IntToStr(module.RunScore);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TMainForm.VListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//------------------------------------------------------------------------------
begin
  if Key = VK_DELETE then
    aDelete.Execute
  else if Key = VK_INSERT then
    aAddModules.Execute;
end;



//------------------------------------------------------------------------------
procedure TMainForm.MessageTimerTimer(Sender: TObject);
//------------------------------------------------------------------------------
var
  messages: TStringList;
begin
  messages := Runner.GetLastMessages;
  try
    if messages.Count > 0 then
    begin
      mLog.Lines.AddStrings(messages);

      { very slow
      if mLog.Lines.Count > 2000 then
      begin
        mLog.Lines.BeginUpdate;
        try
          while mLog.Lines.Count > 1000 do
            mLog.Lines.Delete(0);
        finally
          mLog.Lines.EndUpdate;
        end;
      end;}
      //mLog.ScrollBy(0, mLog.Lines.Count);
      if MessagesAutoScroll then
        SendMessage(mLog.Handle, EM_LINESCROLL, 0, mLog.Lines.Count);

      BottomPanel.Caption := Format('Messages (%d)', [mLog.Lines.Count]);
    end;
  finally
    messages.Free;
  end;
end;

end.
