{
    Copyright (C) 2026 VCC
    creation date: 01 Feb 2026
    initial release date: 03 Feb 2026

    author: VCC
    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"),
    to deal in the Software without restriction, including without limitation
    the rights to use, copy, modify, merge, publish, distribute, sublicense,
    and/or sell copies of the Software, and to permit persons to whom the
    Software is furnished to do so, subject to the following conditions:
    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
    DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
    OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}


unit UIClickerGPUAlgorithmTesterMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, AsyncProcess,
  StdCtrls, ComCtrls, ExtCtrls, ClickerUtils, ClickerCLUtils, GPUTestUtils,
  VirtualTrees, ImgList, Menus;

type
  TEntryRec = record
    Entry: string;
    ImageList: TImageList;
    ImageIndex: Integer;
    GPUOptions: string; //has content if the node is a GPU option node
    PlatformIndex, DeviceIndex, GPUOptionsIndex: Integer;
  end;
  PEntryRec = ^TEntryRec;

  { TfrmUIClickerGPUAlgorithmTester }

  TfrmUIClickerGPUAlgorithmTester = class(TForm)
    chkDisplayGPUOptionsInTree: TCheckBox;
    cmbTestCategory: TComboBox;
    imglstTarget: TImageList;
    imgGPUOption: TImage;
    imglstTestStatus: TImageList;
    imgPlatform: TImage;
    imgDevice: TImage;
    lblTestCategory: TLabel;
    lblPlatform: TLabel;
    lblDevice: TLabel;
    lblGPUOption: TLabel;
    memLog: TMemo;
    MenuItem_CollapseAll: TMenuItem;
    MenuItem1_ExpandAll: TMenuItem;
    Separator1: TMenuItem;
    MenuItem_RerunSelectedTestOption: TMenuItem;
    MenuItem_CopySelectedItemToClipboard: TMenuItem;
    PageControlMain: TPageControl;
    pmVST: TPopupMenu;
    prbPlatform: TProgressBar;
    prbDevice: TProgressBar;
    prbGPUOption: TProgressBar;
    spdbtnPause: TSpeedButton;
    spdbtnRunAll: TSpeedButton;
    spdbtnStop: TSpeedButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    vstResults: TVirtualStringTree;
    procedure chkDisplayGPUOptionsInTreeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem1_ExpandAllClick(Sender: TObject);
    procedure MenuItem_CollapseAllClick(Sender: TObject);
    procedure MenuItem_CopySelectedItemToClipboardClick(Sender: TObject);
    procedure MenuItem_RerunSelectedTestOptionClick(Sender: TObject);
    procedure spdbtnPauseClick(Sender: TObject);
    procedure spdbtnRunAllClick(Sender: TObject);
    procedure spdbtnStopClick(Sender: TObject);
    procedure vstResultsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: boolean; var ImageIndex: integer);
    procedure vstResultsGetImageIndexEx(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: boolean; var ImageIndex: integer;
      var ImageList: TCustomImageList);
    procedure vstResultsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private
    FGPUInfo: TPlatformInfoArr;

    FRunner_Proc: TAsyncProcess;
    FPaused: Boolean;
    FStopping: Boolean;
    FAuthStr: string;
    FDisplayGPUOptionsInTree: Boolean;

    procedure AddToLog(s: string);

    procedure LoadSettingsFromIni;
    procedure SaveSettingsToIni;

    procedure StartTestRunner;
    procedure StopTestRunner;

    procedure GetCLInfoFromRunner;
    procedure AddCLInfoToLog;
    function GetNodeText(ANode: PVirtualNode): string;
    procedure SetAllNodesExpandedState(AIsExpanded: Boolean);

    procedure RunGPUTestPerTarget(APlatformIndex, ADeviceIndex, AGPUOptionIndex: Integer; AGPUOptions: string; ACategoryNode: PVirtualNode);
  public

  end;

var
  frmUIClickerGPUAlgorithmTester: TfrmUIClickerGPUAlgorithmTester;

implementation

{$R *.frm}

uses
  UITestUtils, ClickerActionsClient, Expectations, PitstopTestUtils,
  Clipbrd, ClickerIniFiles;

const
  CGPUOptions: array[0..7] of string = (
    CGPUDbgVar_GPUIncludeDashG,
    CGPUDbgVar_GPUSlaveQueueFromDevice,
    CGPUDbgVar_GPUUseAllKernelsEvent,
    CGPUDbgVar_GPUNdrangeNoLocalParam,
    CGPUDbgVar_GPUUseEventsInEnqueueKernel,
    CGPUDbgVar_GPUWaitForAllKernelsToBeDone,
    CGPUDbgVar_GPUReleaseFinalEventAtKernelEnd,
    CGPUDbgVar_GPUIgnoreExecutionAvailability
  );

  CGetGPUInfoCategoryName = 'TTestGPUSettingsInfo';
  CFindSubControlOnGPUCategoryName = 'TTestGPUSettingsByTarget';
  CFindSubControlOnGPUSingleActionCategoryName = 'TTestGPUSettingsByTargetWithSingleAction';

  COptionStr = 'Option ';

{ TfrmUIClickerGPUAlgorithmTester }


procedure TfrmUIClickerGPUAlgorithmTester.AddToLog(s: string);
begin
  memLog.Lines.Add(DateTimeToStr(Now) + '  ' + s);
end;


function GetIniFileName: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'UIClickerGPUAlgorithmTester.ini';
end;


procedure TfrmUIClickerGPUAlgorithmTester.LoadSettingsFromIni;
var
  Ini: TClkIniReadonlyFile;
begin
  Ini := TClkIniReadonlyFile.Create(GetIniFileName);
  try
    Left := Ini.ReadInteger('Window', 'Left', Left);
    Top := Ini.ReadInteger('Window', 'Top',  Top);
    Width := Ini.ReadInteger('Window', 'Width', Width);
    Height := Ini.ReadInteger('Window', 'Height', Height);

    chkDisplayGPUOptionsInTree.Checked := Ini.ReadBool('Settings', 'DisplayGPUOptionsInTree', chkDisplayGPUOptionsInTree.Checked);
    PageControlMain.ActivePageIndex := Ini.ReadInteger('Settings', 'Main.ActivePageIndex', 0);
    cmbTestCategory.ItemIndex := Ini.ReadInteger('Settings', 'TestCategory', cmbTestCategory.ItemIndex);
  finally
    Ini.Free;
  end;
end;


procedure TfrmUIClickerGPUAlgorithmTester.SaveSettingsToIni;
var
  Ini: TClkIniFile;
begin
  Ini := TClkIniFile.Create(GetIniFileName);
  try
    Ini.WriteInteger('Window', 'Left', Left);
    Ini.WriteInteger('Window', 'Top', Top);
    Ini.WriteInteger('Window', 'Width', Width);
    Ini.WriteInteger('Window', 'Height', Height);

    Ini.WriteBool('Settings', 'DisplayGPUOptionsInTree', chkDisplayGPUOptionsInTree.Checked);
    Ini.WriteInteger('Settings', 'Main.ActivePageIndex', PageControlMain.ActivePageIndex);
    Ini.WriteInteger('Settings', 'TestCategory', cmbTestCategory.ItemIndex);

    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


procedure TfrmUIClickerGPUAlgorithmTester.StartTestRunner;
var
  PathToTestRunner: string;
begin
  PathToTestRunner := ExtractFilePath(ParamStr(0)) + '..\UIClicker\Tests\UIClickerHTTPTests.exe';
  FRunner_Proc := CreateUIClickerProcess(PathToTestRunner, '--StartServer --Auth ' + FAuthStr);
end;


procedure TfrmUIClickerGPUAlgorithmTester.StopTestRunner;
begin
  if FRunner_Proc <> nil then
  begin
    FRunner_Proc.Terminate(0);
    FRunner_Proc.Free;
  end;
end;


procedure TfrmUIClickerGPUAlgorithmTester.GetCLInfoFromRunner;
var
  Response, Params, UIClickerVars: string;
begin
  Params := '$SetPlatformsAndDevices$=False' + '&' + CPitstopCmd_Param_Auth + '=' + FAuthStr;
  Response := SendTextRequestToServer('http://127.0.0.1:7472/' + CPitstopCmd_SetTestVars + '?' + Params);  //should be something like 'BeforeAll_AlwaysExecute=tsPassed(::)(:.:)$RemoteExecResponse$=1$Control_Text$=...'

  try
    Expect(Response).ToBe('Done');
  except
    on E: Exception do
      AddToLog('GetCLInfoFromRunner: ' + E.Message);
  end;

  Params := 'Category=' + CGetGPUInfoCategoryName + '&' + CPitstopCmd_Param_Auth + '=' + FAuthStr;
  Response := SendTextRequestToServer('http://127.0.0.1:7472/' + CPitstopCmd_RunCategory + '?' + Params);

  UIClickerVars := Copy(Response, Pos(CSecondSeparator, Response) + Length(CSecondSeparator), MaxInt);
  DecodeCLInfoFromUIClickerVars(FastReplace_87ToReturn(UIClickerVars), FGPUInfo);
end;


function GetGPUDbgResultVarsFromAllVars(AAllUIClickerVars: string): string;  //expects #13#10 separated list of strings like $var$=value
var
  i: Integer;
  GPUDbgBufferLen_VarIndex, VarLen: Integer;
  VarLenStr: string;
  ListOfGPUDbgResults: TStringList;
begin
  Result := '';

  ListOfGPUDbgResults := TStringList.Create;
  try
    ListOfGPUDbgResults.LineBreak := #13#10;
    ListOfGPUDbgResults.Text := AAllUIClickerVars;

    GPUDbgBufferLen_VarIndex := ListOfGPUDbgResults.IndexOfName('$GPUDbgBuffer.Len$');
    if GPUDbgBufferLen_VarIndex = -1 then
      Exit;

    VarLenStr := ListOfGPUDbgResults.ValueFromIndex[GPUDbgBufferLen_VarIndex];
    Result := '$GPUDbgBuffer.Len$=' + VarLenStr + ', $GPUDbgBuffer[]$= ';

    VarLen := StrToIntDef(VarLenStr, 0);
    for i := 0 to VarLen - 1 do
    begin
      try
        Result := Result + '[' + IntToStr(i) + ']=' + ListOfGPUDbgResults.ValueFromIndex[i + GPUDbgBufferLen_VarIndex + 1] + ', ';
      except
        Result := 'Bad index ' + IntToStr(i);
      end;
    end;

    Result := Result + CGPUDbgVar_AdditionalGPUInfo + '=' + ListOfGPUDbgResults.Values[CGPUDbgVar_AdditionalGPUInfo];
  finally
    ListOfGPUDbgResults.Free;
  end;

  Delete(Result, Length(Result) - 1, 2);
end;


function GetCategoryStatusFromTestStatus(ACurrentCategoryStatus, ATestStatus: TTestStatus): TTestStatus;
const
  CPassedToTestStatus: array[TTestStatus] of TTestStatus = (tsRunning, tsFailed, tsPassed, tsRunning, tsPaused);
begin
  Result := ACurrentCategoryStatus;

  case ACurrentCategoryStatus of
    tsInit:
      Result := ATestStatus;

    tsFailed:
      ;

    tsPassed:
      Result := CPassedToTestStatus[ATestStatus];

    tsRunning:
      Result := ATestStatus;

    tsPaused:
      Result := ATestStatus;
  end;
end;


procedure TfrmUIClickerGPUAlgorithmTester.RunGPUTestPerTarget(APlatformIndex, ADeviceIndex, AGPUOptionIndex: Integer; AGPUOptions: string; ACategoryNode: PVirtualNode);
//const
  //CAfterAllPrefix = 'AfterAll_AlwaysExecute';
  //CFindSubControlTestName = 'Test_FindDashBitOnMainUIClickerWindow_HappyFlow';  //Test_FindDashBitOnMainUIClickerWindow_HappyFlow=tsPassed(::)
var
  Response, Params, UIClickerGPUDbgResults: string;
  TestLine, TestName, TestResult: string;
  ListOfTests: TStringList;
  ErrorMessage, RunInfo: string;
  i: Integer;
  Node: PVirtualNode;
  NodeData, CategoryNodeData: PEntryRec;
begin
  Params := '$SetPlatformsAndDevices$=True' + '&' +
            '$RunOnAllPlatformsAndDevices$=False' + '&' +
            '$SelectedPlatorm$=' + IntToStr(APlatformIndex) + '&' +
            '$SelectedDevice$=' + IntToStr(ADeviceIndex) + '&' +
            '$SetGPUDbgBuffer$=True' + '&' +
            CPitstopCmd_Param_Auth + '=' + FAuthStr + '&' +
            AGPUOptions;

  Response := SendTextRequestToServer('http://127.0.0.1:7472/' + CPitstopCmd_SetTestVars + '?' + Params);  //should be something like 'BeforeAll_AlwaysExecute=tsPassed(::)(:.:)$RemoteExecResponse$=1$Control_Text$=...'

  try
    Expect(Response).ToBe('Done');
  except
    on E: Exception do
    begin
      AddToLog('RunGPUTestPerTarget, SetTestVars: ' + E.Message);
      Exit;
    end;
  end;

  case cmbTestCategory.ItemIndex of
    0: Params := 'Category=' + CFindSubControlOnGPUCategoryName + '&' + CPitstopCmd_Param_Auth + '=' + FAuthStr;
    1: Params := 'Category=' + CFindSubControlOnGPUSingleActionCategoryName + '&' + CPitstopCmd_Param_Auth + '=' + FAuthStr;
    else
      Params := 'Category=Undefined' + '&' + CPitstopCmd_Param_Auth + '=' + FAuthStr;
  end;

  Response := SendTextRequestToServer('http://127.0.0.1:7472/' + CPitstopCmd_RunCategory + '?' + Params);

  AddToLog('Platform ' + IntToStr(APlatformIndex) + ', Device ' + IntToStr(ADeviceIndex) + ', GPUOption ' + IntToStr(AGPUOptionIndex) + ': ');
  ListOfTests := TStringList.Create;
  try
    ListOfTests.LineBreak := #4#5;
    ListOfTests.Text := Response;

    CategoryNodeData := vstResults.GetNodeData(ACategoryNode);
    CategoryNodeData^.Entry := CategoryNodeData^.Entry + ': ';
    CategoryNodeData^.ImageList := imglstTestStatus;
    CategoryNodeData^.ImageIndex := Ord(tsInit);

    for i := 0 to ListOfTests.Count - 1 do
    begin
      TestLine := ListOfTests.Strings[i]; //e.g.  BeforeAll_AlwaysExecute=tsPassed(::)(:.:)Received CL vars.
      ParseTestResult(TestLine, TestName, TestResult, ErrorMessage, RunInfo);

      Node := vstResults.AddChild(ACategoryNode);
      NodeData := vstResults.GetNodeData(Node);
      NodeData^.Entry := TestName + ' / ' + TestResult + ' / ' + ErrorMessage + ' / ';
      NodeData^.ImageList := imglstTestStatus;
      NodeData^.ImageIndex := Ord(TestStatusAsStringToStatus(TestResult));
      NodeData^.GPUOptions := '';
      NodeData^.PlatformIndex := CategoryNodeData^.PlatformIndex;
      NodeData^.DeviceIndex := CategoryNodeData^.DeviceIndex;
      NodeData^.GPUOptionsIndex := CategoryNodeData^.GPUOptionsIndex;

      CategoryNodeData^.Entry := CategoryNodeData^.Entry + ' ' + TestResult;
      CategoryNodeData^.ImageIndex := Ord(GetCategoryStatusFromTestStatus(TTestStatus(CategoryNodeData^.ImageIndex), TTestStatus(NodeData^.ImageIndex)));

      AddToLog('    TestName: ' + TestName);
      AddToLog('        TestResult: ' + TestResult);
      AddToLog('        ErrorMessage: ' + ErrorMessage);

      if Pos('$RemoteExecResponse$', RunInfo) = 1 then
      begin
        UIClickerGPUDbgResults := GetGPUDbgResultVarsFromAllVars(FastReplace_87ToReturn(RunInfo));  //RunInfo contains UIClicker vars
        AddToLog('        RunInfo: ' + UIClickerGPUDbgResults);
        NodeData^.Entry := NodeData^.Entry + 'RunInfo: ' + UIClickerGPUDbgResults
      end
      else
      begin
        AddToLog('        RunInfo: ' + RunInfo);
        NodeData^.Entry := NodeData^.Entry + 'RunInfo: ' + RunInfo
      end;
    end;
  finally
    ListOfTests.Free;
  end;
end;


function GenerateGPUOptionsForRequest(ACombinationIndex: Integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(CGPUOptions) - 1 do
    Result := Result + CGPUOptions[i] + '=' + BoolToStr((ACombinationIndex shr (Length(CGPUOptions) - i - 1)) and 1 = 1, 'True', 'False') + '&';

  Delete(Result, Length(Result), 1); //delete last '&'
end;


procedure TfrmUIClickerGPUAlgorithmTester.AddCLInfoToLog;
var
  i, j: Integer;
  istr, jstr, ijstr: string;
begin
  AddToLog('Platform count: ' + IntToStr(Length(FGPUInfo)));
  for i := 0 to Length(FGPUInfo) - 1 do
  begin
    istr := IntToStr(i);
    AddToLog('  PlatformName[' + istr + ']: ' + Copy(FGPUInfo[i].PlatformName, 1, 1));
    AddToLog('  PlatformVersion[' + istr + ']: ' + FGPUInfo[i].PlatformVersion);
    AddToLog('  PlatformExtensions[' + istr + ']: ' + FGPUInfo[i].PlatformExtensions);

    AddToLog('  Device count[' + istr + ']: ' + IntToStr(Length(FGPUInfo[i].Devices)));
    for j := 0 to Length(FGPUInfo[i].Devices) - 1 do
    begin
      jstr := IntToStr(j);
      ijstr := istr + ', ' + jstr;
      AddToLog('    DeviceName[' + ijstr + ']: ' + Copy(FGPUInfo[i].Devices[j].DeviceName, 1, 1));
      AddToLog('    DeviceVersion[' + ijstr + ']: ' + FGPUInfo[i].Devices[j].DeviceVersion);
      AddToLog('    DeviceOpenCLCVersion[' + ijstr+ ']: ' + FGPUInfo[i].Devices[j].DeviceOpenCLCVersion);
      AddToLog('    DevicePlatformVersion[' + ijstr + ']: ' + FGPUInfo[i].Devices[j].DevicePlatformVersion);
      AddToLog('    DeviceExtensions[' + ijstr + ']: ' + FGPUInfo[i].Devices[j].DeviceExtensions);
    end;
  end;
end;


procedure TfrmUIClickerGPUAlgorithmTester.spdbtnRunAllClick(Sender: TObject);
var
  i, j, k: Integer;
  GPUOptions: string;
  GPUOptionCount: Integer;
  PlatformCountStr, DeviceCountStr, GPUOptionCountStr: string;
  PlatformNode, DeviceNode, GPUOptionNode: PVirtualNode;
  PlatformNodeData, DeviceNodeData, GPUOptionNodeData: PEntryRec;
begin
  FStopping := False;
  spdbtnRunAll.Enabled := False;
  spdbtnPause.Enabled := True;
  spdbtnStop.Enabled := True;
  try
    StartTestRunner;
    try
      GetCLInfoFromRunner;
      AddCLInfoToLog;
      vstResults.Clear;

      GPUOptionCount := (1 shl Length(CGPUOptions));
      GPUOptionCountStr := IntToStr(GPUOptionCount);

      prbPlatform.Max := Length(FGPUInfo) - 1;
      PlatformCountStr := IntToStr(Length(FGPUInfo));

      for i := 0 to Length(FGPUInfo) - 1 do
      begin
        prbPlatform.Position := i;
        prbDevice.Max := Length(FGPUInfo[i].Devices) - 1;
        DeviceCountStr := IntToStr(Length(FGPUInfo[i].Devices));
        lblPlatform.Caption := 'Platform: ' + IntToStr(i + 1) + ' / ' + PlatformCountStr;
        lblPlatform.Repaint;

        PlatformNode := vstResults.AddChild(vstResults.RootNode);
        PlatformNodeData := vstResults.GetNodeData(PlatformNode);
        PlatformNodeData^.Entry := FGPUInfo[i].PlatformName;
        PlatformNodeData^.ImageList := imglstTarget;
        PlatformNodeData^.ImageIndex := 0;
        PlatformNodeData^.GPUOptions := '';
        PlatformNodeData^.PlatformIndex := i;
        PlatformNodeData^.DeviceIndex := -1;
        PlatformNodeData^.GPUOptionsIndex := -1;

        for j := 0 to Length(FGPUInfo[i].Devices) - 1 do
        begin
          prbGPUOption.Max := GPUOptionCount - 1;
          prbDevice.Position := j;
          lblDevice.Caption := 'Device: ' + IntToStr(j + 1) + ' / ' + DeviceCountStr;
          lblDevice.Repaint;

          DeviceNode := vstResults.AddChild(PlatformNode);
          DeviceNodeData := vstResults.GetNodeData(DeviceNode);
          DeviceNodeData^.Entry := FGPUInfo[i].Devices[j].DeviceName;
          DeviceNodeData^.ImageList := imglstTarget;
          DeviceNodeData^.ImageIndex := 1;
          DeviceNodeData^.GPUOptions := '';
          DeviceNodeData^.PlatformIndex := PlatformNodeData^.PlatformIndex;
          DeviceNodeData^.DeviceIndex := j;
          DeviceNodeData^.GPUOptionsIndex := -1;

          for k := 0 to GPUOptionCount - 1 do
          begin
            prbGPUOption.Position := k;
            lblGPUOption.Caption := 'GPU Option: ' + IntToStr(k + 1) + ' / ' + GPUOptionCountStr;
            lblGPUOption.Repaint;

            GPUOptionNode := vstResults.AddChild(DeviceNode);
            GPUOptionNodeData := vstResults.GetNodeData(GPUOptionNode);
            GPUOptionNodeData^.Entry := COptionStr + IntToStr(k);

            GPUOptions := GenerateGPUOptionsForRequest(k);
            GPUOptionNodeData^.GPUOptions := StringReplace(GPUOptions, '&', ', ', [rfReplaceAll]);
            GPUOptionNodeData^.PlatformIndex := DeviceNodeData^.PlatformIndex;
            GPUOptionNodeData^.DeviceIndex := DeviceNodeData^.DeviceIndex;
            GPUOptionNodeData^.GPUOptionsIndex := k;

            RunGPUTestPerTarget(i, j, k, GPUOptions, GPUOptionNode);

            if FStopping then
              Exit;
          end;
        end;
      end;

      for i := 0 to 9 do
      begin
        Sleep(100);
        Application.ProcessMessages;
      end;
    finally
      StopTestRunner;
    end;
  finally
    spdbtnRunAll.Enabled := True;
    spdbtnPause.Enabled := False;
    spdbtnStop.Enabled := False;
    memLog.Lines.Add(''); //for the next run
  end;
end;


procedure TfrmUIClickerGPUAlgorithmTester.spdbtnStopClick(Sender: TObject);
begin
  FPaused := False;
  FStopping := True;
  AddToLog('Stopping...');
  SendTextRequestToServer('http://127.0.0.1:7472/StopTests' + '?' + CPitstopCmd_Param_StoppingNow + '=' + 'False' + '&' + CPitstopCmd_Param_Auth + '=' + FAuthStr);
end;


procedure TfrmUIClickerGPUAlgorithmTester.vstResultsGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: boolean; var ImageIndex: integer);
begin
  //
end;


procedure TfrmUIClickerGPUAlgorithmTester.vstResultsGetImageIndexEx(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: boolean; var ImageIndex: integer;
  var ImageList: TCustomImageList);
var
  NodeData: PEntryRec;
begin
  NodeData := vstResults.GetNodeData(Node);
  ImageList := NodeData^.ImageList;
  ImageIndex := NodeData^.ImageIndex;
end;


function TfrmUIClickerGPUAlgorithmTester.GetNodeText(ANode: PVirtualNode): string;
var
  NodeData: PEntryRec;
begin
  NodeData := vstResults.GetNodeData(ANode);
  Result := NodeData^.Entry;

  if FDisplayGPUOptionsInTree and (NodeData^.GPUOptions <> '') then
    Result := Result + '  ' + NodeData^.GPUOptions;
end;


procedure TfrmUIClickerGPUAlgorithmTester.vstResultsGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  CellText := GetNodeText(Node);
end;


procedure TfrmUIClickerGPUAlgorithmTester.FormCreate(Sender: TObject);
var
  i, NewValue: Integer;
begin
  SetLength(FGPUInfo, 0);
  FRunner_Proc := nil;
  FPaused := False;
  FStopping := False;
  FDisplayGPUOptionsInTree := False;

  FAuthStr := 'abx';
  Randomize;
  NewValue := Random(MaxInt);
  for i := 0 to Random(10) do
  begin
    Inc(NewValue, Random(MaxInt));
    FAuthStr := FAuthStr + IntToStr(GetTickCount64 + NewValue) + IntToStr(Random(MaxInt)) + IntToStr(Random(MaxInt));
    Sleep(33);
    Randomize;
  end;

  GeneralConnectTimeout := 2000; //just a bit more than default
  vstResults.NodeDataSize := SizeOf(TEntryRec);
  PageControlMain.ActivePageIndex := 0;

  try
    LoadSettingsFromIni;
  except
  end;
end;


procedure TfrmUIClickerGPUAlgorithmTester.chkDisplayGPUOptionsInTreeChange(
  Sender: TObject);
begin
  FDisplayGPUOptionsInTree := chkDisplayGPUOptionsInTree.Checked;
  vstResults.Repaint;
end;


procedure TfrmUIClickerGPUAlgorithmTester.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Length(FGPUInfo) - 1 do
    SetLength(FGPUInfo[i].Devices, 0);

  SetLength(FGPUInfo, 0);

  try
    SaveSettingsToIni;
  except
  end;
end;


procedure TfrmUIClickerGPUAlgorithmTester.SetAllNodesExpandedState(AIsExpanded: Boolean);
var
  Node: PVirtualNode;
begin
  Node := vstResults.GetFirst;
  if Node = nil then
    Exit;

  vstResults.BeginUpdate;
  try
    repeat
      vstResults.Expanded[Node] := AIsExpanded;
      Node := vstResults.GetNext(Node);
    until Node = nil;
  finally
    vstResults.EndUpdate;
  end;
end;


procedure TfrmUIClickerGPUAlgorithmTester.MenuItem1_ExpandAllClick(
  Sender: TObject);
begin
  SetAllNodesExpandedState(True);
end;


procedure TfrmUIClickerGPUAlgorithmTester.MenuItem_CollapseAllClick(
  Sender: TObject);
begin
  SetAllNodesExpandedState(False);
end;


procedure TfrmUIClickerGPUAlgorithmTester.MenuItem_CopySelectedItemToClipboardClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstResults.GetFirstSelected;
  if Node = nil then
    Exit;

  Clipboard.AsText := GetNodeText(Node);
end;


procedure TfrmUIClickerGPUAlgorithmTester.MenuItem_RerunSelectedTestOptionClick(
  Sender: TObject);
var
  GPUOptionCount: Integer;
  GPUOptionNode: PVirtualNode;
  NodeData: PEntryRec;
begin
  GPUOptionNode := vstResults.GetFirstSelected;
  if GPUOptionNode = nil then
  begin
    MessageBoxFunction('Please select a GPU Option item.', PChar(Caption), 0);
    Exit;
  end;

  NodeData := vstResults.GetNodeData(GPUOptionNode);
  if Pos(COptionStr, NodeData^.Entry) <> 1 then
  begin
    MessageBoxFunction('Please rerun from a GPU Option item.', PChar(Caption), 0);
    Exit;
  end;

  FStopping := False;
  StartTestRunner;
  try
    GetCLInfoFromRunner;
    AddCLInfoToLog;

    GPUOptionCount := (1 shl Length(CGPUOptions));
    if NodeData^.GPUOptionsIndex > GPUOptionCount - 1 then
    begin
      MessageBoxFunction(PChar('The selected option index (' + IntToStr(NodeData^.GPUOptionsIndex) + ') is not longer available (' + IntToStr(GPUOptionCount - 1) + ').'), PChar(Caption), 0);
      Exit;
    end;

    AddToLog('Rerunning test at PlatformIndex=' + IntToStr(NodeData^.PlatformIndex) + '  DeviceIndex=' + IntToStr(NodeData^.DeviceIndex) + '  GPUOptions=' + IntToStr(NodeData^.GPUOptionsIndex));
    RunGPUTestPerTarget(NodeData^.PlatformIndex, NodeData^.DeviceIndex, NodeData^.GPUOptionsIndex, NodeData^.GPUOptions, GPUOptionNode);
  finally
    StopTestRunner;
  end;
end;


procedure TfrmUIClickerGPUAlgorithmTester.spdbtnPauseClick(Sender: TObject);
begin
  FPaused := not FPaused;

  if FPaused then
  begin
    AddToLog('Paused...');
    SendTextRequestToServer('http://127.0.0.1:7472/PauseTests' + '?' + CPitstopCmd_Param_Auth + '=' + FAuthStr)
  end
  else
  begin
    AddToLog('Running...');
    SendTextRequestToServer('http://127.0.0.1:7472/ContinueTests' + '?' + CPitstopCmd_Param_Auth + '=' + FAuthStr);
  end;
end;

end.

