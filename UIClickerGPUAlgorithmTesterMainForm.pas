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
  StdCtrls, ComCtrls, ExtCtrls, ClickerUtils, ClickerCLUtils;

type

  { TfrmUIClickerGPUAlgorithmTester }

  TfrmUIClickerGPUAlgorithmTester = class(TForm)
    imgGPUOption: TImage;
    imgPlatform: TImage;
    imgDevice: TImage;
    lblPlatform: TLabel;
    lblDevice: TLabel;
    lblGPUOption: TLabel;
    memLog: TMemo;
    prbPlatform: TProgressBar;
    prbDevice: TProgressBar;
    prbGPUOption: TProgressBar;
    spdbtnPause: TSpeedButton;
    spdbtnRunAll: TSpeedButton;
    spdbtnStop: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure spdbtnPauseClick(Sender: TObject);
    procedure spdbtnRunAllClick(Sender: TObject);
    procedure spdbtnStopClick(Sender: TObject);
  private
    FDeviceCount: TIntArr; //The length of this array is PlatformCount.
    FRunner_Proc: TAsyncProcess;
    FPaused: Boolean;
    FStopping: Boolean;
    FAuthStr: string;

    procedure AddToLog(s: string);
    procedure StartTestRunner;
    procedure StopTestRunner;
    procedure GetCLInfoFromRunner;
    procedure RunGPUTestPerTarget(APlatformIndex, ADeviceIndex, AGPUOptionIndex: Integer; AGPUOptions: string);
  public

  end;

var
  frmUIClickerGPUAlgorithmTester: TfrmUIClickerGPUAlgorithmTester;

implementation

{$R *.frm}

uses
  UITestUtils, ClickerActionsClient, Expectations, PitstopTestUtils;

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

{ TfrmUIClickerGPUAlgorithmTester }


procedure TfrmUIClickerGPUAlgorithmTester.AddToLog(s: string);
begin
  memLog.Lines.Add(DateTimeToStr(Now) + '  ' + s);
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
  ListOfUIClickerVars: TStringList;
  i, PlatformCount: Integer;
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

  ListOfUIClickerVars := TStringList.Create;
  try
    ListOfUIClickerVars.LineBreak := #13#10;
    ListOfUIClickerVars.Text := FastReplace_87ToReturn(UIClickerVars);

    PlatformCount := StrToIntDef(ListOfUIClickerVars.Values['$CL.PlatformCount$'], 0);
    if (PlatformCount > 0) and (PlatformCount < 100) then
    begin
      SetLength(FDeviceCount, PlatformCount);
      for i := 0 to Length(FDeviceCount) - 1 do
        FDeviceCount[i] := StrToIntDef(ListOfUIClickerVars.Values['$CL.DeviceCount[' + IntToStr(i) + ']$'], 0);
    end;
  finally
    ListOfUIClickerVars.Free;
  end;
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


procedure TfrmUIClickerGPUAlgorithmTester.RunGPUTestPerTarget(APlatformIndex, ADeviceIndex, AGPUOptionIndex: Integer; AGPUOptions: string);
//const
  //CAfterAllPrefix = 'AfterAll_AlwaysExecute';
  //CFindSubControlTestName = 'Test_FindDashBitOnMainUIClickerWindow_HappyFlow';  //Test_FindDashBitOnMainUIClickerWindow_HappyFlow=tsPassed(::)
var
  Response, Params, UIClickerGPUDbgResults: string;
  TestLine, TestName, TestResult: string;
  ListOfTests: TStringList;
  ErrorMessage, RunInfo: string;
  i: Integer;
begin
  Params := '$SetPlatformsAndDevices$=True' + '&' +
            '$RunOnAllPlatformsAndDevices$=True' + '&' +
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

  Params := 'Category=' + CFindSubControlOnGPUCategoryName + '&' + CPitstopCmd_Param_Auth + '=' + FAuthStr;
  Response := SendTextRequestToServer('http://127.0.0.1:7472/' + CPitstopCmd_RunCategory + '?' + Params);

  AddToLog('Platform ' + IntToStr(APlatformIndex) + ', Device ' + IntToStr(ADeviceIndex) + ', GPUOption ' + IntToStr(AGPUOptionIndex) + ': ');
  ListOfTests := TStringList.Create;
  try
    ListOfTests.LineBreak := #4#5;
    ListOfTests.Text := Response;

    for i := 0 to ListOfTests.Count - 1 do
    begin
      TestLine := ListOfTests.Strings[i]; //e.g.  BeforeAll_AlwaysExecute=tsPassed(::)(:.:)Received CL vars.
      ParseTestResult(TestLine, TestName, TestResult, ErrorMessage, RunInfo);

      AddToLog('    TestName: ' + TestName);
      AddToLog('        TestResult: ' + TestResult);
      AddToLog('        ErrorMessage: ' + ErrorMessage);

      if Pos('$RemoteExecResponse$', RunInfo) = 1 then
      begin
        UIClickerGPUDbgResults := GetGPUDbgResultVarsFromAllVars(FastReplace_87ToReturn(RunInfo));  //RunInfo contains UIClicker vars
        AddToLog('        RunInfo: ' + UIClickerGPUDbgResults);
      end
      else
        AddToLog('        RunInfo: ' + RunInfo);
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


procedure TfrmUIClickerGPUAlgorithmTester.spdbtnRunAllClick(Sender: TObject);
var
  i, j, k: Integer;
  GPUOptions: string;
  GPUOptionCount: Integer;
  PlatformCountStr, DeviceCountStr, GPUOptionCountStr: string;
begin
  FStopping := False;
  spdbtnRunAll.Enabled := False;
  spdbtnPause.Enabled := True;
  spdbtnStop.Enabled := True;
  try
    StartTestRunner;
    try
      GetCLInfoFromRunner;
      GPUOptionCount := (1 shl Length(CGPUOptions));
      GPUOptionCountStr := IntToStr(GPUOptionCount);

      prbPlatform.Max := Length(FDeviceCount);
      PlatformCountStr := IntToStr(prbPlatform.Max);

      for i := 0 to Length(FDeviceCount) - 1 do
      begin
        prbPlatform.Position := i;
        prbDevice.Max := FDeviceCount[i];
        DeviceCountStr := IntToStr(prbDevice.Max);
        lblPlatform.Caption := 'Platform: ' + IntToStr(i) + ' / ' + PlatformCountStr;
        lblPlatform.Repaint;

        for j := 0 to FDeviceCount[i] - 1 do
        begin
          prbGPUOption.Max := GPUOptionCount;
          prbDevice.Position := j;
          lblDevice.Caption := 'Device: ' + IntToStr(j) + ' / ' + DeviceCountStr;
          lblDevice.Repaint;

          for k := 0 to GPUOptionCount - 1 do
          begin
            prbGPUOption.Position := k;
            lblGPUOption.Caption := 'GPU Option: ' + IntToStr(k) + ' / ' + GPUOptionCountStr;
            lblGPUOption.Repaint;

            GPUOptions := GenerateGPUOptionsForRequest(k);
            RunGPUTestPerTarget(i, j, k, GPUOptions);

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
  end;
end;


procedure TfrmUIClickerGPUAlgorithmTester.spdbtnStopClick(Sender: TObject);
begin
  FPaused := False;
  FStopping := True;
  AddToLog('Stopping...');
  SendTextRequestToServer('http://127.0.0.1:7472/StopTests' + '?' + CPitstopCmd_Param_StoppingNow + '=' + 'False' + '&' + CPitstopCmd_Param_Auth + '=' + FAuthStr);
end;


procedure TfrmUIClickerGPUAlgorithmTester.FormCreate(Sender: TObject);
var
  i, NewValue: Integer;
begin
  SetLength(FDeviceCount, 0);
  FRunner_Proc := nil;
  FPaused := False;
  FStopping := False;

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

