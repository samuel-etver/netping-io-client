unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ucommthread, ucommon;

type

  { TMainForm }

  TMainForm = class(TForm)
    IOChannelValueLabel10: TLabel;
    IOChannelValueLabel11: TLabel;
    IOChannelValueLabel12: TLabel;
    IOChannelValueLabel5: TLabel;
    IOChannelValueLabel6: TLabel;
    IOChannelValueLabel7: TLabel;
    IOChannelValueLabel8: TLabel;
    IOChannelValueLabel9: TLabel;
    Label1: TLabel;
    IOChannelValueLabel1: TLabel;
    VersionLabel: TLabel;
    Label3: TLabel;
    IOChannelValueLabel2: TLabel;
    Label5: TLabel;
    IOChannelValueLabel3: TLabel;
    Label7: TLabel;
    IOChannelValueLabel4: TLabel;
    UpdateTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
  private
    FCommThread: TCommThread;
    FIOValues: array[TChannel] of TIOValue;
    FTriggerTicks: array[TChannel] of Longint;
    procedure OnCommFirstGet(Sender: TObject; Channel: TChannel; Value: TIOValue);
    procedure OnCommTrigger(Sender: TObject; Channel: TChannel; OldValue: TIOValue; NewValue: TIOValue);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  Channel: TChannel;
begin
  VersionLabel.Caption := Version;
  UCommon.LoadConfig;
  UCommon.Startup;

  for Channel in TChannel do
    FIOValues[Channel] := iovNone;

  FCommThread := TCommThread.CreateThis;
  FCommThread.OnFirstGet := @OnCommFirstGet;
  FCommthread.OnTrigger := @OnCommTrigger;
  FCommThread.Start;
end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FCommThread.Terminate;
  FCommThread.WaitFor;
end;


procedure TMainForm.UpdateTimerTimer(Sender: TObject);
var
  Channel: TChannel;
  ValueLabel: TLabel;
begin
  for Channel in UCommon.Channels do
  begin
    ValueLabel := FindComponent('IOChannelValueLabel' + IntToStr(Channel)) as TLabel;
    if Assigned(ValueLabel) then
      ValueLabel.Caption := IOValueToStr(FIOValues[Channel]);
  end;
end;


procedure TMainForm.OnCommFirstGet(Sender: TObject; Channel: TChannel; Value: TIOValue);
begin
  FIOValues[Channel] := Value;
  FTriggerTicks[Channel] := GetTickCount64;
  LogChannelState(Channel, Value);
end;


procedure TMainForm.OnCommTrigger(Sender: TObject; Channel: TChannel; OldValue: TIOValue; NewValue: TIOValue);
var
  Period: Longint;
  Ticks: Longint;
begin
  Ticks := GetTickCount64;
  Period := Ticks - FTriggerTicks[Channel];
  FIOValues[Channel] := NewValue;
  FTriggerTicks[Channel] := Ticks;
  LogChannelTrigger(Channel, NewValue, OldValue, Period);
end;

end.

