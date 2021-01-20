unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ucommthread, ucommon;

type
  TMainForm = class(TForm)
    IOChannelValueLabel10: TLabel;
    IOChannelValueLabel11: TLabel;
    IOChannelValueLabel12: TLabel;
    DurationLabel1: TLabel;
    DurationLabel2: TLabel;
    DurationLabel3: TLabel;
    DurationLabel4: TLabel;
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
  FCommThread.OnTrigger := @OnCommTrigger;
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
  DurationLabel: TLabel;
  ChannelStr: AnsiString;
begin
  for Channel in UCommon.Channels do
  begin
    ChannelStr := IntToStr(Channel);
    ValueLabel := FindComponent('IOChannelValueLabel' + ChannelStr) as TLabel;
    if Assigned(ValueLabel) then
      ValueLabel.Caption := IOValueToStr(FIOValues[Channel]);

    DurationLabel := FindComponent('DurationLabel' + ChannelStr) as TLabel;
    if Assigned(DurationLabel) then
      DurationLabel.Caption := FloatToStrF(0.001*(GetTickCount64 - FTriggerTicks[Channel]), ffFixed, 15, 1);
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
  Duration: Longint;
  Ticks: Longint;
begin
  Ticks := GetTickCount64;
  Duration := Ticks - FTriggerTicks[Channel];
  FIOValues[Channel] := NewValue;
  FTriggerTicks[Channel] := Ticks;
  LogChannelTrigger(Channel, NewValue, OldValue, Duration);
end;

end.

