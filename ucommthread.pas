unit ucommthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, fphttpclient, ucommon;

type
  TCommTriggerEvent = procedure(Sender: TObject; Channel: TChannel;
    OldValue: TIOValue; NewValue: TIOValue) of Object;
  TCommFirstGetEvent = procedure(Sender: TObject; Channel: TChannel;
    Value: TIOValue) of Object;

  TCommThread = class(TThread)
  private
    FUrl: AnsiString;
    FGetRequest: AnsiString;
    FRequestInterval: Longint;
    FChannels: set of TChannel;
    FValues: array[TChannel] of TIOValue;
    FBufferedValues: array[TChannel] of TIOValue;
    FFirstGetDone: Boolean;
    FOnTrigger: TCommTriggerEvent;
    FOnFirstGet: TCommFirstGetEvent;
    FSyncArgChannel: TChannel;
    FSyncArgNewValue: TIOValue;
    FSyncArgOldValue: TIOValue;
    function GetIO(Number: TChannel): AnsiString;
    function StrToValue(Str: AnsiString): TIOValue;
    procedure CopyBufferedValues;
    procedure ProcessFirstGetEvent;
    procedure ProcessTriggerEvent;
    function GetValue(Channel: TChannel): TIOValue;
  protected
    procedure Execute; override;
  public
    constructor CreateThis;
    destructor Destroy; override;
    property Values[Channel: TChannel]: TIOValue read GetValue;
    property OnTrigger: TCommTriggerEvent read FOnTrigger write FOnTrigger;
    property OnFirstGet: TCommFirstGetEvent read FOnFirstGet write FOnFirstGet;
  end;

implementation

constructor TCommThread.CreateThis;
var
  Channel: TChannel;
begin
  inherited Create(True);
  FUrl := UCommon.Url;
  FGetRequest := FUrl + '/io.cgi?io';
  FRequestInterval := Trunc(1000*UCommon.RequestInterval);
  FChannels := UCommon.Channels;
  for Channel in TChannel do
  begin
    FValues[Channel] := iovNone;
  end;
  FFirstGetDone := False;
  FOnTrigger := nil;
  FOnFirstGet := nil;
end;


destructor TCommThread.Destroy;
begin
  inherited;
end;


procedure TCommThread.Execute;
var
  ValueStr: AnsiString;
  OldValue: TIOValue;
  NewValue: TIOValue;
  Channel: TChannel;
  IntervalStartTicks: Longint;
begin
  while not Terminated do
  begin
    IntervalStartTicks := GetTickCount64;

    for Channel in FChannels do
    begin
      if Terminated then
        Break;
      ValueStr := GetIO(Channel);
      NewValue := StrToValue(ValueStr);
      OldValue := FBufferedValues[Channel];
      if (NewValue <> OldValue) or not FFirstGetDone then
      begin
        FSyncArgChannel := Channel;
        FSyncArgOldValue := OldValue;
        FSyncArgNewValue := NewValue;
        if not FFirstGetDone
          then Synchronize(@ProcessFirstGetEvent)
          else Synchronize(@ProcessTriggerEvent);
        FBufferedValues[Channel] := NewValue;
      end;
    end;

    FFirstGetDone := True;

    Synchronize(@CopyBufferedValues);

    while GetTickCount64 - IntervalStartTicks < FREquestInterval do
    begin
      if Terminated then
        Break;
      Sleep(10);
    end;
  end;
end;


function TCommThread.StrToValue(Str: AnsiString): TIOValue;
begin
  case Str of
    '0': Result := iovLow;
    '1': Result := iovHigh;
    else Result := iovNone;
  end;

end;


function TCommThread.GetIO(Number: TChannel): AnsiString;
var
  RawJson: AnsiString;
  JsonArray: TJSONArray;

begin
  try
     RawJson := TFPHTTPClient.SimpleGet(FGetRequest + IntToStr(Number));
     JsonArray := TJSONArray(GetJSON(RawJson));
  except
  end;
  Result := '';
end;


procedure TCommThread.CopyBufferedValues;
var
  Channel: TChannel;
begin
  for Channel in FChannels do
  begin
    FValues[Channel] := FBufferedValues[Channel];
  end;
end;


function TCommThread.GetValue(Channel: TChannel): TIOValue;
begin
  Result := FValues[Channel];
end;

procedure TCommThread.ProcessFirstGetEvent;
begin
  if Assigned(FOnFirstGet) then
    FOnFirstGet(Self, FSyncArgChannel, FSyncArgNewValue);
end;

procedure TCommThread.ProcessTriggerEvent;
begin
  if Assigned(FOnTrigger) then
    FOnTrigger(Self, FSyncArgChannel, FSyncArgOldValue, FSyncArgNewValue);
end;

end.

