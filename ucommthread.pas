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
    FClient: TFPHTTPClient;
    function GetIO(Number: TChannel): AnsiString;
    function StrToValue(Str: AnsiString): TIOValue;
    procedure CopyBufferedValues;
    procedure ProcessFirstGetEvent;
    procedure ProcessTriggerEvent;
    function GetValue(Channel: TChannel): TIOValue;
    function ParseDeviceResponse(Response: AnsiString): AnsiString;
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

const
  UserName = 'visor';
  Password = 'ping';

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

  FClient := TFPHTTPClient.Create(nil);
  FClient.UserName := UserName;
  FClient.Password := Password;
end;


destructor TCommThread.Destroy;
begin
  FClient.Free;
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
  DeviceResponse: AnsiString;
begin
  try
    DeviceResponse := FClient.Get(FGetRequest + IntToStr(Number));
    Result := ParseDeviceResponse(DeviceResponse);
  except
    Result := 'exception';
  end;
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


function TCommThread.ParseDeviceResponse(Response: AnsiString): AnsiString;
label
  OnError;
var
  ResponseLen: Integer;
  ParseIndex: Integer = 1;
  CurrChar: Char;

  procedure SkipSpaces;
  begin
    while ParseIndex <= ResponseLen do
    begin
      CurrChar := Response[ParseIndex];
      if CurrChar <> ' ' then
        Break;
      Inc(ParseIndex);
    end;
  end;

  function ParseSym: AnsiString;
  begin
    Result := '';
    if ParseIndex <= ResponseLen then
    begin
      Result := Response[ParseIndex];
      Inc(ParseIndex);
    end;
  end;

  function ParseStr: AnsiString;
  begin
    Result := '';
    while ParseIndex <= ResponseLen do
    begin
      CurrChar := Response[ParseIndex];
      if (CurrChar < 'a') or (CurrChar > 'z') then
        Break;
      Result := Result + CurrChar;
      Inc(ParseIndex);
    end;
  end;

  function ParseInt: AnsiString;
  begin
    Result := '';
    while ParseIndex <= ResponseLen do
    begin
      CurrChar := Response[ParseIndex];
      if ((CurrChar < '0') or (CurrChar > '9')) and (CurrChar <> '-')  then
        Break;
      Result := Result + CurrChar;
      Inc(ParseIndex);
    end;
  end;

  function IsNotEqual(Str1, Str2: AnsiString): Boolean;
  begin
    Result := AnsiCompareStr(Str1, Str2) <> 0;
  end;

begin
  ResponseLen := Length(Response);

  SkipSpaces;
  if IsNotEqual(ParseStr, 'io') then
    goto OnError;
  if IsNotEqual(ParseSym, '_') then
    goto OnError;
  if IsNotEqual(ParseStr, 'result') then
    goto OnError;
  SkipSpaces;
  if IsNotEqual(ParseSym, '(') then
    goto OnError;
  SkipSpaces;
  if IsNotEqual(ParseSym, '''') then
    goto OnError;
  if IsNotEqual(ParseStr, 'ok') then
    goto OnError;
  if IsNotEqual(ParseSym, '''') then
    goto OnError;
  SkipSpaces;
  if IsNotEqual(ParseSym, ',') then
    goto OnError;
  SkipSpaces;
  Result := ParseInt;
  SkipSpaces;
  if IsNotEqual(ParseSym, ',') then
    goto OnError;
  SkipSpaces;

  Result := ParseInt;
  Exit;

OnError:
  Result := 'error';
end;

end.

