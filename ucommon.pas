unit ucommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TChannel = 1..4;
  TIOValue = (iovNone, iovLow, iovHigh);

const
  Version = 'v0.1.0';
  Channels: set of TChannel = [1];
  LogFileName = 'log.txt';

var
  Url: AnsiString;
  LogFilePath: AnsiString;
  RequestInterval: Real;

  procedure LoadConfig;
  procedure Startup;

  function IOValueToStr(Value: TIOValue): AnsiString;

  procedure LogWrite(Txt: AnsiString);
  procedure LogWriteLn(Txt: AnsiString);
  procedure LogNl;
  procedure LogDate;
  procedure LogDate(Txt: AnsiString; NewLine: Boolean = True);
  procedure LogChannelState(Channel: TChannel; Value: TIOValue);
  procedure LogChannelTrigger(Channel: TChannel; NewValue: TIOValue; OldValue: TIOValue; Duration: Longint);
  procedure LogSpace;
  procedure LogSpaces(N: Integer);
  procedure LogFlush;


implementation

uses
  Forms, ulog;

const
  UrlDefault = 'http://192.168.0.100';
  LogFilePathDefault = 'Log';
  RequestIntervalDefault = 1.0;

var
  Log: TLog;


procedure Init;
begin
  Log := nil;
end;


procedure LoadConfig;
var
  Path: AnsiString;
  Lines: TStringList;

  function GetStr(Key: AnsiString; DefValue: AnsiString): AnsiString;
  begin
    Result := Lines.Values[Key];
    if Result.IsEmpty then
      Result := DefValue;
  end;

  function GetFloat(Key: AnsiString; DefValue: Real): Real;
  begin
    try
       Result := StrToFloat(Lines.Values[Key]);
    except
       Result := DefValue;
    end;
  end;

begin
  Path := ChangeFileExt(Application.ExeName, '.config');
  Lines := TStringList.Create;
  try
     Lines.LoadFromFile(Path);
  except
  end;

  Url := GetStr('URL', UrlDefault);
  LogFilePath := GetStr('LogFilePath', LogFilePathDefault);
  RequestInterval := GetFloat('RequestInterval', RequestIntervalDefault);

  Lines.Free;
end;


procedure Startup;
begin
  Log := TLog.Create(LogFilePath + DirectorySeparator + LogFileName);

  LogNl;
  LogWriteLn('---');
  LogDate('Программа запущена');
  LogNl;
end;


procedure Fin;
begin
  LogDate('Программа остановлена');
  if Assigned(Log) then
    Log.Free;
end;


function IOValueToStr(Value: TIOValue): AnsiString;
begin
  case Value of
    iovLow:
      Result := '0';
    iovHigh:
      Result := '1';
    else
      Result := '?';
  end;
end;


procedure LogWrite(Txt: AnsiString);
begin
  Log.Write(Txt);
end;


procedure LogWriteLn(Txt: AnsiString);
begin
  Log.Write(Txt);
  LogNl;
end;


procedure LogNl;
begin
  Log.Write(LineEnding);
end;


procedure LogDate;
var
  DtTm: TDateTime;
  Year, Mon, Day: Word;
  Hour, Min, Sec, MlSec: Word;
  Txt: AnsiString;

  function to02(Value: Word): AnsiString;
  begin
    if Value < 10
      then Result := '0' + IntToStr(Value)
      else Result := IntToStr(Value);
  end;

begin
  DtTm := Now;
  DecodeDate(DtTm, Year, Mon, Day);
  DecodeTime(DtTm, Hour, Min, Sec, MlSec);

  Txt := IntToStr(Year) + '-' +
         to02(Mon) + '-' +
         to02(Day) + ' ' +
         to02(Hour) + ':' +
         to02(Min) + ':' +
         to02(Sec);

  LogWrite(Txt);
end;


procedure LogDate(Txt: AnsiString; NewLine: Boolean);
begin
  LogDate;
  LogSpace;
  LogWrite(Txt);
  if NewLine then
    LogNl;
end;


procedure LogChannelState(Channel: TChannel; Value: TIOValue);
begin
  LogDate('Состояние входа ' + IntToStr(Channel) + ': ' + IOValueToStr(Value));
  LogNl;
end;


procedure LogChannelTrigger(Channel: TChannel; NewValue: TIOValue; OldValue: TIOValue; Duration: Longint);
var
  Txt: AnsiString = '';
  ChannelStr: AnsiString;
  DurationStr: AnsiString;

begin
  if OldValue = NewValue then
    Exit;

  ChannelStr := IntToStr(Channel);
  DurationStr := '(' + FloatToStrF(0.001*Duration, ffFixed, 15, 1) + 'c)';

  if OldValue = iovNone then
  begin
    Txt := 'Восстановление связи' + DurationStr + '. Вход ' + ChannelStr +
      ': ' + IOValueToStr(NewValue);
  end
  else if NewValue = iovNone then
  begin
    Txt := 'Потеря связи. Вход ' + ChannelStr + ': ' + IOValueToStr(OldValue) +
      DurationStr;
  end
  else
  begin
    Txt := 'Изменения состояние входа ' + ChannelStr + ': из ' + IOValueToStr(OldValue) +
      DurationStr + ' в ' + IOValueToStr(NewValue);
  end;

  LogDate(Txt);
  LogNl;
end;


procedure LogSpace;
begin
  LogWrite(' ');
end;


procedure LogSpaces(N: Integer);
var
  I: Integer;
  Txt: AnsiString = '';
begin
  for I := 1 to N do
    Txt := Txt + ' ';
  LogWrite(Txt);
end;

procedure LogFlush;
begin
  Log.Flush;
end;


initialization
   Init;


finalization
   Fin;

end.

