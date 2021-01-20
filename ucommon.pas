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
  ReportFileName = 'report';

var
  Url: AnsiString;
  LogFilePath: AnsiString;
  ReportFilePath: AnsiString;
  RequestInterval: Real;

  procedure LoadConfig;
  procedure Startup;

  function IOValueToStr(Value: TIOValue): AnsiString;

  procedure LogWrite(Txt: AnsiString);
  procedure LogWriteLn(Txt: AnsiString);
  procedure LogNl;
  procedure LogDate;
  procedure LogDate(DtTm: TDateTime);
  procedure LogDate(DtTm: TDateTime; Txt: AnsiString; NewLine: Boolean = True);
  procedure LogDate(Txt: AnsiString; NewLine: Boolean = True);
  procedure LogChannelState(Channel: TChannel; Value: TIOValue; Duration: Longint);
  procedure LogChannelTrigger(Channel: TChannel; NewValue: TIOValue; OldValue: TIOValue; Duration: Longint);
  procedure LogSpace;
  procedure LogSpaces(N: Integer);
  procedure LogFlush;


implementation

uses
  Forms, ulog, ucsvreport;

const
  UrlDefault = 'http://192.168.0.100';
  LogFilePathDefault = 'Log';
  ReportFilePathDefault = 'Report';
  RequestIntervalDefault = 1.0;

var
  Log: TLog;
  Report: TCsvReport;


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
  ReportFilePath := GetStr('ReportFilePath', ReportFilePathDefault);
  RequestInterval := GetFloat('RequestInterval', RequestIntervalDefault);

  Lines.Free;
end;


procedure Startup;
begin
  Log := TLog.Create(LogFilePath + DirectorySeparator + LogFileName);
  Report := TCsvReport.Create(ReportFilePath + DirectorySeparator + ReportFileName);

  LogNl;
  LogWriteLn('---');
  LogDate('Программа запущена');
  LogNl;
end;


procedure Fin;
begin
  LogDate('Программа остановлена');
  if Assigned(Report) then
    Report.Free;
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


procedure LogDate(DtTm: TDateTime);
var
  Year, Mon, Day: Word;
  Hour, Min, Sec, MlSec: Word;
  Txt: AnsiString;

  function To02(Value: Word): AnsiString;
  begin
    if Value < 10
      then Result := '0' + IntToStr(Value)
      else Result := IntToStr(Value);
  end;

begin
  DecodeDate(DtTm, Year, Mon, Day);
  DecodeTime(DtTm, Hour, Min, Sec, MlSec);

  Txt := IntToStr(Year) + '-' +
         To02(Mon) + '-' +
         To02(Day) + ' ' +
         To02(Hour) + ':' +
         To02(Min) + ':' +
         To02(Sec);

  LogWrite(Txt);
end;


procedure LogDate;
begin
  LogDate(Now);
end;


procedure LogDate(DtTm: TDateTime; Txt: AnsiString; NewLine: Boolean);
begin
  LogDate(DtTm);
  LogSpace;
  LogWrite(Txt);
  if NewLine then
    LogNl;
end;


procedure LogDate(Txt: AnsiString; NewLine: Boolean);
begin
  LogDate(Now, Txt, NewLine);
end;


procedure LogChannelState(Channel: TChannel; Value: TIOValue; Duration: Longint);
var
  DtTm: TDateTime;
  Txt: AnsiString;
begin
  DtTm := Now;

  Txt := 'Состояние входа ' + IntToStr(Channel) + ': ' + IOValueToStr(Value);
  if Duration > 0 then
    Txt := Txt + '(' + FloatToStrF(0.001*Duration, ffFixed, 15, 1) + 'c)';
  LogDate(DtTm, Txt);
  LogNl;

  if Duration > 0 then
    Report.Append(DtTm, Value, Duration);
end;


procedure LogChannelTrigger(Channel: TChannel; NewValue: TIOValue; OldValue: TIOValue; Duration: Longint);
var
  DtTm: TDateTime;
  Txt: AnsiString = '';
  ChannelStr: AnsiString;
  DurationStr: AnsiString;

begin
  if OldValue = NewValue then
    Exit;

  DtTm := Now;

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

  LogDate(DtTm, Txt);
  LogNl;

  Report.Append(DtTm, OldValue, Duration);
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

