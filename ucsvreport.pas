unit ucsvreport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ureport, Windows, ucommon;

type
  TCsvReport = class(TReport)
  private
    FFileStream: TFileStream;
    function IsOpened: Boolean;
    procedure Write(Txt: AnsiString);
  public
    constructor Create(FileName: AnsiString);
    destructor Destroy; override;
    procedure Close;
    procedure Flush;
    procedure Append(DtTm: TDateTime; Channel: TChannel; Value: TIOValue; Duration: Longint);
    property Opened: Boolean read IsOpened;
  end;

implementation


constructor TCsvReport.Create(FileName: AnsiString);
const
  FileExt = '.csv';
var
  FilePath: AnsiString;
  Flags: Word;
  Stream: TFileStream = nil;
begin
  if not FileName.EndsWith(FileExt) then
    FileName := FileName + FileExt;
  FilePath := ExtractFilePath(FileName);
  if DirectoryExists(FilePath) then
  begin
    Flags := fmOpenReadWrite or fmShareDenyWrite;
    if not FileExists(FileName) then
      Flags := Flags or fmCreate;
      try
        Stream := TFileStream.Create(FileName, Flags);
        Stream.Position := Stream.Size;
      except
        if Assigned(Stream) then
          Stream.Free;
        Stream := nil;
      end;
  end;

  FFileStream := Stream;
end;


destructor TCsvReport.Destroy;
begin
  Close;
  inherited;
end;


procedure TCsvReport.Close;
var
  Stream: TFileStream;
begin
  if Opened then
  begin
    Stream := FFileStream;
    FFileStream := nil;
    Stream.Free;
  end;
end;


function TCsvReport.IsOpened: Boolean;
begin
  Result := Assigned(FFileStream);
end;


procedure TCsvReport.Flush;
begin
  if Opened then
  begin
    FlushFileBuffers(FFileStream.Handle);
  end;
end;


procedure TCsvReport.Write(Txt: AnsiString);
var
  N: Integer;
begin
  N := Length(Txt);
  if Opened and (N > 0) then
  begin
    try
      FFileStream.Write(PChar(Txt)[0], N);
    except
    end;
  end;
end;


procedure TCsvReport.Append(DtTm: TDateTime; Channel: TChannel; Value: TIOValue; Duration: Longint);
var
  Txt: AnsiString;
  Year, Mon, Day: Word;
  Hour, Min, Sec, MlSec: Word;

  function To02(Value: Word): AnsiString;
  begin
    if Value < 10
      then Result := '0' + IntToStr(Value)
      else Result := IntToStr(Value);
  end;

begin
  if Value <> iovHigh then
    Exit;

  DecodeDate(DtTm, Year, Mon, Day);
  DecodeTime(DtTm, Hour, Min, Sec, MlSec);

  Txt :=
    IntToStr(Year) + '-' +
    To02(Mon) + '-' +
    To02(Day) +
    ';' +
    To02(Hour) + ':' +
    To02(Min) + ':' +
    To02(Sec) +
    ';' +
    IntToStr(Channel) +
    ';' +
    FloatToStrF(0.001*Duration, ffFixed, 15, 0) +
    ';' + LineEnding;

  Write(Txt);
end;

end.

