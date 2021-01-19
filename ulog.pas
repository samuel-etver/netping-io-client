unit ulog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows;

type
  TLog = class
  private
    FFileStream: TFileStream;
    function IsOpened: Boolean;
  public
    constructor Create(FileName: AnsiString);
    destructor Destroy; override;
    procedure Close;
    procedure Flush;
    procedure Write(Txt: AnsiString);
    property Opened: Boolean read IsOpened;
  end;

implementation

constructor TLog.Create(FileName: AnsiString);
var
  FilePath: AnsiString;
  Flags: Word;
  Stream: TFileStream = nil;
begin
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


destructor TLog.Destroy;
begin
  Close;
  inherited;
end;


procedure TLog.Close;
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

function TLog.IsOpened: Boolean;
begin
  Result := Assigned(FFileStream);
end;


procedure TLog.Flush;
begin
  if Opened then
  begin
    FlushFileBuffers(FFileStream.Handle);
  end;
end;


procedure TLog.Write(Txt: AnsiString);
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


end.

