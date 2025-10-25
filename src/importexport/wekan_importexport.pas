unit wekan_importexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, sqldb, sqlite3conn, wekan_core, wekan_database, wekan_models;

type
  // Import/Export formats
  TImportExportFormat = (iefTrello, iefCSV, iefWeKan, iefJSON, iefXML);

  // Import/Export status
  TImportExportStatus = (iesPending, iesInProgress, iesCompleted, iesFailed, iesCancelled);

  // Import/Export job
  TImportExportJob = class
  private
    FId: string;
    FUserId: string;
    FType: string; // 'import' or 'export'
    FFormat: TImportExportFormat;
    FStatus: TImportExportStatus;
    FProgress: Integer;
    FErrorMessage: string;
    FSourceFile: string;
    FDestinationFile: string;
    FBoardId: string;
    FCreatedAt: string;
    FStartedAt: string;
    FCompletedAt: string;
    FTotalItems: Integer;
    FProcessedItems: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    
    property Id: string read FId write FId;
    property UserId: string read FUserId write FUserId;
    property Type: string read FType write FType;
    property Format: TImportExportFormat read FFormat write FFormat;
    property Status: TImportExportStatus read FStatus write FStatus;
    property Progress: Integer read FProgress write FProgress;
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
    property SourceFile: string read FSourceFile write FSourceFile;
    property DestinationFile: string read FDestinationFile write FDestinationFile;
    property BoardId: string read FBoardId write FBoardId;
    property CreatedAt: string read FCreatedAt write FCreatedAt;
    property StartedAt: string read FStartedAt write FStartedAt;
    property CompletedAt: string read FCompletedAt write FCompletedAt;
    property TotalItems: Integer read FTotalItems write FTotalItems;
    property ProcessedItems: Integer read FProcessedItems write FProcessedItems;
    
    function ToJSON: TJSONObject;
    procedure LoadFromJSON(const JSON: TJSONObject);
  end;

  // Import/Export manager
  TImportExportManager = class(TWeKanModel)
  private
    FBoardManager: TBoardManager;
    FListManager: TListManager;
    FCardManager: TCardManager;
    FUserManager: TUserManager;
    FFileManager: TFileManager;
  public
    constructor Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction;
      ABoardManager: TBoardManager; AListManager: TListManager; 
      ACardManager: TCardManager; AUserManager: TUserManager; AFileManager: TFileManager);
    destructor Destroy; override;
    
    // Job management
    function CreateJob(const UserId: string; const JobType: string; const Format: TImportExportFormat; const SourceFile: string; const BoardId: string = ''): TImportExportJob;
    function GetJob(const JobId: string): TImportExportJob;
    function GetJobsByUser(const UserId: string): TJSONArray;
    function UpdateJob(const Job: TImportExportJob): Boolean;
    function DeleteJob(const JobId: string): Boolean;
    
    // Import functions
    function ImportTrello(const FilePath: string; const UserId: string; const BoardId: string = ''): TImportExportJob;
    function ImportCSV(const FilePath: string; const UserId: string; const BoardId: string = ''): TImportExportJob;
    function ImportWeKan(const FilePath: string; const UserId: string; const BoardId: string = ''): TImportExportJob;
    function ImportJSON(const FilePath: string; const UserId: string; const BoardId: string = ''): TImportExportJob;
    
    // Export functions
    function ExportCSV(const BoardId: string; const UserId: string): TImportExportJob;
    function ExportWeKan(const BoardId: string; const UserId: string): TImportExportJob;
    function ExportJSON(const BoardId: string; const UserId: string): TImportExportJob;
    function ExportXML(const BoardId: string; const UserId: string): TImportExportJob;
    
    // Processing functions
    function ProcessJob(const JobId: string): Boolean;
    function CancelJob(const JobId: string): Boolean;
    function GetJobProgress(const JobId: string): TJSONObject;
  end;

  // Trello importer
  TTrelloImporter = class
  private
    FDatabase: TSQLite3Connection;
    FTransaction: TSQLTransaction;
    FBoardManager: TBoardManager;
    FListManager: TListManager;
    FCardManager: TCardManager;
  public
    constructor Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction;
      ABoardManager: TBoardManager; AListManager: TListManager; ACardManager: TCardManager);
    destructor Destroy; override;
    
    function ImportFromFile(const FilePath: string; const UserId: string; const BoardId: string = ''): Boolean;
    function ImportFromJSON(const JSON: TJSONObject; const UserId: string; const BoardId: string = ''): Boolean;
  end;

  // CSV importer/exporter
  TCSVImporterExporter = class
  private
    FDatabase: TSQLite3Connection;
    FTransaction: TSQLTransaction;
    FBoardManager: TBoardManager;
    FListManager: TListManager;
    FCardManager: TCardManager;
  public
    constructor Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction;
      ABoardManager: TBoardManager; AListManager: TListManager; ACardManager: TCardManager);
    destructor Destroy; override;
    
    function ImportFromFile(const FilePath: string; const UserId: string; const BoardId: string = ''): Boolean;
    function ExportToFile(const BoardId: string; const FilePath: string): Boolean;
  end;

  // WeKan importer/exporter
  TWeKanImporterExporter = class
  private
    FDatabase: TSQLite3Connection;
    FTransaction: TSQLTransaction;
    FBoardManager: TBoardManager;
    FListManager: TListManager;
    FCardManager: TCardManager;
  public
    constructor Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction;
      ABoardManager: TBoardManager; AListManager: TListManager; ACardManager: TCardManager);
    destructor Destroy; override;
    
    function ImportFromFile(const FilePath: string; const UserId: string; const BoardId: string = ''): Boolean;
    function ExportToFile(const BoardId: string; const FilePath: string): Boolean;
  end;

// Utility functions
function FormatToString(const Format: TImportExportFormat): string;
function StringToFormat(const Str: string): TImportExportFormat;
function StatusToString(const Status: TImportExportStatus): string;
function StringToStatus(const Str: string): TImportExportStatus;
function ParseCSVLine(const Line: string): TStringArray;
function EscapeCSVField(const Field: string): string;
function UnescapeCSVField(const Field: string): string;

implementation

uses
  strutils, dateutils, csv, wekan_utils;

// TImportExportJob implementation
constructor TImportExportJob.Create;
begin
  inherited Create;
  FId := GenerateId;
  FStatus := iesPending;
  FProgress := 0;
  FCreatedAt := GetCurrentTimestamp;
  FTotalItems := 0;
  FProcessedItems := 0;
end;

destructor TImportExportJob.Destroy;
begin
  inherited Destroy;
end;

function TImportExportJob.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('_id', FId);
  Result.Add('userId', FUserId);
  Result.Add('type', FType);
  Result.Add('format', FormatToString(FFormat));
  Result.Add('status', StatusToString(FStatus));
  Result.Add('progress', FProgress);
  Result.Add('errorMessage', FErrorMessage);
  Result.Add('sourceFile', FSourceFile);
  Result.Add('destinationFile', FDestinationFile);
  Result.Add('boardId', FBoardId);
  Result.Add('createdAt', FCreatedAt);
  Result.Add('startedAt', FStartedAt);
  Result.Add('completedAt', FCompletedAt);
  Result.Add('totalItems', FTotalItems);
  Result.Add('processedItems', FProcessedItems);
end;

procedure TImportExportJob.LoadFromJSON(const JSON: TJSONObject);
begin
  if Assigned(JSON) then
  begin
    FId := JSON.Get('_id', '');
    FUserId := JSON.Get('userId', '');
    FType := JSON.Get('type', '');
    FFormat := StringToFormat(JSON.Get('format', ''));
    FStatus := StringToStatus(JSON.Get('status', ''));
    FProgress := JSON.Get('progress', 0);
    FErrorMessage := JSON.Get('errorMessage', '');
    FSourceFile := JSON.Get('sourceFile', '');
    FDestinationFile := JSON.Get('destinationFile', '');
    FBoardId := JSON.Get('boardId', '');
    FCreatedAt := JSON.Get('createdAt', '');
    FStartedAt := JSON.Get('startedAt', '');
    FCompletedAt := JSON.Get('completedAt', '');
    FTotalItems := JSON.Get('totalItems', 0);
    FProcessedItems := JSON.Get('processedItems', 0);
  end;
end;

// TImportExportManager implementation
constructor TImportExportManager.Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction;
  ABoardManager: TBoardManager; AListManager: TListManager; 
  ACardManager: TCardManager; AUserManager: TUserManager; AFileManager: TFileManager);
begin
  inherited Create(ADatabase, ATransaction);
  FBoardManager := ABoardManager;
  FListManager := AListManager;
  FCardManager := ACardManager;
  FUserManager := AUserManager;
  FFileManager := AFileManager;
end;

destructor TImportExportManager.Destroy;
begin
  inherited Destroy;
end;

function TImportExportManager.CreateJob(const UserId: string; const JobType: string; const Format: TImportExportFormat; const SourceFile: string; const BoardId: string): TImportExportJob;
var
  Job: TImportExportJob;
  SQL: string;
begin
  Result := nil;
  
  Job := TImportExportJob.Create;
  try
    Job.UserId := UserId;
    Job.Type := JobType;
    Job.Format := Format;
    Job.SourceFile := SourceFile;
    Job.BoardId := BoardId;
    
    SQL := 'INSERT INTO importExportHistory (_id, userId, type, source, destination, status, progress, createdAt) ' +
           'VALUES (:id, :userId, :type, :source, :destination, :status, :progress, :createdAt)';
    
    FQuery.Close;
    FQuery.SQL.Text := SQL;
    FQuery.ParamByName('id').AsString := Job.Id;
    FQuery.ParamByName('userId').AsString := Job.UserId;
    FQuery.ParamByName('type').AsString := Job.Type;
    FQuery.ParamByName('source').AsString := Job.SourceFile;
    FQuery.ParamByName('destination').AsString := Job.DestinationFile;
    FQuery.ParamByName('status').AsString := StatusToString(Job.Status);
    FQuery.ParamByName('progress').AsInteger := Job.Progress;
    FQuery.ParamByName('createdAt').AsString := Job.CreatedAt;
    FQuery.ExecSQL;
    FTransaction.Commit;
    
    Result := Job;
  except
    on E: Exception do
    begin
      Job.Free;
      Result := nil;
      LogError('Create job error: ' + E.Message);
    end;
  end;
end;

function TImportExportManager.GetJob(const JobId: string): TImportExportJob;
var
  Job: TImportExportJob;
  SQL: string;
begin
  Result := nil;
  
  SQL := 'SELECT * FROM importExportHistory WHERE _id = :id';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := JobId;
  FQuery.Open;
  
  if not FQuery.Eof then
  begin
    Job := TImportExportJob.Create;
    try
      Job.Id := FQuery.FieldByName('_id').AsString;
      Job.UserId := FQuery.FieldByName('userId').AsString;
      Job.Type := FQuery.FieldByName('type').AsString;
      Job.SourceFile := FQuery.FieldByName('source').AsString;
      Job.DestinationFile := FQuery.FieldByName('destination').AsString;
      Job.Status := StringToStatus(FQuery.FieldByName('status').AsString);
      Job.Progress := FQuery.FieldByName('progress').AsInteger;
      Job.ErrorMessage := FQuery.FieldByName('errorMessage').AsString;
      Job.CreatedAt := FQuery.FieldByName('createdAt').AsString;
      Job.CompletedAt := FQuery.FieldByName('completedAt').AsString;
      
      Result := Job;
    except
      on E: Exception do
      begin
        Job.Free;
        Result := nil;
      end;
    end;
  end;
end;

function TImportExportManager.GetJobsByUser(const UserId: string): TJSONArray;
var
  Jobs: TJSONArray;
  SQL: string;
begin
  Jobs := TJSONArray.Create;
  
  SQL := 'SELECT * FROM importExportHistory WHERE userId = :userId ORDER BY createdAt DESC';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('userId').AsString := UserId;
  FQuery.Open;
  
  while not FQuery.Eof do
  begin
    Jobs.Add(TJSONObject.Create([
      '_id', FQuery.FieldByName('_id').AsString,
      'userId', FQuery.FieldByName('userId').AsString,
      'type', FQuery.FieldByName('type').AsString,
      'source', FQuery.FieldByName('source').AsString,
      'destination', FQuery.FieldByName('destination').AsString,
      'status', FQuery.FieldByName('status').AsString,
      'progress', FQuery.FieldByName('progress').AsInteger,
      'errorMessage', FQuery.FieldByName('errorMessage').AsString,
      'createdAt', FQuery.FieldByName('createdAt').AsString,
      'completedAt', FQuery.FieldByName('completedAt').AsString
    ]));
    FQuery.Next;
  end;
  
  Result := Jobs;
end;

function TImportExportManager.UpdateJob(const Job: TImportExportJob): Boolean;
var
  SQL: string;
begin
  Result := False;
  
  if not Assigned(Job) then
    Exit;
    
  SQL := 'UPDATE importExportHistory SET status = :status, progress = :progress, errorMessage = :errorMessage, ' +
         'destination = :destination, completedAt = :completedAt WHERE _id = :id';
  
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := Job.Id;
  FQuery.ParamByName('status').AsString := StatusToString(Job.Status);
  FQuery.ParamByName('progress').AsInteger := Job.Progress;
  FQuery.ParamByName('errorMessage').AsString := Job.ErrorMessage;
  FQuery.ParamByName('destination').AsString := Job.DestinationFile;
  FQuery.ParamByName('completedAt').AsString := Job.CompletedAt;
  
  try
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      LogError('Update job error: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TImportExportManager.DeleteJob(const JobId: string): Boolean;
var
  SQL: string;
begin
  Result := False;
  
  SQL := 'DELETE FROM importExportHistory WHERE _id = :id';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := JobId;
  
  try
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      LogError('Delete job error: ' + E.Message);
      Result := False;
    end;
  end;
end;

// Import functions
function TImportExportManager.ImportTrello(const FilePath: string; const UserId: string; const BoardId: string): TImportExportJob;
var
  Job: TImportExportJob;
  Importer: TTrelloImporter;
begin
  Result := nil;
  
  Job := CreateJob(UserId, 'import', iefTrello, FilePath, BoardId);
  if not Assigned(Job) then
    Exit;
    
  try
    Job.Status := iesInProgress;
    Job.StartedAt := GetCurrentTimestamp;
    UpdateJob(Job);
    
    Importer := TTrelloImporter.Create(FDatabase, FTransaction, FBoardManager, FListManager, FCardManager);
    try
      if Importer.ImportFromFile(FilePath, UserId, BoardId) then
      begin
        Job.Status := iesCompleted;
        Job.Progress := 100;
        Job.CompletedAt := GetCurrentTimestamp;
      end
      else
      begin
        Job.Status := iesFailed;
        Job.ErrorMessage := 'Import failed';
      end;
    finally
      Importer.Free;
    end;
    
    UpdateJob(Job);
    Result := Job;
  except
    on E: Exception do
    begin
      Job.Status := iesFailed;
      Job.ErrorMessage := E.Message;
      UpdateJob(Job);
      Result := Job;
    end;
  end;
end;

function TImportExportManager.ImportCSV(const FilePath: string; const UserId: string; const BoardId: string): TImportExportJob;
var
  Job: TImportExportJob;
  Importer: TCSVImporterExporter;
begin
  Result := nil;
  
  Job := CreateJob(UserId, 'import', iefCSV, FilePath, BoardId);
  if not Assigned(Job) then
    Exit;
    
  try
    Job.Status := iesInProgress;
    Job.StartedAt := GetCurrentTimestamp;
    UpdateJob(Job);
    
    Importer := TCSVImporterExporter.Create(FDatabase, FTransaction, FBoardManager, FListManager, FCardManager);
    try
      if Importer.ImportFromFile(FilePath, UserId, BoardId) then
      begin
        Job.Status := iesCompleted;
        Job.Progress := 100;
        Job.CompletedAt := GetCurrentTimestamp;
      end
      else
      begin
        Job.Status := iesFailed;
        Job.ErrorMessage := 'Import failed';
      end;
    finally
      Importer.Free;
    end;
    
    UpdateJob(Job);
    Result := Job;
  except
    on E: Exception do
    begin
      Job.Status := iesFailed;
      Job.ErrorMessage := E.Message;
      UpdateJob(Job);
      Result := Job;
    end;
  end;
end;

function TImportExportManager.ImportWeKan(const FilePath: string; const UserId: string; const BoardId: string): TImportExportJob;
var
  Job: TImportExportJob;
  Importer: TWeKanImporterExporter;
begin
  Result := nil;
  
  Job := CreateJob(UserId, 'import', iefWeKan, FilePath, BoardId);
  if not Assigned(Job) then
    Exit;
    
  try
    Job.Status := iesInProgress;
    Job.StartedAt := GetCurrentTimestamp;
    UpdateJob(Job);
    
    Importer := TWeKanImporterExporter.Create(FDatabase, FTransaction, FBoardManager, FListManager, FCardManager);
    try
      if Importer.ImportFromFile(FilePath, UserId, BoardId) then
      begin
        Job.Status := iesCompleted;
        Job.Progress := 100;
        Job.CompletedAt := GetCurrentTimestamp;
      end
      else
      begin
        Job.Status := iesFailed;
        Job.ErrorMessage := 'Import failed';
      end;
    finally
      Importer.Free;
    end;
    
    UpdateJob(Job);
    Result := Job;
  except
    on E: Exception do
    begin
      Job.Status := iesFailed;
      Job.ErrorMessage := E.Message;
      UpdateJob(Job);
      Result := Job;
    end;
  end;
end;

function TImportExportManager.ImportJSON(const FilePath: string; const UserId: string; const BoardId: string): TImportExportJob;
var
  Job: TImportExportJob;
  JSONFile: TStringList;
  Parser: TJSONParser;
  JSON: TJSONObject;
  Importer: TWeKanImporterExporter;
begin
  Result := nil;
  
  Job := CreateJob(UserId, 'import', iefJSON, FilePath, BoardId);
  if not Assigned(Job) then
    Exit;
    
  try
    Job.Status := iesInProgress;
    Job.StartedAt := GetCurrentTimestamp;
    UpdateJob(Job);
    
    JSONFile := TStringList.Create;
    try
      JSONFile.LoadFromFile(FilePath);
      Parser := TJSONParser.Create(JSONFile.Text);
      try
        JSON := Parser.Parse as TJSONObject;
        try
          Importer := TWeKanImporterExporter.Create(FDatabase, FTransaction, FBoardManager, FListManager, FCardManager);
          try
            if Importer.ImportFromFile(FilePath, UserId, BoardId) then
            begin
              Job.Status := iesCompleted;
              Job.Progress := 100;
              Job.CompletedAt := GetCurrentTimestamp;
            end
            else
            begin
              Job.Status := iesFailed;
              Job.ErrorMessage := 'Import failed';
            end;
          finally
            Importer.Free;
          end;
        finally
          JSON.Free;
        end;
      finally
        Parser.Free;
      end;
    finally
      JSONFile.Free;
    end;
    
    UpdateJob(Job);
    Result := Job;
  except
    on E: Exception do
    begin
      Job.Status := iesFailed;
      Job.ErrorMessage := E.Message;
      UpdateJob(Job);
      Result := Job;
    end;
  end;
end;

// Export functions
function TImportExportManager.ExportCSV(const BoardId: string; const UserId: string): TImportExportJob;
var
  Job: TImportExportJob;
  Exporter: TCSVImporterExporter;
  FilePath: string;
begin
  Result := nil;
  
  FilePath := 'exports/' + GenerateId + '.csv';
  Job := CreateJob(UserId, 'export', iefCSV, '', BoardId);
  if not Assigned(Job) then
    Exit;
    
  Job.DestinationFile := FilePath;
  
  try
    Job.Status := iesInProgress;
    Job.StartedAt := GetCurrentTimestamp;
    UpdateJob(Job);
    
    Exporter := TCSVImporterExporter.Create(FDatabase, FTransaction, FBoardManager, FListManager, FCardManager);
    try
      if Exporter.ExportToFile(BoardId, FilePath) then
      begin
        Job.Status := iesCompleted;
        Job.Progress := 100;
        Job.CompletedAt := GetCurrentTimestamp;
      end
      else
      begin
        Job.Status := iesFailed;
        Job.ErrorMessage := 'Export failed';
      end;
    finally
      Exporter.Free;
    end;
    
    UpdateJob(Job);
    Result := Job;
  except
    on E: Exception do
    begin
      Job.Status := iesFailed;
      Job.ErrorMessage := E.Message;
      UpdateJob(Job);
      Result := Job;
    end;
  end;
end;

function TImportExportManager.ExportWeKan(const BoardId: string; const UserId: string): TImportExportJob;
var
  Job: TImportExportJob;
  Exporter: TWeKanImporterExporter;
  FilePath: string;
begin
  Result := nil;
  
  FilePath := 'exports/' + GenerateId + '.wekan';
  Job := CreateJob(UserId, 'export', iefWeKan, '', BoardId);
  if not Assigned(Job) then
    Exit;
    
  Job.DestinationFile := FilePath;
  
  try
    Job.Status := iesInProgress;
    Job.StartedAt := GetCurrentTimestamp;
    UpdateJob(Job);
    
    Exporter := TWeKanImporterExporter.Create(FDatabase, FTransaction, FBoardManager, FListManager, FCardManager);
    try
      if Exporter.ExportToFile(BoardId, FilePath) then
      begin
        Job.Status := iesCompleted;
        Job.Progress := 100;
        Job.CompletedAt := GetCurrentTimestamp;
      end
      else
      begin
        Job.Status := iesFailed;
        Job.ErrorMessage := 'Export failed';
      end;
    finally
      Exporter.Free;
    end;
    
    UpdateJob(Job);
    Result := Job;
  except
    on E: Exception do
    begin
      Job.Status := iesFailed;
      Job.ErrorMessage := E.Message;
      UpdateJob(Job);
      Result := Job;
    end;
  end;
end;

function TImportExportManager.ExportJSON(const BoardId: string; const UserId: string): TImportExportJob;
var
  Job: TImportExportJob;
  Exporter: TWeKanImporterExporter;
  FilePath: string;
begin
  Result := nil;
  
  FilePath := 'exports/' + GenerateId + '.json';
  Job := CreateJob(UserId, 'export', iefJSON, '', BoardId);
  if not Assigned(Job) then
    Exit;
    
  Job.DestinationFile := FilePath;
  
  try
    Job.Status := iesInProgress;
    Job.StartedAt := GetCurrentTimestamp;
    UpdateJob(Job);
    
    Exporter := TWeKanImporterExporter.Create(FDatabase, FTransaction, FBoardManager, FListManager, FCardManager);
    try
      if Exporter.ExportToFile(BoardId, FilePath) then
      begin
        Job.Status := iesCompleted;
        Job.Progress := 100;
        Job.CompletedAt := GetCurrentTimestamp;
      end
      else
      begin
        Job.Status := iesFailed;
        Job.ErrorMessage := 'Export failed';
      end;
    finally
      Exporter.Free;
    end;
    
    UpdateJob(Job);
    Result := Job;
  except
    on E: Exception do
    begin
      Job.Status := iesFailed;
      Job.ErrorMessage := E.Message;
      UpdateJob(Job);
      Result := Job;
    end;
  end;
end;

function TImportExportManager.ExportXML(const BoardId: string; const UserId: string): TImportExportJob;
var
  Job: TImportExportJob;
  FilePath: string;
  XML: TStringList;
  Board: TBoard;
  Lists: TJSONArray;
  Cards: TJSONArray;
  i, j: Integer;
  List: TJSONObject;
  Card: TJSONObject;
begin
  Result := nil;
  
  FilePath := 'exports/' + GenerateId + '.xml';
  Job := CreateJob(UserId, 'export', iefXML, '', BoardId);
  if not Assigned(Job) then
    Exit;
    
  Job.DestinationFile := FilePath;
  
  try
    Job.Status := iesInProgress;
    Job.StartedAt := GetCurrentTimestamp;
    UpdateJob(Job);
    
    XML := TStringList.Create;
    try
      XML.Add('<?xml version="1.0" encoding="UTF-8"?>');
      XML.Add('<wekan>');
      XML.Add('  <board id="' + BoardId + '">');
      
      // Get board data
      Board := FBoardManager.GetBoardById(BoardId);
      if Assigned(Board) then
      begin
        try
          XML.Add('    <title>' + EscapeCSVField(Board.Title) + '</title>');
          XML.Add('    <description>' + EscapeCSVField(Board.Description) + '</description>');
          XML.Add('    <createdAt>' + Board.CreatedAt + '</createdAt>');
          XML.Add('    <modifiedAt>' + Board.ModifiedAt + '</modifiedAt>');
        finally
          Board.Free;
        end;
      end;
      
      // Get lists
      Lists := FListManager.GetListsByBoard(BoardId);
      try
        for i := 0 to Lists.Count - 1 do
        begin
          List := Lists.Objects[i];
          XML.Add('    <list id="' + List.Get('_id', '') + '">');
          XML.Add('      <title>' + EscapeCSVField(List.Get('title', '')) + '</title>');
          XML.Add('      <sort>' + IntToStr(List.Get('sort', 0)) + '</sort>');
          
          // Get cards for this list
          Cards := FCardManager.GetCardsByList(List.Get('_id', ''));
          try
            for j := 0 to Cards.Count - 1 do
            begin
              Card := Cards.Objects[j];
              XML.Add('      <card id="' + Card.Get('_id', '') + '">');
              XML.Add('        <title>' + EscapeCSVField(Card.Get('title', '')) + '</title>');
              XML.Add('        <description>' + EscapeCSVField(Card.Get('description', '')) + '</description>');
              XML.Add('        <sort>' + IntToStr(Card.Get('sort', 0)) + '</sort>');
              XML.Add('        <createdAt>' + Card.Get('createdAt', '') + '</createdAt>');
              XML.Add('        <modifiedAt>' + Card.Get('modifiedAt', '') + '</modifiedAt>');
              XML.Add('      </card>');
            end;
          finally
            Cards.Free;
          end;
          
          XML.Add('    </list>');
        end;
      finally
        Lists.Free;
      end;
      
      XML.Add('  </board>');
      XML.Add('</wekan>');
      
      // Save to file
      XML.SaveToFile(FilePath);
      
      Job.Status := iesCompleted;
      Job.Progress := 100;
      Job.CompletedAt := GetCurrentTimestamp;
    finally
      XML.Free;
    end;
    
    UpdateJob(Job);
    Result := Job;
  except
    on E: Exception do
    begin
      Job.Status := iesFailed;
      Job.ErrorMessage := E.Message;
      UpdateJob(Job);
      Result := Job;
    end;
  end;
end;

// Processing functions
function TImportExportManager.ProcessJob(const JobId: string): Boolean;
var
  Job: TImportExportJob;
begin
  Result := False;
  
  Job := GetJob(JobId);
  if not Assigned(Job) then
    Exit;
    
  try
    case Job.Format of
      iefTrello:
        begin
          if Job.Type = 'import' then
            Result := Assigned(ImportTrello(Job.SourceFile, Job.UserId, Job.BoardId));
        end;
      iefCSV:
        begin
          if Job.Type = 'import' then
            Result := Assigned(ImportCSV(Job.SourceFile, Job.UserId, Job.BoardId))
          else if Job.Type = 'export' then
            Result := Assigned(ExportCSV(Job.BoardId, Job.UserId));
        end;
      iefWeKan:
        begin
          if Job.Type = 'import' then
            Result := Assigned(ImportWeKan(Job.SourceFile, Job.UserId, Job.BoardId))
          else if Job.Type = 'export' then
            Result := Assigned(ExportWeKan(Job.BoardId, Job.UserId));
        end;
      iefJSON:
        begin
          if Job.Type = 'import' then
            Result := Assigned(ImportJSON(Job.SourceFile, Job.UserId, Job.BoardId))
          else if Job.Type = 'export' then
            Result := Assigned(ExportJSON(Job.BoardId, Job.UserId));
        end;
      iefXML:
        begin
          if Job.Type = 'export' then
            Result := Assigned(ExportXML(Job.BoardId, Job.UserId));
        end;
    end;
  finally
    Job.Free;
  end;
end;

function TImportExportManager.CancelJob(const JobId: string): Boolean;
var
  Job: TImportExportJob;
begin
  Result := False;
  
  Job := GetJob(JobId);
  if not Assigned(Job) then
    Exit;
    
  try
    if Job.Status = iesInProgress then
    begin
      Job.Status := iesCancelled;
      Job.CompletedAt := GetCurrentTimestamp;
      Result := UpdateJob(Job);
    end;
  finally
    Job.Free;
  end;
end;

function TImportExportManager.GetJobProgress(const JobId: string): TJSONObject;
var
  Job: TImportExportJob;
begin
  Result := nil;
  
  Job := GetJob(JobId);
  if Assigned(Job) then
  begin
    try
      Result := TJSONObject.Create([
        'jobId', Job.Id,
        'status', StatusToString(Job.Status),
        'progress', Job.Progress,
        'totalItems', Job.TotalItems,
        'processedItems', Job.ProcessedItems,
        'errorMessage', Job.ErrorMessage,
        'createdAt', Job.CreatedAt,
        'startedAt', Job.StartedAt,
        'completedAt', Job.CompletedAt
      ]);
    finally
      Job.Free;
    end;
  end;
end;

// TTrelloImporter implementation
constructor TTrelloImporter.Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction;
  ABoardManager: TBoardManager; AListManager: TListManager; ACardManager: TCardManager);
begin
  inherited Create;
  FDatabase := ADatabase;
  FTransaction := ATransaction;
  FBoardManager := ABoardManager;
  FListManager := AListManager;
  FCardManager := ACardManager;
end;

destructor TTrelloImporter.Destroy;
begin
  inherited Destroy;
end;

function TTrelloImporter.ImportFromFile(const FilePath: string; const UserId: string; const BoardId: string): Boolean;
var
  JSONFile: TStringList;
  Parser: TJSONParser;
  JSON: TJSONObject;
begin
  Result := False;
  
  if not FileExists(FilePath) then
    Exit;
    
  JSONFile := TStringList.Create;
  try
    JSONFile.LoadFromFile(FilePath);
    Parser := TJSONParser.Create(JSONFile.Text);
    try
      JSON := Parser.Parse as TJSONObject;
      try
        Result := ImportFromJSON(JSON, UserId, BoardId);
      finally
        JSON.Free;
      end;
    finally
      Parser.Free;
    end;
  finally
    JSONFile.Free;
  end;
end;

function TTrelloImporter.ImportFromJSON(const JSON: TJSONObject; const UserId: string; const BoardId: string): Boolean;
var
  Board: TBoard;
  Lists: TJSONArray;
  Cards: TJSONArray;
  i, j: Integer;
  List: TJSONObject;
  Card: TJSONObject;
  NewBoard: TBoard;
  NewList: TList;
  NewCard: TCard;
  ListId: string;
begin
  Result := False;
  
  if not Assigned(JSON) then
    Exit;
    
  try
    // Create board
    NewBoard := FBoardManager.CreateBoard(
      JSON.Get('name', 'Imported Board'),
      JSON.Get('desc', ''),
      UserId
    );
    
    if not Assigned(NewBoard) then
      Exit;
      
    try
      // Import lists
      if JSON.Find('lists') <> nil then
      begin
        Lists := JSON.Arrays['lists'];
        for i := 0 to Lists.Count - 1 do
        begin
          List := Lists.Objects[i];
          if not List.Get('closed', False) then
          begin
            NewList := FListManager.CreateList(List.Get('name', ''), NewBoard.Id);
            if Assigned(NewList) then
            begin
              try
                ListId := NewList.Id;
                
                // Import cards for this list
                if JSON.Find('cards') <> nil then
                begin
                  Cards := JSON.Arrays['cards'];
                  for j := 0 to Cards.Count - 1 do
                  begin
                    Card := Cards.Objects[j];
                    if (Card.Get('idList', '') = List.Get('id', '')) and not Card.Get('closed', False) then
                    begin
                      NewCard := FCardManager.CreateCard(
                        Card.Get('name', ''),
                        ListId,
                        NewBoard.Id
                      );
                      if Assigned(NewCard) then
                      begin
                        try
                          NewCard.Description := Card.Get('desc', '');
                          NewCard.DueAt := Card.Get('due', '');
                          FCardManager.UpdateCard(NewCard);
                        finally
                          NewCard.Free;
                        end;
                      end;
                    end;
                  end;
                end;
              finally
                NewList.Free;
              end;
            end;
          end;
        end;
      end;
      
      Result := True;
    finally
      NewBoard.Free;
    end;
  except
    on E: Exception do
    begin
      LogError('Trello import error: ' + E.Message);
      Result := False;
    end;
  end;
end;

// TCSVImporterExporter implementation
constructor TCSVImporterExporter.Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction;
  ABoardManager: TBoardManager; AListManager: TListManager; ACardManager: TCardManager);
begin
  inherited Create;
  FDatabase := ADatabase;
  FTransaction := ATransaction;
  FBoardManager := ABoardManager;
  FListManager := AListManager;
  FCardManager := ACardManager;
end;

destructor TCSVImporterExporter.Destroy;
begin
  inherited Destroy;
end;

function TCSVImporterExporter.ImportFromFile(const FilePath: string; const UserId: string; const BoardId: string): Boolean;
var
  CSVFile: TStringList;
  i: Integer;
  Line: string;
  Fields: TStringArray;
  Board: TBoard;
  List: TList;
  Card: TCard;
  ListId: string;
begin
  Result := False;
  
  if not FileExists(FilePath) then
    Exit;
    
  CSVFile := TStringList.Create;
  try
    CSVFile.LoadFromFile(FilePath);
    
    if CSVFile.Count < 2 then
      Exit;
      
    // Create board
    Board := FBoardManager.CreateBoard('Imported Board', 'Imported from CSV', UserId);
    if not Assigned(Board) then
      Exit;
      
    try
      // Create default list
      List := FListManager.CreateList('Imported Cards', Board.Id);
      if not Assigned(List) then
        Exit;
        
      try
        ListId := List.Id;
        
        // Skip header row
        for i := 1 to CSVFile.Count - 1 do
        begin
          Line := CSVFile[i];
          Fields := ParseCSVLine(Line);
          
          if Length(Fields) >= 1 then
          begin
            Card := FCardManager.CreateCard(Fields[0], ListId, Board.Id);
            if Assigned(Card) then
            begin
              try
                if Length(Fields) >= 2 then
                  Card.Description := Fields[1];
                if Length(Fields) >= 3 then
                  Card.DueAt := Fields[2];
                FCardManager.UpdateCard(Card);
              finally
                Card.Free;
              end;
            end;
          end;
        end;
        
        Result := True;
      finally
        List.Free;
      end;
    finally
      Board.Free;
    end;
  finally
    CSVFile.Free;
  end;
end;

function TCSVImporterExporter.ExportToFile(const BoardId: string; const FilePath: string): Boolean;
var
  CSVFile: TStringList;
  Board: TBoard;
  Lists: TJSONArray;
  Cards: TJSONArray;
  i, j: Integer;
  List: TJSONObject;
  Card: TJSONObject;
  Line: string;
begin
  Result := False;
  
  CSVFile := TStringList.Create;
  try
    // Add header
    CSVFile.Add('Title,Description,Due Date,List,Board');
    
    // Get board
    Board := FBoardManager.GetBoardById(BoardId);
    if not Assigned(Board) then
      Exit;
      
    try
      // Get lists
      Lists := FListManager.GetListsByBoard(BoardId);
      try
        for i := 0 to Lists.Count - 1 do
        begin
          List := Lists.Objects[i];
          
          // Get cards for this list
          Cards := FCardManager.GetCardsByList(List.Get('_id', ''));
          try
            for j := 0 to Cards.Count - 1 do
            begin
              Card := Cards.Objects[j];
              Line := EscapeCSVField(Card.Get('title', '')) + ',' +
                      EscapeCSVField(Card.Get('description', '')) + ',' +
                      EscapeCSVField(Card.Get('dueAt', '')) + ',' +
                      EscapeCSVField(List.Get('title', '')) + ',' +
                      EscapeCSVField(Board.Title);
              CSVFile.Add(Line);
            end;
          finally
            Cards.Free;
          end;
        end;
      finally
        Lists.Free;
      end;
      
      // Save to file
      CSVFile.SaveToFile(FilePath);
      Result := True;
    finally
      Board.Free;
    end;
  finally
    CSVFile.Free;
  end;
end;

// TWeKanImporterExporter implementation
constructor TWeKanImporterExporter.Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction;
  ABoardManager: TBoardManager; AListManager: TListManager; ACardManager: TCardManager);
begin
  inherited Create;
  FDatabase := ADatabase;
  FTransaction := ATransaction;
  FBoardManager := ABoardManager;
  FListManager := AListManager;
  FCardManager := ACardManager;
end;

destructor TWeKanImporterExporter.Destroy;
begin
  inherited Destroy;
end;

function TWeKanImporterExporter.ImportFromFile(const FilePath: string; const UserId: string; const BoardId: string): Boolean;
var
  JSONFile: TStringList;
  Parser: TJSONParser;
  JSON: TJSONObject;
  Boards: TJSONArray;
  i: Integer;
  Board: TJSONObject;
  NewBoard: TBoard;
begin
  Result := False;
  
  if not FileExists(FilePath) then
    Exit;
    
  JSONFile := TStringList.Create;
  try
    JSONFile.LoadFromFile(FilePath);
    Parser := TJSONParser.Create(JSONFile.Text);
    try
      JSON := Parser.Parse as TJSONObject;
      try
        if JSON.Find('boards') <> nil then
        begin
          Boards := JSON.Arrays['boards'];
          for i := 0 to Boards.Count - 1 do
          begin
            Board := Boards.Objects[i];
            NewBoard := FBoardManager.CreateBoard(
              Board.Get('title', 'Imported Board'),
              Board.Get('description', ''),
              UserId
            );
            if Assigned(NewBoard) then
              NewBoard.Free;
          end;
        end;
        Result := True;
      finally
        JSON.Free;
      end;
    finally
      Parser.Free;
    end;
  finally
    JSONFile.Free;
  end;
end;

function TWeKanImporterExporter.ExportToFile(const BoardId: string; const FilePath: string): Boolean;
var
  JSONFile: TStringList;
  Board: TBoard;
  Lists: TJSONArray;
  Cards: TJSONArray;
  ExportData: TJSONObject;
  BoardData: TJSONObject;
  i, j: Integer;
  List: TJSONObject;
  Card: TJSONObject;
begin
  Result := False;
  
  JSONFile := TStringList.Create;
  try
    ExportData := TJSONObject.Create;
    try
      BoardData := TJSONObject.Create;
      try
        // Get board
        Board := FBoardManager.GetBoardById(BoardId);
        if not Assigned(Board) then
          Exit;
          
        try
          BoardData.Add('_id', Board.Id);
          BoardData.Add('title', Board.Title);
          BoardData.Add('description', Board.Description);
          BoardData.Add('createdAt', Board.CreatedAt);
          BoardData.Add('modifiedAt', Board.ModifiedAt);
          BoardData.Add('permission', Board.Permission);
          BoardData.Add('color', Board.Color);
          
          // Get lists
          Lists := FListManager.GetListsByBoard(BoardId);
          try
            BoardData.Add('lists', Lists);
            
            // Get cards for each list
            for i := 0 to Lists.Count - 1 do
            begin
              List := Lists.Objects[i];
              Cards := FCardManager.GetCardsByList(List.Get('_id', ''));
              List.Add('cards', Cards);
            end;
          finally
            Lists.Free;
          end;
          
          ExportData.Add('boards', TJSONArray.Create([BoardData]));
          ExportData.Add('exportedAt', GetCurrentTimestamp);
          ExportData.Add('version', '1.0');
          
          JSONFile.Text := ExportData.AsJSON;
          JSONFile.SaveToFile(FilePath);
          Result := True;
        finally
          Board.Free;
        end;
      finally
        BoardData.Free;
      end;
    finally
      ExportData.Free;
    end;
  finally
    JSONFile.Free;
  end;
end;

// Utility functions
function FormatToString(const Format: TImportExportFormat): string;
begin
  case Format of
    iefTrello: Result := 'trello';
    iefCSV: Result := 'csv';
    iefWeKan: Result := 'wekan';
    iefJSON: Result := 'json';
    iefXML: Result := 'xml';
  end;
end;

function StringToFormat(const Str: string): TImportExportFormat;
begin
  case LowerCase(Str) of
    'trello': Result := iefTrello;
    'csv': Result := iefCSV;
    'wekan': Result := iefWeKan;
    'json': Result := iefJSON;
    'xml': Result := iefXML;
    else
      Result := iefJSON;
  end;
end;

function StatusToString(const Status: TImportExportStatus): string;
begin
  case Status of
    iesPending: Result := 'pending';
    iesInProgress: Result := 'in_progress';
    iesCompleted: Result := 'completed';
    iesFailed: Result := 'failed';
    iesCancelled: Result := 'cancelled';
  end;
end;

function StringToStatus(const Str: string): TImportExportStatus;
begin
  case LowerCase(Str) of
    'pending': Result := iesPending;
    'in_progress': Result := iesInProgress;
    'completed': Result := iesCompleted;
    'failed': Result := iesFailed;
    'cancelled': Result := iesCancelled;
    else
      Result := iesPending;
  end;
end;

function ParseCSVLine(const Line: string): TStringArray;
var
  List: TStringList;
  i: Integer;
begin
  List := TStringList.Create;
  try
    List.Delimiter := ',';
    List.DelimitedText := Line;
    
    SetLength(Result, List.Count);
    for i := 0 to List.Count - 1 do
    begin
      Result[i] := UnescapeCSVField(Trim(List[i]));
    end;
  finally
    List.Free;
  end;
end;

function EscapeCSVField(const Field: string): string;
begin
  Result := Field;
  Result := StringReplace(Result, '"', '""', [rfReplaceAll]);
  if (Pos(',', Result) > 0) or (Pos('"', Result) > 0) or (Pos(#13, Result) > 0) or (Pos(#10, Result) > 0) then
    Result := '"' + Result + '"';
end;

function UnescapeCSVField(const Field: string): string;
begin
  Result := Field;
  if (Length(Result) >= 2) and (Result[1] = '"') and (Result[Length(Result)] = '"') then
  begin
    Delete(Result, 1, 1);
    Delete(Result, Length(Result), 1);
    Result := StringReplace(Result, '""', '"', [rfReplaceAll]);
  end;
end;

end.
