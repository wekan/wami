unit wekan_core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, sqldb, sqlite3conn, HTTPDefs, httproute;

type
  // Core configuration class
  TWeKanConfig = class
  private
    FDatabaseFile: string;
    FPort: Integer;
    FLogLevel: string;
    FSessionTimeout: Integer;
    FMaxFileSize: Int64;
    FUploadPath: string;
    FPublicPath: string;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    
    property DatabaseFile: string read FDatabaseFile write FDatabaseFile;
    property Port: Integer read FPort write FPort;
    property LogLevel: string read FLogLevel write FLogLevel;
    property SessionTimeout: Integer read FSessionTimeout write FSessionTimeout;
    property MaxFileSize: Int64 read FMaxFileSize write FMaxFileSize;
    property UploadPath: string read FUploadPath write FUploadPath;
    property PublicPath: string read FPublicPath write FPublicPath;
  end;

  // Base model class
  TWeKanModel = class
  protected
    FDatabase: TSQLite3Connection;
    FTransaction: TSQLTransaction;
    FQuery: TSQLQuery;
  public
    constructor Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction);
    destructor Destroy; override;
    
    function GenerateId: string;
    function GetCurrentTimestamp: string;
    function SanitizeInput(const Input: string): string;
    function ValidateEmail(const Email: string): Boolean;
    function HashPassword(const Password: string): string;
    function VerifyPassword(const Password, Hash: string): Boolean;
  end;

  // WebSocket handler for real-time features
  TWebSocketHandler = class
  private
    FClients: TList;
    FEventQueue: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure AddClient(Client: TObject);
    procedure RemoveClient(Client: TObject);
    procedure Broadcast(const Event: string; const Data: string; const BoardId: string = '');
    procedure SendToUser(const UserId: string; const Event: string; const Data: string);
    procedure ProcessEventQueue;
  end;

  // Event types for real-time updates
  TWeKanEvent = record
    EventType: string;
    Data: string;
    BoardId: string;
    UserId: string;
    Timestamp: TDateTime;
  end;

  // Utility functions
  function FormatDateTime(const DateTime: TDateTime): string;
  function ParseDateTime(const DateTimeStr: string): TDateTime;
  function GenerateSlug(const Title: string): string;
  function IsValidId(const Id: string): Boolean;
  function CreateJSONResponse(const Success: Boolean; const Message: string; const Data: TJSONObject = nil): string;
  function CreateJSONError(const Message: string; const Code: Integer = 500): string;

  // Route handler types
  procedure HandleStaticFiles(ARequest: TRequest; AResponse: TResponse);
  procedure HandleHomePage(ARequest: TRequest; AResponse: TResponse);
  procedure HandleLoginPage(ARequest: TRequest; AResponse: TResponse);
  procedure HandleRegisterPage(ARequest: TRequest; AResponse: TResponse);
  procedure HandleBoardsPage(ARequest: TRequest; AResponse: TResponse);
  procedure HandleBoardPage(ARequest: TRequest; AResponse: TResponse);
  procedure HandleCardPage(ARequest: TRequest; AResponse: TResponse);
  procedure HandleWebSocket(ARequest: TRequest; AResponse: TResponse);

implementation

uses
  md5, sha1, base64, strutils, dateutils, regexpr, fpjson, jsonparser;

// TWeKanConfig implementation
constructor TWeKanConfig.Create;
begin
  inherited Create;
  FDatabaseFile := 'wekan.db';
  FPort := 5500;
  FLogLevel := 'INFO';
  FSessionTimeout := 3600; // 1 hour
  FMaxFileSize := 10 * 1024 * 1024; // 10MB
  FUploadPath := 'uploads/';
  FPublicPath := 'public/';
end;

destructor TWeKanConfig.Destroy;
begin
  inherited Destroy;
end;

procedure TWeKanConfig.LoadFromFile(const FileName: string);
var
  ConfigFile: TStringList;
  Line: string;
  Key, Value: string;
  i: Integer;
begin
  if not FileExists(FileName) then
    Exit;
    
  ConfigFile := TStringList.Create;
  try
    ConfigFile.LoadFromFile(FileName);
    
    for i := 0 to ConfigFile.Count - 1 do
    begin
      Line := Trim(ConfigFile[i]);
      if (Line = '') or (Line[1] = '#') then
        Continue;
        
      if Pos('=', Line) > 0 then
      begin
        Key := Trim(Copy(Line, 1, Pos('=', Line) - 1));
        Value := Trim(Copy(Line, Pos('=', Line) + 1, Length(Line)));
        
        case LowerCase(Key) of
          'database_file': FDatabaseFile := Value;
          'port': FPort := StrToIntDef(Value, 5500);
          'log_level': FLogLevel := Value;
          'session_timeout': FSessionTimeout := StrToIntDef(Value, 3600);
          'max_file_size': FMaxFileSize := StrToInt64Def(Value, 10485760);
          'upload_path': FUploadPath := Value;
          'public_path': FPublicPath := Value;
        end;
      end;
    end;
  finally
    ConfigFile.Free;
  end;
end;

procedure TWeKanConfig.SaveToFile(const FileName: string);
var
  ConfigFile: TStringList;
begin
  ConfigFile := TStringList.Create;
  try
    ConfigFile.Add('# WeKan WAMI Configuration');
    ConfigFile.Add('# Generated on ' + DateTimeToStr(Now));
    ConfigFile.Add('');
    ConfigFile.Add('database_file=' + FDatabaseFile);
    ConfigFile.Add('port=' + IntToStr(FPort));
    ConfigFile.Add('log_level=' + FLogLevel);
    ConfigFile.Add('session_timeout=' + IntToStr(FSessionTimeout));
    ConfigFile.Add('max_file_size=' + IntToStr(FMaxFileSize));
    ConfigFile.Add('upload_path=' + FUploadPath);
    ConfigFile.Add('public_path=' + FPublicPath);
    
    ConfigFile.SaveToFile(FileName);
  finally
    ConfigFile.Free;
  end;
end;

// TWeKanModel implementation
constructor TWeKanModel.Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction);
begin
  inherited Create;
  FDatabase := ADatabase;
  FTransaction := ATransaction;
  FQuery := TSQLQuery.Create(nil);
  FQuery.Database := FDatabase;
  FQuery.Transaction := FTransaction;
end;

destructor TWeKanModel.Destroy;
begin
  if Assigned(FQuery) then
    FQuery.Free;
  inherited Destroy;
end;

function TWeKanModel.GenerateId: string;
var
  Guid: TGUID;
  GuidStr: string;
begin
  CreateGUID(Guid);
  GuidStr := GUIDToString(Guid);
  // Remove braces and convert to lowercase
  Result := LowerCase(StringReplace(StringReplace(GuidStr, '{', '', []), '}', '', []));
end;

function TWeKanModel.GetCurrentTimestamp: string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
end;

function TWeKanModel.SanitizeInput(const Input: string): string;
begin
  Result := StringReplace(Input, '''', '''''', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
end;

function TWeKanModel.ValidateEmail(const Email: string): Boolean;
var
  Regex: TRegExpr;
begin
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$';
    Result := Regex.Exec(Email);
  finally
    Regex.Free;
  end;
end;

function TWeKanModel.HashPassword(const Password: string): string;
var
  Salt: string;
  Hash: string;
begin
  // Generate random salt
  Salt := GenerateId;
  // Hash password with salt using SHA-256
  Hash := SHA1Print(SHA1String(Password + Salt));
  Result := Salt + ':' + Hash;
end;

function TWeKanModel.VerifyPassword(const Password, Hash: string): Boolean;
var
  Salt, StoredHash, ComputedHash: string;
  ColonPos: Integer;
begin
  ColonPos := Pos(':', Hash);
  if ColonPos = 0 then
  begin
    Result := False;
    Exit;
  end;
  
  Salt := Copy(Hash, 1, ColonPos - 1);
  StoredHash := Copy(Hash, ColonPos + 1, Length(Hash));
  ComputedHash := SHA1Print(SHA1String(Password + Salt));
  
  Result := (ComputedHash = StoredHash);
end;

// TWebSocketHandler implementation
constructor TWebSocketHandler.Create;
begin
  inherited Create;
  FClients := TList.Create;
  FEventQueue := TStringList.Create;
end;

destructor TWebSocketHandler.Destroy;
begin
  if Assigned(FEventQueue) then
    FEventQueue.Free;
  if Assigned(FClients) then
    FClients.Free;
  inherited Destroy;
end;

procedure TWebSocketHandler.AddClient(Client: TObject);
begin
  FClients.Add(Client);
end;

procedure TWebSocketHandler.RemoveClient(Client: TObject);
begin
  FClients.Remove(Client);
end;

procedure TWebSocketHandler.Broadcast(const Event: string; const Data: string; const BoardId: string);
var
  i: Integer;
  EventData: string;
begin
  EventData := '{"event":"' + Event + '","data":' + Data + ',"boardId":"' + BoardId + '","timestamp":"' + FormatDateTime(Now) + '"}';
  
  // Add to event queue for processing
  FEventQueue.Add(EventData);
  
  // Process event queue
  ProcessEventQueue;
end;

procedure TWebSocketHandler.SendToUser(const UserId: string; const Event: string; const Data: string);
var
  EventData: string;
begin
  EventData := '{"event":"' + Event + '","data":' + Data + ',"userId":"' + UserId + '","timestamp":"' + FormatDateTime(Now) + '"}';
  FEventQueue.Add(EventData);
  ProcessEventQueue;
end;

procedure TWebSocketHandler.ProcessEventQueue;
var
  i: Integer;
begin
  // Process all events in queue
  for i := 0 to FEventQueue.Count - 1 do
  begin
    // Send to all connected clients
    // Implementation depends on specific WebSocket library used
  end;
  
  // Clear processed events
  FEventQueue.Clear;
end;

// Utility functions
function FormatDateTime(const DateTime: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', DateTime);
end;

function ParseDateTime(const DateTimeStr: string): TDateTime;
begin
  Result := StrToDateTimeDef(DateTimeStr, Now);
end;

function GenerateSlug(const Title: string): string;
var
  i: Integer;
  c: Char;
begin
  Result := '';
  for i := 1 to Length(Title) do
  begin
    c := Title[i];
    if (c in ['a'..'z', 'A'..'Z', '0'..'9']) then
      Result := Result + LowerCase(c)
    else if (c = ' ') then
      Result := Result + '-'
    else if (c in ['-', '_']) then
      Result := Result + c;
  end;
  
  // Remove multiple consecutive dashes
  while Pos('--', Result) > 0 do
    Result := StringReplace(Result, '--', '-', [rfReplaceAll]);
    
  // Remove leading/trailing dashes
  if (Result <> '') and (Result[1] = '-') then
    Delete(Result, 1, 1);
  if (Result <> '') and (Result[Length(Result)] = '-') then
    Delete(Result, Length(Result), 1);
end;

function IsValidId(const Id: string): Boolean;
var
  Regex: TRegExpr;
begin
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$';
    Result := Regex.Exec(Id);
  finally
    Regex.Free;
  end;
end;

function CreateJSONResponse(const Success: Boolean; const Message: string; const Data: TJSONObject): string;
var
  Response: TJSONObject;
begin
  Response := TJSONObject.Create;
  try
    Response.Add('success', Success);
    Response.Add('message', Message);
    if Assigned(Data) then
      Response.Add('data', Data);
    Result := Response.AsJSON;
  finally
    Response.Free;
  end;
end;

function CreateJSONError(const Message: string; const Code: Integer): string;
var
  Response: TJSONObject;
begin
  Response := TJSONObject.Create;
  try
    Response.Add('success', False);
    Response.Add('error', Message);
    Response.Add('code', Code);
    Result := Response.AsJSON;
  finally
    Response.Free;
  end;
end;

// Route handlers (basic implementations)
procedure HandleStaticFiles(ARequest: TRequest; AResponse: TResponse);
begin
  // Serve static files from public directory
  AResponse.Content := 'Static file serving not implemented yet';
  AResponse.Code := 200;
end;

procedure HandleHomePage(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.Content := 'WeKan WAMI Home Page';
  AResponse.Code := 200;
end;

procedure HandleLoginPage(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.Content := 'WeKan WAMI Login Page';
  AResponse.Code := 200;
end;

procedure HandleRegisterPage(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.Content := 'WeKan WAMI Register Page';
  AResponse.Code := 200;
end;

procedure HandleBoardsPage(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.Content := 'WeKan WAMI Boards Page';
  AResponse.Code := 200;
end;

procedure HandleBoardPage(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.Content := 'WeKan WAMI Board Page';
  AResponse.Code := 200;
end;

procedure HandleCardPage(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.Content := 'WeKan WAMI Card Page';
  AResponse.Code := 200;
end;

procedure HandleWebSocket(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.Content := 'WebSocket connection not implemented yet';
  AResponse.Code := 200;
end;

end.
