unit wekan_models;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, sqldb, sqlite3conn, wekan_core, wekan_database;

type
  // Board model
  TBoard = class
  private
    FId: string;
    FTitle: string;
    FSlug: string;
    FArchived: Boolean;
    FArchivedAt: string;
    FCreatedAt: string;
    FModifiedAt: string;
    FStars: Integer;
    FPermission: string;
    FColor: string;
    FDescription: string;
    FType: string;
    FIsTemplate: Boolean;
    FIsTemplateContainer: Boolean;
    FMembers: TJSONArray;
    FLabels: TJSONArray;
    FSettings: TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure LoadFromJSON(const JSON: TJSONObject);
    function ToJSON: TJSONObject;
    
    property Id: string read FId write FId;
    property Title: string read FTitle write FTitle;
    property Slug: string read FSlug write FSlug;
    property Archived: Boolean read FArchived write FArchived;
    property ArchivedAt: string read FArchivedAt write FArchivedAt;
    property CreatedAt: string read FCreatedAt write FCreatedAt;
    property ModifiedAt: string read FModifiedAt write FModifiedAt;
    property Stars: Integer read FStars write FStars;
    property Permission: string read FPermission write FPermission;
    property Color: string read FColor write FColor;
    property Description: string read FDescription write FDescription;
    property Type: string read FType write FType;
    property IsTemplate: Boolean read FIsTemplate write FIsTemplate;
    property IsTemplateContainer: Boolean read FIsTemplateContainer write FIsTemplateContainer;
    property Members: TJSONArray read FMembers write FMembers;
    property Labels: TJSONArray read FLabels write FLabels;
    property Settings: TJSONObject read FSettings write FSettings;
  end;

  // List model
  TList = class
  private
    FId: string;
    FTitle: string;
    FBoardId: string;
    FArchived: Boolean;
    FArchivedAt: string;
    FCreatedAt: string;
    FModifiedAt: string;
    FSort: Integer;
    FWipLimit: Integer;
    FColor: string;
    FType: string;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure LoadFromJSON(const JSON: TJSONObject);
    function ToJSON: TJSONObject;
    
    property Id: string read FId write FId;
    property Title: string read FTitle write FTitle;
    property BoardId: string read FBoardId write FBoardId;
    property Archived: Boolean read FArchived write FArchived;
    property ArchivedAt: string read FArchivedAt write FArchivedAt;
    property CreatedAt: string read FCreatedAt write FCreatedAt;
    property ModifiedAt: string read FModifiedAt write FModifiedAt;
    property Sort: Integer read FSort write FSort;
    property WipLimit: Integer read FWipLimit write FWipLimit;
    property Color: string read FColor write FColor;
    property Type: string read FType write FType;
  end;

  // Card model
  TCard = class
  private
    FId: string;
    FTitle: string;
    FListId: string;
    FBoardId: string;
    FArchived: Boolean;
    FArchivedAt: string;
    FCreatedAt: string;
    FModifiedAt: string;
    FSort: Integer;
    FDescription: string;
    FDueAt: string;
    FCoverId: string;
    FColor: string;
    FMembers: TJSONArray;
    FLabels: TJSONArray;
    FCustomFields: TJSONArray;
    FType: string;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure LoadFromJSON(const JSON: TJSONObject);
    function ToJSON: TJSONObject;
    
    property Id: string read FId write FId;
    property Title: string read FTitle write FTitle;
    property ListId: string read FListId write FListId;
    property BoardId: string read FBoardId write FBoardId;
    property Archived: Boolean read FArchived write FArchived;
    property ArchivedAt: string read FArchivedAt write FArchivedAt;
    property CreatedAt: string read FCreatedAt write FCreatedAt;
    property ModifiedAt: string read FModifiedAt write FModifiedAt;
    property Sort: Integer read FSort write FSort;
    property Description: string read FDescription write FDescription;
    property DueAt: string read FDueAt write FDueAt;
    property CoverId: string read FCoverId write FCoverId;
    property Color: string read FColor write FColor;
    property Members: TJSONArray read FMembers write FMembers;
    property Labels: TJSONArray read FLabels write FLabels;
    property CustomFields: TJSONArray read FCustomFields write FCustomFields;
    property Type: string read FType write FType;
  end;

  // Board manager
  TBoardManager = class(TWeKanModel)
  public
    constructor Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction);
    destructor Destroy; override;
    
    function CreateBoard(const Title, Description: string; const UserId: string): TBoard;
    function GetBoardById(const BoardId: string): TBoard;
    function GetBoardsByUser(const UserId: string): TJSONArray;
    function UpdateBoard(const Board: TBoard): Boolean;
    function DeleteBoard(const BoardId: string): Boolean;
    function ArchiveBoard(const BoardId: string): Boolean;
    function UnarchiveBoard(const BoardId: string): Boolean;
    function AddMember(const BoardId, UserId: string): Boolean;
    function RemoveMember(const BoardId, UserId: string): Boolean;
    function AddLabel(const BoardId: string; const Label: TJSONObject): Boolean;
    function RemoveLabel(const BoardId, LabelId: string): Boolean;
  end;

  // List manager
  TListManager = class(TWeKanModel)
  public
    constructor Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction);
    destructor Destroy; override;
    
    function CreateList(const Title: string; const BoardId: string): TList;
    function GetListById(const ListId: string): TList;
    function GetListsByBoard(const BoardId: string): TJSONArray;
    function UpdateList(const List: TList): Boolean;
    function DeleteList(const ListId: string): Boolean;
    function ArchiveList(const ListId: string): Boolean;
    function UnarchiveList(const ListId: string): Boolean;
    function MoveList(const ListId: string; const NewSort: Integer): Boolean;
  end;

  // Card manager
  TCardManager = class(TWeKanModel)
  public
    constructor Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction);
    destructor Destroy; override;
    
    function CreateCard(const Title: string; const ListId, BoardId: string): TCard;
    function GetCardById(const CardId: string): TCard;
    function GetCardsByList(const ListId: string): TJSONArray;
    function GetCardsByBoard(const BoardId: string): TJSONArray;
    function UpdateCard(const Card: TCard): Boolean;
    function DeleteCard(const CardId: string): Boolean;
    function ArchiveCard(const CardId: string): Boolean;
    function UnarchiveCard(const CardId: string): Boolean;
    function MoveCard(const CardId: string; const NewListId: string; const NewSort: Integer): Boolean;
    function AddMember(const CardId, UserId: string): Boolean;
    function RemoveMember(const CardId, UserId: string): Boolean;
    function AddLabel(const CardId, LabelId: string): Boolean;
    function RemoveLabel(const CardId, LabelId: string): Boolean;
  end;

  // User manager
  TUserManager = class(TWeKanModel)
  public
    constructor Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction);
    destructor Destroy; override;
    
    function GetUsers: TJSONArray;
    function GetUserById(const UserId: string): TJSONObject;
    function GetUserByUsername(const Username: string): TJSONObject;
    function UpdateUser(const UserId: string; const UserData: TJSONObject): Boolean;
    function DeleteUser(const UserId: string): Boolean;
    function SearchUsers(const Query: string): TJSONArray;
  end;

  // File manager
  TFileManager = class
  private
    FUploadPath: string;
    FMaxFileSize: Int64;
  public
    constructor Create;
    destructor Destroy; override;
    
    function UploadFile(const FileName: string; const FileData: string; const UserId: string): string;
    function GetFilePath(const FileId: string): string;
    function DeleteFile(const FileId: string): Boolean;
    function GetFileInfo(const FileId: string): TJSONObject;
    
    property UploadPath: string read FUploadPath write FUploadPath;
    property MaxFileSize: Int64 read FMaxFileSize write FMaxFileSize;
  end;

implementation

// TBoard implementation
constructor TBoard.Create;
begin
  inherited Create;
  FMembers := TJSONArray.Create;
  FLabels := TJSONArray.Create;
  FSettings := TJSONObject.Create;
end;

destructor TBoard.Destroy;
begin
  if Assigned(FMembers) then
    FMembers.Free;
  if Assigned(FLabels) then
    FLabels.Free;
  if Assigned(FSettings) then
    FSettings.Free;
  inherited Destroy;
end;

procedure TBoard.LoadFromJSON(const JSON: TJSONObject);
begin
  if Assigned(JSON) then
  begin
    FId := JSON.Get('_id', '');
    FTitle := JSON.Get('title', '');
    FSlug := JSON.Get('slug', '');
    FArchived := JSON.Get('archived', False);
    FArchivedAt := JSON.Get('archivedAt', '');
    FCreatedAt := JSON.Get('createdAt', '');
    FModifiedAt := JSON.Get('modifiedAt', '');
    FStars := JSON.Get('stars', 0);
    FPermission := JSON.Get('permission', 'private');
    FColor := JSON.Get('color', 'belize');
    FDescription := JSON.Get('description', '');
    FType := JSON.Get('type', 'board');
    FIsTemplate := JSON.Get('isTemplate', False);
    FIsTemplateContainer := JSON.Get('isTemplateContainer', False);
    
    if JSON.Find('members') <> nil then
      FMembers := JSON.Arrays['members'];
    if JSON.Find('labels') <> nil then
      FLabels := JSON.Arrays['labels'];
    if JSON.Find('settings') <> nil then
      FSettings := JSON.Objects['settings'];
  end;
end;

function TBoard.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('_id', FId);
  Result.Add('title', FTitle);
  Result.Add('slug', FSlug);
  Result.Add('archived', FArchived);
  Result.Add('archivedAt', FArchivedAt);
  Result.Add('createdAt', FCreatedAt);
  Result.Add('modifiedAt', FModifiedAt);
  Result.Add('stars', FStars);
  Result.Add('permission', FPermission);
  Result.Add('color', FColor);
  Result.Add('description', FDescription);
  Result.Add('type', FType);
  Result.Add('isTemplate', FIsTemplate);
  Result.Add('isTemplateContainer', FIsTemplateContainer);
  Result.Add('members', FMembers);
  Result.Add('labels', FLabels);
  Result.Add('settings', FSettings);
end;

// TList implementation
constructor TList.Create;
begin
  inherited Create;
end;

destructor TList.Destroy;
begin
  inherited Destroy;
end;

procedure TList.LoadFromJSON(const JSON: TJSONObject);
begin
  if Assigned(JSON) then
  begin
    FId := JSON.Get('_id', '');
    FTitle := JSON.Get('title', '');
    FBoardId := JSON.Get('boardId', '');
    FArchived := JSON.Get('archived', False);
    FArchivedAt := JSON.Get('archivedAt', '');
    FCreatedAt := JSON.Get('createdAt', '');
    FModifiedAt := JSON.Get('modifiedAt', '');
    FSort := JSON.Get('sort', 0);
    FWipLimit := JSON.Get('wipLimit', 0);
    FColor := JSON.Get('color', '');
    FType := JSON.Get('type', 'list');
  end;
end;

function TList.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('_id', FId);
  Result.Add('title', FTitle);
  Result.Add('boardId', FBoardId);
  Result.Add('archived', FArchived);
  Result.Add('archivedAt', FArchivedAt);
  Result.Add('createdAt', FCreatedAt);
  Result.Add('modifiedAt', FModifiedAt);
  Result.Add('sort', FSort);
  Result.Add('wipLimit', FWipLimit);
  Result.Add('color', FColor);
  Result.Add('type', FType);
end;

// TCard implementation
constructor TCard.Create;
begin
  inherited Create;
  FMembers := TJSONArray.Create;
  FLabels := TJSONArray.Create;
  FCustomFields := TJSONArray.Create;
end;

destructor TCard.Destroy;
begin
  if Assigned(FMembers) then
    FMembers.Free;
  if Assigned(FLabels) then
    FLabels.Free;
  if Assigned(FCustomFields) then
    FCustomFields.Free;
  inherited Destroy;
end;

procedure TCard.LoadFromJSON(const JSON: TJSONObject);
begin
  if Assigned(JSON) then
  begin
    FId := JSON.Get('_id', '');
    FTitle := JSON.Get('title', '');
    FListId := JSON.Get('listId', '');
    FBoardId := JSON.Get('boardId', '');
    FArchived := JSON.Get('archived', False);
    FArchivedAt := JSON.Get('archivedAt', '');
    FCreatedAt := JSON.Get('createdAt', '');
    FModifiedAt := JSON.Get('modifiedAt', '');
    FSort := JSON.Get('sort', 0);
    FDescription := JSON.Get('description', '');
    FDueAt := JSON.Get('dueAt', '');
    FCoverId := JSON.Get('coverId', '');
    FColor := JSON.Get('color', '');
    FType := JSON.Get('type', 'card');
    
    if JSON.Find('members') <> nil then
      FMembers := JSON.Arrays['members'];
    if JSON.Find('labels') <> nil then
      FLabels := JSON.Arrays['labels'];
    if JSON.Find('customFields') <> nil then
      FCustomFields := JSON.Arrays['customFields'];
  end;
end;

function TCard.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('_id', FId);
  Result.Add('title', FTitle);
  Result.Add('listId', FListId);
  Result.Add('boardId', FBoardId);
  Result.Add('archived', FArchived);
  Result.Add('archivedAt', FArchivedAt);
  Result.Add('createdAt', FCreatedAt);
  Result.Add('modifiedAt', FModifiedAt);
  Result.Add('sort', FSort);
  Result.Add('description', FDescription);
  Result.Add('dueAt', FDueAt);
  Result.Add('coverId', FCoverId);
  Result.Add('color', FColor);
  Result.Add('type', FType);
  Result.Add('members', FMembers);
  Result.Add('labels', FLabels);
  Result.Add('customFields', FCustomFields);
end;

// TBoardManager implementation
constructor TBoardManager.Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction);
begin
  inherited Create(ADatabase, ATransaction);
end;

destructor TBoardManager.Destroy;
begin
  inherited Destroy;
end;

function TBoardManager.CreateBoard(const Title, Description: string; const UserId: string): TBoard;
var
  Board: TBoard;
  BoardId: string;
  SQL: string;
begin
  Result := nil;
  
  Board := TBoard.Create;
  try
    BoardId := GenerateId;
    
    Board.Id := BoardId;
    Board.Title := Title;
    Board.Slug := GenerateSlug(Title);
    Board.Description := Description;
    Board.CreatedAt := GetCurrentTimestamp;
    Board.Permission := 'private';
    Board.Color := 'belize';
    Board.Type := 'board';
    Board.IsTemplate := False;
    Board.IsTemplateContainer := False;
    Board.Archived := False;
    Board.Stars := 0;
    
    // Add creator as member
    Board.Members.Add(TJSONObject.Create(['userId', UserId, 'isAdmin', True, 'isActive', True]));
    
    // Insert board into database
    SQL := 'INSERT INTO boards (_id, title, slug, description, createdAt, permission, color, type, ' +
           'isTemplate, isTemplateContainer, archived, stars, members, labels, settings) ' +
           'VALUES (:id, :title, :slug, :description, :createdAt, :permission, :color, :type, ' +
           ':isTemplate, :isTemplateContainer, :archived, :stars, :members, :labels, :settings)';
    
    FQuery.Close;
    FQuery.SQL.Text := SQL;
    FQuery.ParamByName('id').AsString := Board.Id;
    FQuery.ParamByName('title').AsString := Board.Title;
    FQuery.ParamByName('slug').AsString := Board.Slug;
    FQuery.ParamByName('description').AsString := Board.Description;
    FQuery.ParamByName('createdAt').AsString := Board.CreatedAt;
    FQuery.ParamByName('permission').AsString := Board.Permission;
    FQuery.ParamByName('color').AsString := Board.Color;
    FQuery.ParamByName('type').AsString := Board.Type;
    FQuery.ParamByName('isTemplate').AsBoolean := Board.IsTemplate;
    FQuery.ParamByName('isTemplateContainer').AsBoolean := Board.IsTemplateContainer;
    FQuery.ParamByName('archived').AsBoolean := Board.Archived;
    FQuery.ParamByName('stars').AsInteger := Board.Stars;
    FQuery.ParamByName('members').AsString := Board.Members.AsJSON;
    FQuery.ParamByName('labels').AsString := Board.Labels.AsJSON;
    FQuery.ParamByName('settings').AsString := Board.Settings.AsJSON;
    FQuery.ExecSQL;
    FTransaction.Commit;
    
    Result := Board;
  except
    on E: Exception do
    begin
      Board.Free;
      Result := nil;
    end;
  end;
end;

function TBoardManager.GetBoardById(const BoardId: string): TBoard;
var
  Board: TBoard;
  SQL: string;
begin
  Result := nil;
  
  SQL := 'SELECT * FROM boards WHERE _id = :id';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := BoardId;
  FQuery.Open;
  
  if not FQuery.Eof then
  begin
    Board := TBoard.Create;
    try
      Board.Id := FQuery.FieldByName('_id').AsString;
      Board.Title := FQuery.FieldByName('title').AsString;
      Board.Slug := FQuery.FieldByName('slug').AsString;
      Board.Description := FQuery.FieldByName('description').AsString;
      Board.CreatedAt := FQuery.FieldByName('createdAt').AsString;
      Board.ModifiedAt := FQuery.FieldByName('modifiedAt').AsString;
      Board.Permission := FQuery.FieldByName('permission').AsString;
      Board.Color := FQuery.FieldByName('color').AsString;
      Board.Type := FQuery.FieldByName('type').AsString;
      Board.IsTemplate := FQuery.FieldByName('isTemplate').AsBoolean;
      Board.IsTemplateContainer := FQuery.FieldByName('isTemplateContainer').AsBoolean;
      Board.Archived := FQuery.FieldByName('archived').AsBoolean;
      Board.ArchivedAt := FQuery.FieldByName('archivedAt').AsString;
      Board.Stars := FQuery.FieldByName('stars').AsInteger;
      
      // Parse JSON fields
      if FQuery.FieldByName('members').AsString <> '' then
        Board.Members := GetJSON(FQuery.FieldByName('members').AsString) as TJSONArray;
      if FQuery.FieldByName('labels').AsString <> '' then
        Board.Labels := GetJSON(FQuery.FieldByName('labels').AsString) as TJSONArray;
      if FQuery.FieldByName('settings').AsString <> '' then
        Board.Settings := GetJSON(FQuery.FieldByName('settings').AsString) as TJSONObject;
        
      Result := Board;
    except
      on E: Exception do
      begin
        Board.Free;
        Result := nil;
      end;
    end;
  end;
end;

function TBoardManager.GetBoardsByUser(const UserId: string): TJSONArray;
var
  Boards: TJSONArray;
  SQL: string;
begin
  Result := TJSONArray.Create;
  
  SQL := 'SELECT * FROM boards WHERE members LIKE :userId ORDER BY modifiedAt DESC';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('userId').AsString := '%' + UserId + '%';
  FQuery.Open;
  
  while not FQuery.Eof do
  begin
    Result.Add(TJSONObject.Create([
      '_id', FQuery.FieldByName('_id').AsString,
      'title', FQuery.FieldByName('title').AsString,
      'slug', FQuery.FieldByName('slug').AsString,
      'description', FQuery.FieldByName('description').AsString,
      'createdAt', FQuery.FieldByName('createdAt').AsString,
      'modifiedAt', FQuery.FieldByName('modifiedAt').AsString,
      'permission', FQuery.FieldByName('permission').AsString,
      'color', FQuery.FieldByName('color').AsString,
      'type', FQuery.FieldByName('type').AsString,
      'isTemplate', FQuery.FieldByName('isTemplate').AsBoolean,
      'isTemplateContainer', FQuery.FieldByName('isTemplateContainer').AsBoolean,
      'archived', FQuery.FieldByName('archived').AsBoolean,
      'stars', FQuery.FieldByName('stars').AsInteger
    ]));
    FQuery.Next;
  end;
end;

function TBoardManager.UpdateBoard(const Board: TBoard): Boolean;
var
  SQL: string;
begin
  Result := False;
  
  if not Assigned(Board) then
    Exit;
    
  SQL := 'UPDATE boards SET title = :title, slug = :slug, description = :description, ' +
         'modifiedAt = :modifiedAt, permission = :permission, color = :color, type = :type, ' +
         'isTemplate = :isTemplate, isTemplateContainer = :isTemplateContainer, ' +
         'archived = :archived, archivedAt = :archivedAt, stars = :stars, ' +
         'members = :members, labels = :labels, settings = :settings WHERE _id = :id';
  
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := Board.Id;
  FQuery.ParamByName('title').AsString := Board.Title;
  FQuery.ParamByName('slug').AsString := Board.Slug;
  FQuery.ParamByName('description').AsString := Board.Description;
  FQuery.ParamByName('modifiedAt').AsString := GetCurrentTimestamp;
  FQuery.ParamByName('permission').AsString := Board.Permission;
  FQuery.ParamByName('color').AsString := Board.Color;
  FQuery.ParamByName('type').AsString := Board.Type;
  FQuery.ParamByName('isTemplate').AsBoolean := Board.IsTemplate;
  FQuery.ParamByName('isTemplateContainer').AsBoolean := Board.IsTemplateContainer;
  FQuery.ParamByName('archived').AsBoolean := Board.Archived;
  FQuery.ParamByName('archivedAt').AsString := Board.ArchivedAt;
  FQuery.ParamByName('stars').AsInteger := Board.Stars;
  FQuery.ParamByName('members').AsString := Board.Members.AsJSON;
  FQuery.ParamByName('labels').AsString := Board.Labels.AsJSON;
  FQuery.ParamByName('settings').AsString := Board.Settings.AsJSON;
  
  try
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TBoardManager.DeleteBoard(const BoardId: string): Boolean;
var
  SQL: string;
begin
  Result := False;
  
  SQL := 'DELETE FROM boards WHERE _id = :id';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := BoardId;
  
  try
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TBoardManager.ArchiveBoard(const BoardId: string): Boolean;
var
  SQL: string;
begin
  Result := False;
  
  SQL := 'UPDATE boards SET archived = TRUE, archivedAt = :archivedAt, modifiedAt = :modifiedAt WHERE _id = :id';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := BoardId;
  FQuery.ParamByName('archivedAt').AsString := GetCurrentTimestamp;
  FQuery.ParamByName('modifiedAt').AsString := GetCurrentTimestamp;
  
  try
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TBoardManager.UnarchiveBoard(const BoardId: string): Boolean;
var
  SQL: string;
begin
  Result := False;
  
  SQL := 'UPDATE boards SET archived = FALSE, archivedAt = NULL, modifiedAt = :modifiedAt WHERE _id = :id';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := BoardId;
  FQuery.ParamByName('modifiedAt').AsString := GetCurrentTimestamp;
  
  try
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TBoardManager.AddMember(const BoardId, UserId: string): Boolean;
var
  Board: TBoard;
  Member: TJSONObject;
begin
  Result := False;
  
  Board := GetBoardById(BoardId);
  if not Assigned(Board) then
    Exit;
    
  try
    Member := TJSONObject.Create(['userId', UserId, 'isAdmin', False, 'isActive', True]);
    Board.Members.Add(Member);
    Result := UpdateBoard(Board);
  finally
    Board.Free;
  end;
end;

function TBoardManager.RemoveMember(const BoardId, UserId: string): Boolean;
var
  Board: TBoard;
  i: Integer;
begin
  Result := False;
  
  Board := GetBoardById(BoardId);
  if not Assigned(Board) then
    Exit;
    
  try
    for i := Board.Members.Count - 1 downto 0 do
    begin
      if Board.Members.Objects[i].Get('userId', '') = UserId then
      begin
        Board.Members.Delete(i);
        Break;
      end;
    end;
    Result := UpdateBoard(Board);
  finally
    Board.Free;
  end;
end;

function TBoardManager.AddLabel(const BoardId: string; const Label: TJSONObject): Boolean;
var
  Board: TBoard;
begin
  Result := False;
  
  Board := GetBoardById(BoardId);
  if not Assigned(Board) then
    Exit;
    
  try
    Board.Labels.Add(Label);
    Result := UpdateBoard(Board);
  finally
    Board.Free;
  end;
end;

function TBoardManager.RemoveLabel(const BoardId, LabelId: string): Boolean;
var
  Board: TBoard;
  i: Integer;
begin
  Result := False;
  
  Board := GetBoardById(BoardId);
  if not Assigned(Board) then
    Exit;
    
  try
    for i := Board.Labels.Count - 1 downto 0 do
    begin
      if Board.Labels.Objects[i].Get('_id', '') = LabelId then
      begin
        Board.Labels.Delete(i);
        Break;
      end;
    end;
    Result := UpdateBoard(Board);
  finally
    Board.Free;
  end;
end;

// TListManager implementation
constructor TListManager.Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction);
begin
  inherited Create(ADatabase, ATransaction);
end;

destructor TListManager.Destroy;
begin
  inherited Destroy;
end;

function TListManager.CreateList(const Title: string; const BoardId: string): TList;
var
  List: TList;
  ListId: string;
  SQL: string;
begin
  Result := nil;
  
  List := TList.Create;
  try
    ListId := GenerateId;
    
    List.Id := ListId;
    List.Title := Title;
    List.BoardId := BoardId;
    List.CreatedAt := GetCurrentTimestamp;
    List.Type := 'list';
    List.Archived := False;
    List.Sort := 0;
    List.WipLimit := 0;
    
    // Insert list into database
    SQL := 'INSERT INTO lists (_id, title, boardId, createdAt, type, archived, sort, wipLimit, color) ' +
           'VALUES (:id, :title, :boardId, :createdAt, :type, :archived, :sort, :wipLimit, :color)';
    
    FQuery.Close;
    FQuery.SQL.Text := SQL;
    FQuery.ParamByName('id').AsString := List.Id;
    FQuery.ParamByName('title').AsString := List.Title;
    FQuery.ParamByName('boardId').AsString := List.BoardId;
    FQuery.ParamByName('createdAt').AsString := List.CreatedAt;
    FQuery.ParamByName('type').AsString := List.Type;
    FQuery.ParamByName('archived').AsBoolean := List.Archived;
    FQuery.ParamByName('sort').AsInteger := List.Sort;
    FQuery.ParamByName('wipLimit').AsInteger := List.WipLimit;
    FQuery.ParamByName('color').AsString := List.Color;
    FQuery.ExecSQL;
    FTransaction.Commit;
    
    Result := List;
  except
    on E: Exception do
    begin
      List.Free;
      Result := nil;
    end;
  end;
end;

function TListManager.GetListById(const ListId: string): TList;
var
  List: TList;
  SQL: string;
begin
  Result := nil;
  
  SQL := 'SELECT * FROM lists WHERE _id = :id';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := ListId;
  FQuery.Open;
  
  if not FQuery.Eof then
  begin
    List := TList.Create;
    try
      List.Id := FQuery.FieldByName('_id').AsString;
      List.Title := FQuery.FieldByName('title').AsString;
      List.BoardId := FQuery.FieldByName('boardId').AsString;
      List.CreatedAt := FQuery.FieldByName('createdAt').AsString;
      List.ModifiedAt := FQuery.FieldByName('modifiedAt').AsString;
      List.Type := FQuery.FieldByName('type').AsString;
      List.Archived := FQuery.FieldByName('archived').AsBoolean;
      List.ArchivedAt := FQuery.FieldByName('archivedAt').AsString;
      List.Sort := FQuery.FieldByName('sort').AsInteger;
      List.WipLimit := FQuery.FieldByName('wipLimit').AsInteger;
      List.Color := FQuery.FieldByName('color').AsString;
      
      Result := List;
    except
      on E: Exception do
      begin
        List.Free;
        Result := nil;
      end;
    end;
  end;
end;

function TListManager.GetListsByBoard(const BoardId: string): TJSONArray;
var
  Lists: TJSONArray;
  SQL: string;
begin
  Result := TJSONArray.Create;
  
  SQL := 'SELECT * FROM lists WHERE boardId = :boardId AND archived = FALSE ORDER BY sort ASC';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('boardId').AsString := BoardId;
  FQuery.Open;
  
  while not FQuery.Eof do
  begin
    Result.Add(TJSONObject.Create([
      '_id', FQuery.FieldByName('_id').AsString,
      'title', FQuery.FieldByName('title').AsString,
      'boardId', FQuery.FieldByName('boardId').AsString,
      'createdAt', FQuery.FieldByName('createdAt').AsString,
      'modifiedAt', FQuery.FieldByName('modifiedAt').AsString,
      'type', FQuery.FieldByName('type').AsString,
      'archived', FQuery.FieldByName('archived').AsBoolean,
      'sort', FQuery.FieldByName('sort').AsInteger,
      'wipLimit', FQuery.FieldByName('wipLimit').AsInteger,
      'color', FQuery.FieldByName('color').AsString
    ]));
    FQuery.Next;
  end;
end;

function TListManager.UpdateList(const List: TList): Boolean;
var
  SQL: string;
begin
  Result := False;
  
  if not Assigned(List) then
    Exit;
    
  SQL := 'UPDATE lists SET title = :title, boardId = :boardId, modifiedAt = :modifiedAt, ' +
         'type = :type, archived = :archived, archivedAt = :archivedAt, sort = :sort, ' +
         'wipLimit = :wipLimit, color = :color WHERE _id = :id';
  
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := List.Id;
  FQuery.ParamByName('title').AsString := List.Title;
  FQuery.ParamByName('boardId').AsString := List.BoardId;
  FQuery.ParamByName('modifiedAt').AsString := GetCurrentTimestamp;
  FQuery.ParamByName('type').AsString := List.Type;
  FQuery.ParamByName('archived').AsBoolean := List.Archived;
  FQuery.ParamByName('archivedAt').AsString := List.ArchivedAt;
  FQuery.ParamByName('sort').AsInteger := List.Sort;
  FQuery.ParamByName('wipLimit').AsInteger := List.WipLimit;
  FQuery.ParamByName('color').AsString := List.Color;
  
  try
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TListManager.DeleteList(const ListId: string): Boolean;
var
  SQL: string;
begin
  Result := False;
  
  SQL := 'DELETE FROM lists WHERE _id = :id';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := ListId;
  
  try
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TListManager.ArchiveList(const ListId: string): Boolean;
var
  SQL: string;
begin
  Result := False;
  
  SQL := 'UPDATE lists SET archived = TRUE, archivedAt = :archivedAt, modifiedAt = :modifiedAt WHERE _id = :id';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := ListId;
  FQuery.ParamByName('archivedAt').AsString := GetCurrentTimestamp;
  FQuery.ParamByName('modifiedAt').AsString := GetCurrentTimestamp;
  
  try
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TListManager.UnarchiveList(const ListId: string): Boolean;
var
  SQL: string;
begin
  Result := False;
  
  SQL := 'UPDATE lists SET archived = FALSE, archivedAt = NULL, modifiedAt = :modifiedAt WHERE _id = :id';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := ListId;
  FQuery.ParamByName('modifiedAt').AsString := GetCurrentTimestamp;
  
  try
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TListManager.MoveList(const ListId: string; const NewSort: Integer): Boolean;
var
  SQL: string;
begin
  Result := False;
  
  SQL := 'UPDATE lists SET sort = :sort, modifiedAt = :modifiedAt WHERE _id = :id';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := ListId;
  FQuery.ParamByName('sort').AsInteger := NewSort;
  FQuery.ParamByName('modifiedAt').AsString := GetCurrentTimestamp;
  
  try
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

// TCardManager implementation
constructor TCardManager.Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction);
begin
  inherited Create(ADatabase, ATransaction);
end;

destructor TCardManager.Destroy;
begin
  inherited Destroy;
end;

function TCardManager.CreateCard(const Title: string; const ListId, BoardId: string): TCard;
var
  Card: TCard;
  CardId: string;
  SQL: string;
begin
  Result := nil;
  
  Card := TCard.Create;
  try
    CardId := GenerateId;
    
    Card.Id := CardId;
    Card.Title := Title;
    Card.ListId := ListId;
    Card.BoardId := BoardId;
    Card.CreatedAt := GetCurrentTimestamp;
    Card.Type := 'card';
    Card.Archived := False;
    Card.Sort := 0;
    
    // Insert card into database
    SQL := 'INSERT INTO cards (_id, title, listId, boardId, createdAt, type, archived, sort, ' +
           'description, dueAt, coverId, color, members, labels, customFields) ' +
           'VALUES (:id, :title, :listId, :boardId, :createdAt, :type, :archived, :sort, ' +
           ':description, :dueAt, :coverId, :color, :members, :labels, :customFields)';
    
    FQuery.Close;
    FQuery.SQL.Text := SQL;
    FQuery.ParamByName('id').AsString := Card.Id;
    FQuery.ParamByName('title').AsString := Card.Title;
    FQuery.ParamByName('listId').AsString := Card.ListId;
    FQuery.ParamByName('boardId').AsString := Card.BoardId;
    FQuery.ParamByName('createdAt').AsString := Card.CreatedAt;
    FQuery.ParamByName('type').AsString := Card.Type;
    FQuery.ParamByName('archived').AsBoolean := Card.Archived;
    FQuery.ParamByName('sort').AsInteger := Card.Sort;
    FQuery.ParamByName('description').AsString := Card.Description;
    FQuery.ParamByName('dueAt').AsString := Card.DueAt;
    FQuery.ParamByName('coverId').AsString := Card.CoverId;
    FQuery.ParamByName('color').AsString := Card.Color;
    FQuery.ParamByName('members').AsString := Card.Members.AsJSON;
    FQuery.ParamByName('labels').AsString := Card.Labels.AsJSON;
    FQuery.ParamByName('customFields').AsString := Card.CustomFields.AsJSON;
    FQuery.ExecSQL;
    FTransaction.Commit;
    
    Result := Card;
  except
    on E: Exception do
    begin
      Card.Free;
      Result := nil;
    end;
  end;
end;

function TCardManager.GetCardById(const CardId: string): TCard;
var
  Card: TCard;
  SQL: string;
begin
  Result := nil;
  
  SQL := 'SELECT * FROM cards WHERE _id = :id';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := CardId;
  FQuery.Open;
  
  if not FQuery.Eof then
  begin
    Card := TCard.Create;
    try
      Card.Id := FQuery.FieldByName('_id').AsString;
      Card.Title := FQuery.FieldByName('title').AsString;
      Card.ListId := FQuery.FieldByName('listId').AsString;
      Card.BoardId := FQuery.FieldByName('boardId').AsString;
      Card.CreatedAt := FQuery.FieldByName('createdAt').AsString;
      Card.ModifiedAt := FQuery.FieldByName('modifiedAt').AsString;
      Card.Type := FQuery.FieldByName('type').AsString;
      Card.Archived := FQuery.FieldByName('archived').AsBoolean;
      Card.ArchivedAt := FQuery.FieldByName('archivedAt').AsString;
      Card.Sort := FQuery.FieldByName('sort').AsInteger;
      Card.Description := FQuery.FieldByName('description').AsString;
      Card.DueAt := FQuery.FieldByName('dueAt').AsString;
      Card.CoverId := FQuery.FieldByName('coverId').AsString;
      Card.Color := FQuery.FieldByName('color').AsString;
      
      // Parse JSON fields
      if FQuery.FieldByName('members').AsString <> '' then
        Card.Members := GetJSON(FQuery.FieldByName('members').AsString) as TJSONArray;
      if FQuery.FieldByName('labels').AsString <> '' then
        Card.Labels := GetJSON(FQuery.FieldByName('labels').AsString) as TJSONArray;
      if FQuery.FieldByName('customFields').AsString <> '' then
        Card.CustomFields := GetJSON(FQuery.FieldByName('customFields').AsString) as TJSONArray;
        
      Result := Card;
    except
      on E: Exception do
      begin
        Card.Free;
        Result := nil;
      end;
    end;
  end;
end;

function TCardManager.GetCardsByList(const ListId: string): TJSONArray;
var
  Cards: TJSONArray;
  SQL: string;
begin
  Result := TJSONArray.Create;
  
  SQL := 'SELECT * FROM cards WHERE listId = :listId AND archived = FALSE ORDER BY sort ASC';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('listId').AsString := ListId;
  FQuery.Open;
  
  while not FQuery.Eof do
  begin
    Result.Add(TJSONObject.Create([
      '_id', FQuery.FieldByName('_id').AsString,
      'title', FQuery.FieldByName('title').AsString,
      'listId', FQuery.FieldByName('listId').AsString,
      'boardId', FQuery.FieldByName('boardId').AsString,
      'createdAt', FQuery.FieldByName('createdAt').AsString,
      'modifiedAt', FQuery.FieldByName('modifiedAt').AsString,
      'type', FQuery.FieldByName('type').AsString,
      'archived', FQuery.FieldByName('archived').AsBoolean,
      'sort', FQuery.FieldByName('sort').AsInteger,
      'description', FQuery.FieldByName('description').AsString,
      'dueAt', FQuery.FieldByName('dueAt').AsString,
      'coverId', FQuery.FieldByName('coverId').AsString,
      'color', FQuery.FieldByName('color').AsString
    ]));
    FQuery.Next;
  end;
end;

function TCardManager.GetCardsByBoard(const BoardId: string): TJSONArray;
var
  Cards: TJSONArray;
  SQL: string;
begin
  Result := TJSONArray.Create;
  
  SQL := 'SELECT * FROM cards WHERE boardId = :boardId AND archived = FALSE ORDER BY sort ASC';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('boardId').AsString := BoardId;
  FQuery.Open;
  
  while not FQuery.Eof do
  begin
    Result.Add(TJSONObject.Create([
      '_id', FQuery.FieldByName('_id').AsString,
      'title', FQuery.FieldByName('title').AsString,
      'listId', FQuery.FieldByName('listId').AsString,
      'boardId', FQuery.FieldByName('boardId').AsString,
      'createdAt', FQuery.FieldByName('createdAt').AsString,
      'modifiedAt', FQuery.FieldByName('modifiedAt').AsString,
      'type', FQuery.FieldByName('type').AsString,
      'archived', FQuery.FieldByName('archived').AsBoolean,
      'sort', FQuery.FieldByName('sort').AsInteger,
      'description', FQuery.FieldByName('description').AsString,
      'dueAt', FQuery.FieldByName('dueAt').AsString,
      'coverId', FQuery.FieldByName('coverId').AsString,
      'color', FQuery.FieldByName('color').AsString
    ]));
    FQuery.Next;
  end;
end;

function TCardManager.UpdateCard(const Card: TCard): Boolean;
var
  SQL: string;
begin
  Result := False;
  
  if not Assigned(Card) then
    Exit;
    
  SQL := 'UPDATE cards SET title = :title, listId = :listId, boardId = :boardId, ' +
         'modifiedAt = :modifiedAt, type = :type, archived = :archived, archivedAt = :archivedAt, ' +
         'sort = :sort, description = :description, dueAt = :dueAt, coverId = :coverId, ' +
         'color = :color, members = :members, labels = :labels, customFields = :customFields ' +
         'WHERE _id = :id';
  
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := Card.Id;
  FQuery.ParamByName('title').AsString := Card.Title;
  FQuery.ParamByName('listId').AsString := Card.ListId;
  FQuery.ParamByName('boardId').AsString := Card.BoardId;
  FQuery.ParamByName('modifiedAt').AsString := GetCurrentTimestamp;
  FQuery.ParamByName('type').AsString := Card.Type;
  FQuery.ParamByName('archived').AsBoolean := Card.Archived;
  FQuery.ParamByName('archivedAt').AsString := Card.ArchivedAt;
  FQuery.ParamByName('sort').AsInteger := Card.Sort;
  FQuery.ParamByName('description').AsString := Card.Description;
  FQuery.ParamByName('dueAt').AsString := Card.DueAt;
  FQuery.ParamByName('coverId').AsString := Card.CoverId;
  FQuery.ParamByName('color').AsString := Card.Color;
  FQuery.ParamByName('members').AsString := Card.Members.AsJSON;
  FQuery.ParamByName('labels').AsString := Card.Labels.AsJSON;
  FQuery.ParamByName('customFields').AsString := Card.CustomFields.AsJSON;
  
  try
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TCardManager.DeleteCard(const CardId: string): Boolean;
var
  SQL: string;
begin
  Result := False;
  
  SQL := 'DELETE FROM cards WHERE _id = :id';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := CardId;
  
  try
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TCardManager.ArchiveCard(const CardId: string): Boolean;
var
  SQL: string;
begin
  Result := False;
  
  SQL := 'UPDATE cards SET archived = TRUE, archivedAt = :archivedAt, modifiedAt = :modifiedAt WHERE _id = :id';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := CardId;
  FQuery.ParamByName('archivedAt').AsString := GetCurrentTimestamp;
  FQuery.ParamByName('modifiedAt').AsString := GetCurrentTimestamp;
  
  try
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TCardManager.UnarchiveCard(const CardId: string): Boolean;
var
  SQL: string;
begin
  Result := False;
  
  SQL := 'UPDATE cards SET archived = FALSE, archivedAt = NULL, modifiedAt = :modifiedAt WHERE _id = :id';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := CardId;
  FQuery.ParamByName('modifiedAt').AsString := GetCurrentTimestamp;
  
  try
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TCardManager.MoveCard(const CardId: string; const NewListId: string; const NewSort: Integer): Boolean;
var
  SQL: string;
begin
  Result := False;
  
  SQL := 'UPDATE cards SET listId = :listId, sort = :sort, modifiedAt = :modifiedAt WHERE _id = :id';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := CardId;
  FQuery.ParamByName('listId').AsString := NewListId;
  FQuery.ParamByName('sort').AsInteger := NewSort;
  FQuery.ParamByName('modifiedAt').AsString := GetCurrentTimestamp;
  
  try
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TCardManager.AddMember(const CardId, UserId: string): Boolean;
var
  Card: TCard;
  Member: TJSONObject;
begin
  Result := False;
  
  Card := GetCardById(CardId);
  if not Assigned(Card) then
    Exit;
    
  try
    Member := TJSONObject.Create(['userId', UserId, 'isActive', True]);
    Card.Members.Add(Member);
    Result := UpdateCard(Card);
  finally
    Card.Free;
  end;
end;

function TCardManager.RemoveMember(const CardId, UserId: string): Boolean;
var
  Card: TCard;
  i: Integer;
begin
  Result := False;
  
  Card := GetCardById(CardId);
  if not Assigned(Card) then
    Exit;
    
  try
    for i := Card.Members.Count - 1 downto 0 do
    begin
      if Card.Members.Objects[i].Get('userId', '') = UserId then
      begin
        Card.Members.Delete(i);
        Break;
      end;
    end;
    Result := UpdateCard(Card);
  finally
    Card.Free;
  end;
end;

function TCardManager.AddLabel(const CardId, LabelId: string): Boolean;
var
  Card: TCard;
  Label: TJSONObject;
begin
  Result := False;
  
  Card := GetCardById(CardId);
  if not Assigned(Card) then
    Exit;
    
  try
    Label := TJSONObject.Create(['_id', LabelId]);
    Card.Labels.Add(Label);
    Result := UpdateCard(Card);
  finally
    Card.Free;
  end;
end;

function TCardManager.RemoveLabel(const CardId, LabelId: string): Boolean;
var
  Card: TCard;
  i: Integer;
begin
  Result := False;
  
  Card := GetCardById(CardId);
  if not Assigned(Card) then
    Exit;
    
  try
    for i := Card.Labels.Count - 1 downto 0 do
    begin
      if Card.Labels.Objects[i].Get('_id', '') = LabelId then
      begin
        Card.Labels.Delete(i);
        Break;
      end;
    end;
    Result := UpdateCard(Card);
  finally
    Card.Free;
  end;
end;

// TUserManager implementation
constructor TUserManager.Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction);
begin
  inherited Create(ADatabase, ATransaction);
end;

destructor TUserManager.Destroy;
begin
  inherited Destroy;
end;

function TUserManager.GetUsers: TJSONArray;
var
  Users: TJSONArray;
  SQL: string;
begin
  Result := TJSONArray.Create;
  
  SQL := 'SELECT _id, username, emails, createdAt, lastLogin, isAdmin, isActive FROM users ORDER BY username ASC';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.Open;
  
  while not FQuery.Eof do
  begin
    Result.Add(TJSONObject.Create([
      '_id', FQuery.FieldByName('_id').AsString,
      'username', FQuery.FieldByName('username').AsString,
      'emails', FQuery.FieldByName('emails').AsString,
      'createdAt', FQuery.FieldByName('createdAt').AsString,
      'lastLogin', FQuery.FieldByName('lastLogin').AsString,
      'isAdmin', FQuery.FieldByName('isAdmin').AsBoolean,
      'isActive', FQuery.FieldByName('isActive').AsBoolean
    ]));
    FQuery.Next;
  end;
end;

function TUserManager.GetUserById(const UserId: string): TJSONObject;
var
  User: TJSONObject;
  SQL: string;
begin
  Result := nil;
  
  SQL := 'SELECT _id, username, emails, createdAt, lastLogin, isAdmin, isActive FROM users WHERE _id = :id';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := UserId;
  FQuery.Open;
  
  if not FQuery.Eof then
  begin
    Result := TJSONObject.Create([
      '_id', FQuery.FieldByName('_id').AsString,
      'username', FQuery.FieldByName('username').AsString,
      'emails', FQuery.FieldByName('emails').AsString,
      'createdAt', FQuery.FieldByName('createdAt').AsString,
      'lastLogin', FQuery.FieldByName('lastLogin').AsString,
      'isAdmin', FQuery.FieldByName('isAdmin').AsBoolean,
      'isActive', FQuery.FieldByName('isActive').AsBoolean
    ]);
  end;
end;

function TUserManager.GetUserByUsername(const Username: string): TJSONObject;
var
  User: TJSONObject;
  SQL: string;
begin
  Result := nil;
  
  SQL := 'SELECT _id, username, emails, createdAt, lastLogin, isAdmin, isActive FROM users WHERE username = :username';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('username').AsString := Username;
  FQuery.Open;
  
  if not FQuery.Eof then
  begin
    Result := TJSONObject.Create([
      '_id', FQuery.FieldByName('_id').AsString,
      'username', FQuery.FieldByName('username').AsString,
      'emails', FQuery.FieldByName('emails').AsString,
      'createdAt', FQuery.FieldByName('createdAt').AsString,
      'lastLogin', FQuery.FieldByName('lastLogin').AsString,
      'isAdmin', FQuery.FieldByName('isAdmin').AsBoolean,
      'isActive', FQuery.FieldByName('isActive').AsBoolean
    ]);
  end;
end;

function TUserManager.UpdateUser(const UserId: string; const UserData: TJSONObject): Boolean;
var
  SQL: string;
begin
  Result := False;
  
  if not Assigned(UserData) then
    Exit;
    
  SQL := 'UPDATE users SET username = :username, emails = :emails, modifiedAt = :modifiedAt, ' +
         'isAdmin = :isAdmin, isActive = :isActive WHERE _id = :id';
  
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := UserId;
  FQuery.ParamByName('username').AsString := UserData.Get('username', '');
  FQuery.ParamByName('emails').AsString := UserData.Get('emails', '');
  FQuery.ParamByName('modifiedAt').AsString := GetCurrentTimestamp;
  FQuery.ParamByName('isAdmin').AsBoolean := UserData.Get('isAdmin', False);
  FQuery.ParamByName('isActive').AsBoolean := UserData.Get('isActive', True);
  
  try
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TUserManager.DeleteUser(const UserId: string): Boolean;
var
  SQL: string;
begin
  Result := False;
  
  SQL := 'DELETE FROM users WHERE _id = :id';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('id').AsString := UserId;
  
  try
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TUserManager.SearchUsers(const Query: string): TJSONArray;
var
  Users: TJSONArray;
  SQL: string;
begin
  Result := TJSONArray.Create;
  
  SQL := 'SELECT _id, username, emails, createdAt, lastLogin, isAdmin, isActive FROM users ' +
         'WHERE username LIKE :query OR emails LIKE :query ORDER BY username ASC';
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('query').AsString := '%' + Query + '%';
  FQuery.Open;
  
  while not FQuery.Eof do
  begin
    Result.Add(TJSONObject.Create([
      '_id', FQuery.FieldByName('_id').AsString,
      'username', FQuery.FieldByName('username').AsString,
      'emails', FQuery.FieldByName('emails').AsString,
      'createdAt', FQuery.FieldByName('createdAt').AsString,
      'lastLogin', FQuery.FieldByName('lastLogin').AsString,
      'isAdmin', FQuery.FieldByName('isAdmin').AsBoolean,
      'isActive', FQuery.FieldByName('isActive').AsBoolean
    ]));
    FQuery.Next;
  end;
end;

// TFileManager implementation
constructor TFileManager.Create;
begin
  inherited Create;
  FUploadPath := 'uploads/';
  FMaxFileSize := 10 * 1024 * 1024; // 10MB
end;

destructor TFileManager.Destroy;
begin
  inherited Destroy;
end;

function TFileManager.UploadFile(const FileName: string; const FileData: string; const UserId: string): string;
var
  FileId: string;
  FilePath: string;
  FileStream: TFileStream;
begin
  Result := '';
  
  FileId := GenerateId;
  FilePath := FUploadPath + FileId + '_' + FileName;
  
  try
    // Create upload directory if it doesn't exist
    if not DirectoryExists(FUploadPath) then
      CreateDir(FUploadPath);
      
    // Write file
    FileStream := TFileStream.Create(FilePath, fmCreate);
    try
      FileStream.WriteBuffer(FileData[1], Length(FileData));
    finally
      FileStream.Free;
    end;
    
    Result := FileId;
  except
    on E: Exception do
      Result := '';
  end;
end;

function TFileManager.GetFilePath(const FileId: string): string;
var
  SQL: string;
begin
  Result := '';
  
  SQL := 'SELECT path FROM attachments WHERE _id = :id';
  // This would need to be implemented with proper database connection
  // For now, return a simple path
  Result := FUploadPath + FileId;
end;

function TFileManager.DeleteFile(const FileId: string): Boolean;
var
  FilePath: string;
begin
  Result := False;
  
  FilePath := GetFilePath(FileId);
  if FileExists(FilePath) then
  begin
    try
      DeleteFile(FilePath);
      Result := True;
    except
      on E: Exception do
        Result := False;
    end;
  end;
end;

function TFileManager.GetFileInfo(const FileId: string): TJSONObject;
var
  FilePath: string;
  FileInfo: TJSONObject;
begin
  Result := nil;
  
  FilePath := GetFilePath(FileId);
  if FileExists(FilePath) then
  begin
    FileInfo := TJSONObject.Create;
    try
      FileInfo.Add('_id', FileId);
      FileInfo.Add('path', FilePath);
      FileInfo.Add('size', FileSize(FilePath));
      FileInfo.Add('exists', True);
      
      Result := FileInfo;
    except
      on E: Exception do
      begin
        FileInfo.Free;
        Result := nil;
      end;
    end;
  end;
end;

end.
