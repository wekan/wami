unit wekan_database;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, sqlite3conn, fpjson, wekan_core;

type
  // Database manager class
  TDatabaseManager = class
  private
    FDatabase: TSQLite3Connection;
    FTransaction: TSQLTransaction;
    FQuery: TSQLQuery;
  public
    constructor Create(const DatabaseFile: string);
    destructor Destroy; override;
    
    procedure InitializeDatabase;
    procedure CreateTables;
    procedure CreateIndexes;
    procedure CreateTriggers;
    
    function ExecuteQuery(const SQL: string): TSQLQuery;
    function ExecuteUpdate(const SQL: string): Integer;
    function GetLastInsertId: string;
    
    property Database: TSQLite3Connection read FDatabase;
    property Transaction: TSQLTransaction read FTransaction;
    property Query: TSQLQuery read FQuery;
  end;

  // Migration manager for database schema updates
  TMigrationManager = class
  private
    FDatabase: TSQLite3Connection;
    FTransaction: TSQLTransaction;
  public
    constructor Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction);
    destructor Destroy; override;
    
    procedure RunMigrations;
    procedure CreateMigrationTable;
    function GetCurrentVersion: Integer;
    procedure SetVersion(const Version: Integer);
    procedure ExecuteMigration(const Version: Integer; const SQL: string);
  end;

implementation

// TDatabaseManager implementation
constructor TDatabaseManager.Create(const DatabaseFile: string);
begin
  inherited Create;
  
  FDatabase := TSQLite3Connection.Create(nil);
  FDatabase.DatabaseName := DatabaseFile;
  FDatabase.Connected := True;
  
  FTransaction := TSQLTransaction.Create(nil);
  FTransaction.Database := FDatabase;
  FDatabase.Transaction := FTransaction;
  
  FQuery := TSQLQuery.Create(nil);
  FQuery.Database := FDatabase;
  FQuery.Transaction := FTransaction;
  
  InitializeDatabase;
end;

destructor TDatabaseManager.Destroy;
begin
  if Assigned(FQuery) then
    FQuery.Free;
  if Assigned(FTransaction) then
    FTransaction.Free;
  if Assigned(FDatabase) then
    FDatabase.Free;
  inherited Destroy;
end;

procedure TDatabaseManager.InitializeDatabase;
begin
  CreateTables;
  CreateIndexes;
  CreateTriggers;
end;

procedure TDatabaseManager.CreateTables;
begin
  // Users table
  ExecuteUpdate('CREATE TABLE IF NOT EXISTS users (' +
    '_id TEXT PRIMARY KEY, ' +
    'username TEXT UNIQUE NOT NULL, ' +
    'emails TEXT, ' +
    'profile TEXT, ' +
    'services TEXT, ' +
    'createdAt TEXT NOT NULL, ' +
    'modifiedAt TEXT, ' +
    'lastLogin TEXT, ' +
    'isAdmin BOOLEAN DEFAULT FALSE, ' +
    'isActive BOOLEAN DEFAULT TRUE' +
    ')');

  // Boards table
  ExecuteUpdate('CREATE TABLE IF NOT EXISTS boards (' +
    '_id TEXT PRIMARY KEY, ' +
    'title TEXT NOT NULL, ' +
    'slug TEXT NOT NULL, ' +
    'archived BOOLEAN DEFAULT FALSE, ' +
    'archivedAt TEXT, ' +
    'createdAt TEXT NOT NULL, ' +
    'modifiedAt TEXT, ' +
    'stars INTEGER DEFAULT 0, ' +
    'permission TEXT DEFAULT ''private'', ' +
    'color TEXT DEFAULT ''belize'', ' +
    'description TEXT, ' +
    'type TEXT DEFAULT ''board'', ' +
    'isTemplate BOOLEAN DEFAULT FALSE, ' +
    'isTemplateContainer BOOLEAN DEFAULT FALSE, ' +
    'members TEXT, ' +
    'labels TEXT, ' +
    'settings TEXT' +
    ')');

  // Lists table
  ExecuteUpdate('CREATE TABLE IF NOT EXISTS lists (' +
    '_id TEXT PRIMARY KEY, ' +
    'title TEXT NOT NULL, ' +
    'boardId TEXT NOT NULL REFERENCES boards(_id), ' +
    'archived BOOLEAN DEFAULT FALSE, ' +
    'archivedAt TEXT, ' +
    'createdAt TEXT NOT NULL, ' +
    'modifiedAt TEXT, ' +
    'sort INTEGER DEFAULT 0, ' +
    'wipLimit INTEGER, ' +
    'color TEXT, ' +
    'type TEXT DEFAULT ''list''' +
    ')');

  // Cards table
  ExecuteUpdate('CREATE TABLE IF NOT EXISTS cards (' +
    '_id TEXT PRIMARY KEY, ' +
    'title TEXT NOT NULL, ' +
    'listId TEXT NOT NULL REFERENCES lists(_id), ' +
    'boardId TEXT NOT NULL REFERENCES boards(_id), ' +
    'archived BOOLEAN DEFAULT FALSE, ' +
    'archivedAt TEXT, ' +
    'createdAt TEXT NOT NULL, ' +
    'modifiedAt TEXT, ' +
    'sort INTEGER DEFAULT 0, ' +
    'description TEXT, ' +
    'dueAt TEXT, ' +
    'coverId TEXT, ' +
    'color TEXT, ' +
    'members TEXT, ' +
    'labels TEXT, ' +
    'customFields TEXT, ' +
    'type TEXT DEFAULT ''card''' +
    ')');

  // Swimlanes table
  ExecuteUpdate('CREATE TABLE IF NOT EXISTS swimlanes (' +
    '_id TEXT PRIMARY KEY, ' +
    'title TEXT NOT NULL, ' +
    'boardId TEXT NOT NULL REFERENCES boards(_id), ' +
    'archived BOOLEAN DEFAULT FALSE, ' +
    'archivedAt TEXT, ' +
    'createdAt TEXT NOT NULL, ' +
    'modifiedAt TEXT, ' +
    'sort INTEGER DEFAULT 0, ' +
    'color TEXT, ' +
    'type TEXT DEFAULT ''swimlane''' +
    ')');

  // Activities table
  ExecuteUpdate('CREATE TABLE IF NOT EXISTS activities (' +
    '_id TEXT PRIMARY KEY, ' +
    'userId TEXT REFERENCES users(_id), ' +
    'username TEXT, ' +
    'memberId TEXT, ' +
    'type TEXT NOT NULL, ' +
    'activityTypeId TEXT, ' +
    'activityType TEXT, ' +
    'title TEXT, ' +
    'boardId TEXT REFERENCES boards(_id), ' +
    'oldBoardId TEXT, ' +
    'boardName TEXT, ' +
    'oldBoardName TEXT, ' +
    'swimlaneId TEXT REFERENCES swimlanes(_id), ' +
    'oldSwimlaneId TEXT, ' +
    'swimlaneName TEXT, ' +
    'listId TEXT REFERENCES lists(_id), ' +
    'oldListId TEXT, ' +
    'listName TEXT, ' +
    'cardId TEXT REFERENCES cards(_id), ' +
    'cardTitle TEXT, ' +
    'oldCardTitle TEXT, ' +
    'commentId TEXT, ' +
    'attachmentId TEXT, ' +
    'checklistId TEXT, ' +
    'checklistItemId TEXT, ' +
    'customFieldId TEXT, ' +
    'labelId TEXT, ' +
    'labelName TEXT, ' +
    'memberId TEXT, ' +
    'oldMemberId TEXT, ' +
    'memberName TEXT, ' +
    'oldMemberName TEXT, ' +
    'createdAt TEXT NOT NULL, ' +
    'modifiedAt TEXT' +
    ')');

  // Comments table
  ExecuteUpdate('CREATE TABLE IF NOT EXISTS comments (' +
    '_id TEXT PRIMARY KEY, ' +
    'cardId TEXT NOT NULL REFERENCES cards(_id), ' +
    'boardId TEXT NOT NULL REFERENCES boards(_id), ' +
    'text TEXT NOT NULL, ' +
    'authorId TEXT NOT NULL REFERENCES users(_id), ' +
    'createdAt TEXT NOT NULL, ' +
    'modifiedAt TEXT' +
    ')');

  // Attachments table
  ExecuteUpdate('CREATE TABLE IF NOT EXISTS attachments (' +
    '_id TEXT PRIMARY KEY, ' +
    'cardId TEXT NOT NULL REFERENCES cards(_id), ' +
    'boardId TEXT NOT NULL REFERENCES boards(_id), ' +
    'name TEXT NOT NULL, ' +
    'path TEXT NOT NULL, ' +
    'size INTEGER, ' +
    'type TEXT, ' +
    'extension TEXT, ' +
    'mimeType TEXT, ' +
    'userId TEXT NOT NULL REFERENCES users(_id), ' +
    'createdAt TEXT NOT NULL, ' +
    'modifiedAt TEXT' +
    ')');

  // Checklists table
  ExecuteUpdate('CREATE TABLE IF NOT EXISTS checklists (' +
    '_id TEXT PRIMARY KEY, ' +
    'cardId TEXT NOT NULL REFERENCES cards(_id), ' +
    'boardId TEXT NOT NULL REFERENCES boards(_id), ' +
    'title TEXT NOT NULL, ' +
    'sort INTEGER DEFAULT 0, ' +
    'createdAt TEXT NOT NULL, ' +
    'modifiedAt TEXT' +
    ')');

  // Checklist items table
  ExecuteUpdate('CREATE TABLE IF NOT EXISTS checklistItems (' +
    '_id TEXT PRIMARY KEY, ' +
    'checklistId TEXT NOT NULL REFERENCES checklists(_id), ' +
    'cardId TEXT NOT NULL REFERENCES cards(_id), ' +
    'boardId TEXT NOT NULL REFERENCES boards(_id), ' +
    'title TEXT NOT NULL, ' +
    'sort INTEGER DEFAULT 0, ' +
    'isFinished BOOLEAN DEFAULT FALSE, ' +
    'finishedAt TEXT, ' +
    'createdAt TEXT NOT NULL, ' +
    'modifiedAt TEXT' +
    ')');

  // Custom fields table
  ExecuteUpdate('CREATE TABLE IF NOT EXISTS customFields (' +
    '_id TEXT PRIMARY KEY, ' +
    'boardId TEXT NOT NULL REFERENCES boards(_id), ' +
    'name TEXT NOT NULL, ' +
    'type TEXT NOT NULL, ' +
    'settings TEXT, ' +
    'showOnCard BOOLEAN DEFAULT TRUE, ' +
    'showLabelOnMiniCard BOOLEAN DEFAULT FALSE, ' +
    'showSumAtTopOfList BOOLEAN DEFAULT FALSE, ' +
    'automaticallyOnCard BOOLEAN DEFAULT FALSE, ' +
    'createdAt TEXT NOT NULL, ' +
    'modifiedAt TEXT' +
    ')');

  // Custom field values table
  ExecuteUpdate('CREATE TABLE IF NOT EXISTS customFieldValues (' +
    '_id TEXT PRIMARY KEY, ' +
    'cardId TEXT NOT NULL REFERENCES cards(_id), ' +
    'boardId TEXT NOT NULL REFERENCES boards(_id), ' +
    'customFieldId TEXT NOT NULL REFERENCES customFields(_id), ' +
    'value TEXT, ' +
    'createdAt TEXT NOT NULL, ' +
    'modifiedAt TEXT' +
    ')');

  // Sessions table
  ExecuteUpdate('CREATE TABLE IF NOT EXISTS sessions (' +
    '_id TEXT PRIMARY KEY, ' +
    'userId TEXT NOT NULL REFERENCES users(_id), ' +
    'sessionId TEXT UNIQUE NOT NULL, ' +
    'ipAddress TEXT, ' +
    'userAgent TEXT, ' +
    'createdAt TEXT NOT NULL, ' +
    'expiresAt TEXT NOT NULL, ' +
    'isActive BOOLEAN DEFAULT TRUE' +
    ')');

  // Settings table
  ExecuteUpdate('CREATE TABLE IF NOT EXISTS settings (' +
    '_id TEXT PRIMARY KEY, ' +
    'key TEXT UNIQUE NOT NULL, ' +
    'value TEXT, ' +
    'createdAt TEXT NOT NULL, ' +
    'modifiedAt TEXT' +
    ')');

  // Organizations table
  ExecuteUpdate('CREATE TABLE IF NOT EXISTS organizations (' +
    '_id TEXT PRIMARY KEY, ' +
    'name TEXT NOT NULL, ' +
    'description TEXT, ' +
    'website TEXT, ' +
    'isActive BOOLEAN DEFAULT TRUE, ' +
    'createdAt TEXT NOT NULL, ' +
    'modifiedAt TEXT' +
    ')');

  // Organization users table
  ExecuteUpdate('CREATE TABLE IF NOT EXISTS organizationUsers (' +
    '_id TEXT PRIMARY KEY, ' +
    'organizationId TEXT NOT NULL REFERENCES organizations(_id), ' +
    'userId TEXT NOT NULL REFERENCES users(_id), ' +
    'role TEXT DEFAULT ''member'', ' +
    'createdAt TEXT NOT NULL, ' +
    'modifiedAt TEXT' +
    ')');

  // Teams table
  ExecuteUpdate('CREATE TABLE IF NOT EXISTS teams (' +
    '_id TEXT PRIMARY KEY, ' +
    'name TEXT NOT NULL, ' +
    'description TEXT, ' +
    'website TEXT, ' +
    'isActive BOOLEAN DEFAULT TRUE, ' +
    'createdAt TEXT NOT NULL, ' +
    'modifiedAt TEXT' +
    ')');

  // Team users table
  ExecuteUpdate('CREATE TABLE IF NOT EXISTS teamUsers (' +
    '_id TEXT PRIMARY KEY, ' +
    'teamId TEXT NOT NULL REFERENCES teams(_id), ' +
    'userId TEXT NOT NULL REFERENCES users(_id), ' +
    'role TEXT DEFAULT ''member'', ' +
    'createdAt TEXT NOT NULL, ' +
    'modifiedAt TEXT' +
    ')');

  // Rules table
  ExecuteUpdate('CREATE TABLE IF NOT EXISTS rules (' +
    '_id TEXT PRIMARY KEY, ' +
    'boardId TEXT NOT NULL REFERENCES boards(_id), ' +
    'title TEXT NOT NULL, ' +
    'description TEXT, ' +
    'trigger TEXT NOT NULL, ' +
    'action TEXT NOT NULL, ' +
    'isActive BOOLEAN DEFAULT TRUE, ' +
    'createdAt TEXT NOT NULL, ' +
    'modifiedAt TEXT' +
    ')');

  // Triggers table
  ExecuteUpdate('CREATE TABLE IF NOT EXISTS triggers (' +
    '_id TEXT PRIMARY KEY, ' +
    'ruleId TEXT NOT NULL REFERENCES rules(_id), ' +
    'type TEXT NOT NULL, ' +
    'conditions TEXT, ' +
    'createdAt TEXT NOT NULL, ' +
    'modifiedAt TEXT' +
    ')');

  // Actions table
  ExecuteUpdate('CREATE TABLE IF NOT EXISTS actions (' +
    '_id TEXT PRIMARY KEY, ' +
    'ruleId TEXT NOT NULL REFERENCES rules(_id), ' +
    'type TEXT NOT NULL, ' +
    'parameters TEXT, ' +
    'createdAt TEXT NOT NULL, ' +
    'modifiedAt TEXT' +
    ')');

  // Import/Export history table
  ExecuteUpdate('CREATE TABLE IF NOT EXISTS importExportHistory (' +
    '_id TEXT PRIMARY KEY, ' +
    'userId TEXT NOT NULL REFERENCES users(_id), ' +
    'type TEXT NOT NULL, ' +
    'source TEXT, ' +
    'destination TEXT, ' +
    'status TEXT DEFAULT ''pending'', ' +
    'progress INTEGER DEFAULT 0, ' +
    'errorMessage TEXT, ' +
    'createdAt TEXT NOT NULL, ' +
    'completedAt TEXT' +
    ')');
end;

procedure TDatabaseManager.CreateIndexes;
begin
  // Users indexes
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_users_username ON users(username)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_users_email ON users(emails)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_users_createdAt ON users(createdAt)');

  // Boards indexes
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_boards_slug ON boards(slug)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_boards_createdAt ON boards(createdAt)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_boards_permission ON boards(permission)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_boards_type ON boards(type)');

  // Lists indexes
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_lists_boardId ON lists(boardId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_lists_sort ON lists(sort)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_lists_createdAt ON lists(createdAt)');

  // Cards indexes
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_cards_listId ON cards(listId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_cards_boardId ON cards(boardId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_cards_sort ON cards(sort)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_cards_createdAt ON cards(createdAt)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_cards_dueAt ON cards(dueAt)');

  // Swimlanes indexes
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_swimlanes_boardId ON swimlanes(boardId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_swimlanes_sort ON swimlanes(sort)');

  // Activities indexes
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_activities_userId ON activities(userId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_activities_boardId ON activities(boardId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_activities_cardId ON activities(cardId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_activities_createdAt ON activities(createdAt)');

  // Comments indexes
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_comments_cardId ON comments(cardId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_comments_boardId ON comments(boardId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_comments_authorId ON comments(authorId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_comments_createdAt ON comments(createdAt)');

  // Attachments indexes
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_attachments_cardId ON attachments(cardId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_attachments_boardId ON attachments(boardId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_attachments_userId ON attachments(userId)');

  // Checklists indexes
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_checklists_cardId ON checklists(cardId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_checklists_boardId ON checklists(boardId)');

  // Checklist items indexes
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_checklistItems_checklistId ON checklistItems(checklistId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_checklistItems_cardId ON checklistItems(cardId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_checklistItems_boardId ON checklistItems(boardId)');

  // Custom fields indexes
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_customFields_boardId ON customFields(boardId)');

  // Custom field values indexes
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_customFieldValues_cardId ON customFieldValues(cardId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_customFieldValues_boardId ON customFieldValues(boardId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_customFieldValues_customFieldId ON customFieldValues(customFieldId)');

  // Sessions indexes
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_sessions_userId ON sessions(userId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_sessions_sessionId ON sessions(sessionId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_sessions_expiresAt ON sessions(expiresAt)');

  // Settings indexes
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_settings_key ON settings(key)');

  // Organization indexes
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_organizationUsers_organizationId ON organizationUsers(organizationId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_organizationUsers_userId ON organizationUsers(userId)');

  // Team indexes
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_teamUsers_teamId ON teamUsers(teamId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_teamUsers_userId ON teamUsers(userId)');

  // Rules indexes
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_rules_boardId ON rules(boardId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_triggers_ruleId ON triggers(ruleId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_actions_ruleId ON actions(ruleId)');

  // Import/Export history indexes
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_importExportHistory_userId ON importExportHistory(userId)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_importExportHistory_type ON importExportHistory(type)');
  ExecuteUpdate('CREATE INDEX IF NOT EXISTS idx_importExportHistory_status ON importExportHistory(status)');
end;

procedure TDatabaseManager.CreateTriggers;
begin
  // Update modifiedAt timestamp triggers
  ExecuteUpdate('CREATE TRIGGER IF NOT EXISTS update_users_modifiedAt ' +
    'AFTER UPDATE ON users ' +
    'BEGIN ' +
    '  UPDATE users SET modifiedAt = datetime(''now'') WHERE _id = NEW._id; ' +
    'END');

  ExecuteUpdate('CREATE TRIGGER IF NOT EXISTS update_boards_modifiedAt ' +
    'AFTER UPDATE ON boards ' +
    'BEGIN ' +
    '  UPDATE boards SET modifiedAt = datetime(''now'') WHERE _id = NEW._id; ' +
    'END');

  ExecuteUpdate('CREATE TRIGGER IF NOT EXISTS update_lists_modifiedAt ' +
    'AFTER UPDATE ON lists ' +
    'BEGIN ' +
    '  UPDATE lists SET modifiedAt = datetime(''now'') WHERE _id = NEW._id; ' +
    'END');

  ExecuteUpdate('CREATE TRIGGER IF NOT EXISTS update_cards_modifiedAt ' +
    'AFTER UPDATE ON cards ' +
    'BEGIN ' +
    '  UPDATE cards SET modifiedAt = datetime(''now'') WHERE _id = NEW._id; ' +
    'END');

  // Update board modifiedAt when list changes
  ExecuteUpdate('CREATE TRIGGER IF NOT EXISTS update_board_on_list_change ' +
    'AFTER INSERT OR UPDATE OR DELETE ON lists ' +
    'BEGIN ' +
    '  UPDATE boards SET modifiedAt = datetime(''now'') WHERE _id = COALESCE(NEW.boardId, OLD.boardId); ' +
    'END');

  // Update board modifiedAt when card changes
  ExecuteUpdate('CREATE TRIGGER IF NOT EXISTS update_board_on_card_change ' +
    'AFTER INSERT OR UPDATE OR DELETE ON cards ' +
    'BEGIN ' +
    '  UPDATE boards SET modifiedAt = datetime(''now'') WHERE _id = COALESCE(NEW.boardId, OLD.boardId); ' +
    'END');

  // Update list modifiedAt when card changes
  ExecuteUpdate('CREATE TRIGGER IF NOT EXISTS update_list_on_card_change ' +
    'AFTER INSERT OR UPDATE OR DELETE ON cards ' +
    'BEGIN ' +
    '  UPDATE lists SET modifiedAt = datetime(''now'') WHERE _id = COALESCE(NEW.listId, OLD.listId); ' +
    'END');
end;

function TDatabaseManager.ExecuteQuery(const SQL: string): TSQLQuery;
begin
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.Open;
  Result := FQuery;
end;

function TDatabaseManager.ExecuteUpdate(const SQL: string): Integer;
begin
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ExecSQL;
  FTransaction.Commit;
  Result := FQuery.RowsAffected;
end;

function TDatabaseManager.GetLastInsertId: string;
begin
  Result := FDatabase.GetInsertID.ToString;
end;

// TMigrationManager implementation
constructor TMigrationManager.Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction);
begin
  inherited Create;
  FDatabase := ADatabase;
  FTransaction := ATransaction;
  CreateMigrationTable;
end;

destructor TMigrationManager.Destroy;
begin
  inherited Destroy;
end;

procedure TMigrationManager.RunMigrations;
var
  CurrentVersion: Integer;
begin
  CurrentVersion := GetCurrentVersion;
  
  // Run migrations in order
  if CurrentVersion < 1 then
  begin
    ExecuteMigration(1, '-- Migration 1: Initial schema');
    SetVersion(1);
  end;
  
  // Add more migrations as needed
  // if CurrentVersion < 2 then
  // begin
  //   ExecuteMigration(2, '-- Migration 2: Add new features');
  //   SetVersion(2);
  // end;
end;

procedure TMigrationManager.CreateMigrationTable;
begin
  // Create migrations table if it doesn't exist
  FDatabase.ExecuteDirect('CREATE TABLE IF NOT EXISTS migrations (' +
    'version INTEGER PRIMARY KEY, ' +
    'applied_at TEXT NOT NULL' +
    ')');
  FTransaction.Commit;
end;

function TMigrationManager.GetCurrentVersion: Integer;
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabase;
    Query.Transaction := FTransaction;
    Query.SQL.Text := 'SELECT MAX(version) as version FROM migrations';
    Query.Open;
    
    if Query.FieldByName('version').IsNull then
      Result := 0
    else
      Result := Query.FieldByName('version').AsInteger;
  finally
    Query.Free;
  end;
end;

procedure TMigrationManager.SetVersion(const Version: Integer);
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.Database := FDatabase;
    Query.Transaction := FTransaction;
    Query.SQL.Text := 'INSERT OR REPLACE INTO migrations (version, applied_at) VALUES (:version, datetime(''now''))';
    Query.ParamByName('version').AsInteger := Version;
    Query.ExecSQL;
    FTransaction.Commit;
  finally
    Query.Free;
  end;
end;

procedure TMigrationManager.ExecuteMigration(const Version: Integer; const SQL: string);
begin
  // Execute migration SQL
  FDatabase.ExecuteDirect(SQL);
  FTransaction.Commit;
end;

end.
