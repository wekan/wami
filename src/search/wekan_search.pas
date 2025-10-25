unit wekan_search;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, sqldb, sqlite3conn, wekan_core, wekan_database, wekan_models;

type
  // Search result types
  TSearchResultType = (srtBoard, srtList, srtCard, srtUser, srtComment, srtAttachment);

  // Search result item
  TSearchResult = class
  private
    FId: string;
    FType: TSearchResultType;
    FTitle: string;
    FDescription: string;
    FBoardId: string;
    FBoardTitle: string;
    FRelevance: Double;
    FHighlight: string;
  public
    constructor Create;
    destructor Destroy; override;
    
    property Id: string read FId write FId;
    property Type: TSearchResultType read FType write FType;
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    property BoardId: string read FBoardId write FBoardId;
    property BoardTitle: string read FBoardTitle write FBoardTitle;
    property Relevance: Double read FRelevance write FRelevance;
    property Highlight: string read FHighlight write FHighlight;
    
    function ToJSON: TJSONObject;
  end;

  // Search query
  TSearchQuery = class
  private
    FQuery: string;
    FBoardId: string;
    FUserId: string;
    FTypes: TStringArray;
    FLimit: Integer;
    FOffset: Integer;
    FSortBy: string;
    FSortOrder: string;
  public
    constructor Create;
    destructor Destroy; override;
    
    property Query: string read FQuery write FQuery;
    property BoardId: string read FBoardId write FBoardId;
    property UserId: string read FUserId write FUserId;
    property Types: TStringArray read FTypes write FTypes;
    property Limit: Integer read FLimit write FLimit;
    property Offset: Integer read FOffset write FOffset;
    property SortBy: string read FSortBy write FSortBy;
    property SortOrder: string read FSortOrder write FSortOrder;
  end;

  // Search manager
  TSearchManager = class(TWeKanModel)
  private
    FBoardManager: TBoardManager;
    FListManager: TListManager;
    FCardManager: TCardManager;
    FUserManager: TUserManager;
  public
    constructor Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction;
      ABoardManager: TBoardManager; AListManager: TListManager; 
      ACardManager: TCardManager; AUserManager: TUserManager);
    destructor Destroy; override;
    
    // Search functions
    function Search(const SearchQuery: TSearchQuery): TJSONArray;
    function SearchBoards(const Query: string; const UserId: string; const Limit: Integer = 20): TJSONArray;
    function SearchLists(const Query: string; const BoardId: string; const UserId: string; const Limit: Integer = 20): TJSONArray;
    function SearchCards(const Query: string; const BoardId: string; const UserId: string; const Limit: Integer = 20): TJSONArray;
    function SearchUsers(const Query: string; const Limit: Integer = 20): TJSONArray;
    function SearchComments(const Query: string; const BoardId: string; const UserId: string; const Limit: Integer = 20): TJSONArray;
    
    // Advanced search
    function SearchByDateRange(const StartDate, EndDate: string; const BoardId: string; const UserId: string): TJSONArray;
    function SearchByUser(const UserId: string; const BoardId: string; const Limit: Integer = 20): TJSONArray;
    function SearchByLabel(const LabelId: string; const BoardId: string; const UserId: string): TJSONArray;
    function SearchByCustomField(const FieldId: string; const Value: string; const BoardId: string; const UserId: string): TJSONArray;
    
    // Search suggestions
    function GetSearchSuggestions(const Query: string; const UserId: string): TJSONArray;
    function GetRecentSearches(const UserId: string): TJSONArray;
    function SaveSearchHistory(const Query: string; const UserId: string): Boolean;
    
    // Full-text search
    function CreateFullTextIndex: Boolean;
    function RebuildFullTextIndex: Boolean;
    function DropFullTextIndex: Boolean;
  end;

  // Search indexer
  TSearchIndexer = class(TWeKanModel)
  public
    constructor Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction);
    destructor Destroy; override;
    
    function IndexBoard(const Board: TBoard): Boolean;
    function IndexList(const List: TList): Boolean;
    function IndexCard(const Card: TCard): Boolean;
    function IndexUser(const User: TUser): Boolean;
    function IndexComment(const Comment: TJSONObject): Boolean;
    function IndexAttachment(const Attachment: TJSONObject): Boolean;
    
    function RemoveFromIndex(const Id: string; const Type: TSearchResultType): Boolean;
    function UpdateIndex(const Id: string; const Type: TSearchResultType; const Data: TJSONObject): Boolean;
  end;

// Utility functions
function ResultTypeToString(const ResultType: TSearchResultType): string;
function StringToResultType(const Str: string): TSearchResultType;
function CalculateRelevance(const Query: string; const Text: string): Double;
function HighlightText(const Text: string; const Query: string): string;
function TokenizeQuery(const Query: string): TStringArray;
function BuildSearchSQL(const Query: string; const Table: string; const Fields: TStringArray; const Conditions: string): string;

implementation

uses
  strutils, dateutils, regexpr, wekan_utils;

// TSearchResult implementation
constructor TSearchResult.Create;
begin
  inherited Create;
  FRelevance := 0.0;
end;

destructor TSearchResult.Destroy;
begin
  inherited Destroy;
end;

function TSearchResult.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('_id', FId);
  Result.Add('type', ResultTypeToString(FType));
  Result.Add('title', FTitle);
  Result.Add('description', FDescription);
  Result.Add('boardId', FBoardId);
  Result.Add('boardTitle', FBoardTitle);
  Result.Add('relevance', FRelevance);
  Result.Add('highlight', FHighlight);
end;

// TSearchQuery implementation
constructor TSearchQuery.Create;
begin
  inherited Create;
  FLimit := 20;
  FOffset := 0;
  FSortBy := 'relevance';
  FSortOrder := 'desc';
  SetLength(FTypes, 0);
end;

destructor TSearchQuery.Destroy;
begin
  inherited Destroy;
end;

// TSearchManager implementation
constructor TSearchManager.Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction;
  ABoardManager: TBoardManager; AListManager: TListManager; 
  ACardManager: TCardManager; AUserManager: TUserManager);
begin
  inherited Create(ADatabase, ATransaction);
  FBoardManager := ABoardManager;
  FListManager := AListManager;
  FCardManager := ACardManager;
  FUserManager := AUserManager;
end;

destructor TSearchManager.Destroy;
begin
  inherited Destroy;
end;

function TSearchManager.Search(const SearchQuery: TSearchQuery): TJSONArray;
var
  Results: TJSONArray;
  i: Integer;
  TypeStr: string;
begin
  Results := TJSONArray.Create;
  
  try
    // If no specific types requested, search all
    if Length(SearchQuery.Types) = 0 then
    begin
      // Search boards
      Results.Add(SearchBoards(SearchQuery.Query, SearchQuery.UserId, SearchQuery.Limit));
      
      // Search lists
      Results.Add(SearchLists(SearchQuery.Query, SearchQuery.BoardId, SearchQuery.UserId, SearchQuery.Limit));
      
      // Search cards
      Results.Add(SearchCards(SearchQuery.Query, SearchQuery.BoardId, SearchQuery.UserId, SearchQuery.Limit));
      
      // Search users
      Results.Add(SearchUsers(SearchQuery.Query, SearchQuery.Limit));
      
      // Search comments
      Results.Add(SearchComments(SearchQuery.Query, SearchQuery.BoardId, SearchQuery.UserId, SearchQuery.Limit));
    end
    else
    begin
      // Search specific types
      for i := 0 to High(SearchQuery.Types) do
      begin
        TypeStr := SearchQuery.Types[i];
        case StringToResultType(TypeStr) of
          srtBoard:
            Results.Add(SearchBoards(SearchQuery.Query, SearchQuery.UserId, SearchQuery.Limit));
          srtList:
            Results.Add(SearchLists(SearchQuery.Query, SearchQuery.BoardId, SearchQuery.UserId, SearchQuery.Limit));
          srtCard:
            Results.Add(SearchCards(SearchQuery.Query, SearchQuery.BoardId, SearchQuery.UserId, SearchQuery.Limit));
          srtUser:
            Results.Add(SearchUsers(SearchQuery.Query, SearchQuery.Limit));
          srtComment:
            Results.Add(SearchComments(SearchQuery.Query, SearchQuery.BoardId, SearchQuery.UserId, SearchQuery.Limit));
        end;
      end;
    end;
    
    Result := Results;
  except
    on E: Exception do
    begin
      Results.Free;
      Result := TJSONArray.Create;
      LogError('Search error: ' + E.Message);
    end;
  end;
end;

function TSearchManager.SearchBoards(const Query: string; const UserId: string; const Limit: Integer): TJSONArray;
var
  Results: TJSONArray;
  SQL: string;
  Tokens: TStringArray;
  i: Integer;
  Token: string;
  WhereClause: string;
begin
  Results := TJSONArray.Create;
  
  if Query = '' then
  begin
    Result := Results;
    Exit;
  end;
  
  Tokens := TokenizeQuery(Query);
  WhereClause := '';
  
  // Build WHERE clause for each token
  for i := 0 to High(Tokens) do
  begin
    Token := Tokens[i];
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    WhereClause := WhereClause + '(title LIKE :token' + IntToStr(i) + ' OR description LIKE :token' + IntToStr(i) + ')';
  end;
  
  // Add user permission check
  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND ';
  WhereClause := WhereClause + '(members LIKE :userId OR permission = ''public'')';
  
  SQL := 'SELECT _id, title, description, createdAt, modifiedAt, permission, color ' +
         'FROM boards WHERE ' + WhereClause + ' AND archived = FALSE ' +
         'ORDER BY modifiedAt DESC LIMIT :limit';
  
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  
  // Set parameters
  for i := 0 to High(Tokens) do
  begin
    FQuery.ParamByName('token' + IntToStr(i)).AsString := '%' + Tokens[i] + '%';
  end;
  FQuery.ParamByName('userId').AsString := '%' + UserId + '%';
  FQuery.ParamByName('limit').AsInteger := Limit;
  
  FQuery.Open;
  
  while not FQuery.Eof do
  begin
    Results.Add(TJSONObject.Create([
      '_id', FQuery.FieldByName('_id').AsString,
      'type', 'board',
      'title', FQuery.FieldByName('title').AsString,
      'description', FQuery.FieldByName('description').AsString,
      'createdAt', FQuery.FieldByName('createdAt').AsString,
      'modifiedAt', FQuery.FieldByName('modifiedAt').AsString,
      'permission', FQuery.FieldByName('permission').AsString,
      'color', FQuery.FieldByName('color').AsString,
      'relevance', CalculateRelevance(Query, FQuery.FieldByName('title').AsString + ' ' + FQuery.FieldByName('description').AsString),
      'highlight', HighlightText(FQuery.FieldByName('title').AsString, Query)
    ]));
    FQuery.Next;
  end;
  
  Result := Results;
end;

function TSearchManager.SearchLists(const Query: string; const BoardId: string; const UserId: string; const Limit: Integer): TJSONArray;
var
  Results: TJSONArray;
  SQL: string;
  Tokens: TStringArray;
  i: Integer;
  Token: string;
  WhereClause: string;
begin
  Results := TJSONArray.Create;
  
  if Query = '' then
  begin
    Result := Results;
    Exit;
  end;
  
  Tokens := TokenizeQuery(Query);
  WhereClause := '';
  
  // Build WHERE clause for each token
  for i := 0 to High(Tokens) do
  begin
    Token := Tokens[i];
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    WhereClause := WhereClause + 'l.title LIKE :token' + IntToStr(i);
  end;
  
  // Add board permission check
  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND ';
  WhereClause := WhereClause + '(b.members LIKE :userId OR b.permission = ''public'')';
  
  if BoardId <> '' then
  begin
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    WhereClause := WhereClause + 'l.boardId = :boardId';
  end;
  
  SQL := 'SELECT l._id, l.title, l.boardId, b.title as boardTitle, l.createdAt, l.modifiedAt ' +
         'FROM lists l ' +
         'JOIN boards b ON l.boardId = b._id ' +
         'WHERE ' + WhereClause + ' AND l.archived = FALSE ' +
         'ORDER BY l.modifiedAt DESC LIMIT :limit';
  
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  
  // Set parameters
  for i := 0 to High(Tokens) do
  begin
    FQuery.ParamByName('token' + IntToStr(i)).AsString := '%' + Tokens[i] + '%';
  end;
  FQuery.ParamByName('userId').AsString := '%' + UserId + '%';
  if BoardId <> '' then
    FQuery.ParamByName('boardId').AsString := BoardId;
  FQuery.ParamByName('limit').AsInteger := Limit;
  
  FQuery.Open;
  
  while not FQuery.Eof do
  begin
    Results.Add(TJSONObject.Create([
      '_id', FQuery.FieldByName('_id').AsString,
      'type', 'list',
      'title', FQuery.FieldByName('title').AsString,
      'boardId', FQuery.FieldByName('boardId').AsString,
      'boardTitle', FQuery.FieldByName('boardTitle').AsString,
      'createdAt', FQuery.FieldByName('createdAt').AsString,
      'modifiedAt', FQuery.FieldByName('modifiedAt').AsString,
      'relevance', CalculateRelevance(Query, FQuery.FieldByName('title').AsString),
      'highlight', HighlightText(FQuery.FieldByName('title').AsString, Query)
    ]));
    FQuery.Next;
  end;
  
  Result := Results;
end;

function TSearchManager.SearchCards(const Query: string; const BoardId: string; const UserId: string; const Limit: Integer): TJSONArray;
var
  Results: TJSONArray;
  SQL: string;
  Tokens: TStringArray;
  i: Integer;
  Token: string;
  WhereClause: string;
begin
  Results := TJSONArray.Create;
  
  if Query = '' then
  begin
    Result := Results;
    Exit;
  end;
  
  Tokens := TokenizeQuery(Query);
  WhereClause := '';
  
  // Build WHERE clause for each token
  for i := 0 to High(Tokens) do
  begin
    Token := Tokens[i];
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    WhereClause := WhereClause + '(c.title LIKE :token' + IntToStr(i) + ' OR c.description LIKE :token' + IntToStr(i) + ')';
  end;
  
  // Add board permission check
  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND ';
  WhereClause := WhereClause + '(b.members LIKE :userId OR b.permission = ''public'')';
  
  if BoardId <> '' then
  begin
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    WhereClause := WhereClause + 'c.boardId = :boardId';
  end;
  
  SQL := 'SELECT c._id, c.title, c.description, c.boardId, c.listId, b.title as boardTitle, l.title as listTitle, ' +
         'c.createdAt, c.modifiedAt, c.dueAt, c.color ' +
         'FROM cards c ' +
         'JOIN boards b ON c.boardId = b._id ' +
         'JOIN lists l ON c.listId = l._id ' +
         'WHERE ' + WhereClause + ' AND c.archived = FALSE ' +
         'ORDER BY c.modifiedAt DESC LIMIT :limit';
  
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  
  // Set parameters
  for i := 0 to High(Tokens) do
  begin
    FQuery.ParamByName('token' + IntToStr(i)).AsString := '%' + Tokens[i] + '%';
  end;
  FQuery.ParamByName('userId').AsString := '%' + UserId + '%';
  if BoardId <> '' then
    FQuery.ParamByName('boardId').AsString := BoardId;
  FQuery.ParamByName('limit').AsInteger := Limit;
  
  FQuery.Open;
  
  while not FQuery.Eof do
  begin
    Results.Add(TJSONObject.Create([
      '_id', FQuery.FieldByName('_id').AsString,
      'type', 'card',
      'title', FQuery.FieldByName('title').AsString,
      'description', FQuery.FieldByName('description').AsString,
      'boardId', FQuery.FieldByName('boardId').AsString,
      'listId', FQuery.FieldByName('listId').AsString,
      'boardTitle', FQuery.FieldByName('boardTitle').AsString,
      'listTitle', FQuery.FieldByName('listTitle').AsString,
      'createdAt', FQuery.FieldByName('createdAt').AsString,
      'modifiedAt', FQuery.FieldByName('modifiedAt').AsString,
      'dueAt', FQuery.FieldByName('dueAt').AsString,
      'color', FQuery.FieldByName('color').AsString,
      'relevance', CalculateRelevance(Query, FQuery.FieldByName('title').AsString + ' ' + FQuery.FieldByName('description').AsString),
      'highlight', HighlightText(FQuery.FieldByName('title').AsString, Query)
    ]));
    FQuery.Next;
  end;
  
  Result := Results;
end;

function TSearchManager.SearchUsers(const Query: string; const Limit: Integer): TJSONArray;
var
  Results: TJSONArray;
  SQL: string;
  Tokens: TStringArray;
  i: Integer;
  Token: string;
  WhereClause: string;
begin
  Results := TJSONArray.Create;
  
  if Query = '' then
  begin
    Result := Results;
    Exit;
  end;
  
  Tokens := TokenizeQuery(Query);
  WhereClause := '';
  
  // Build WHERE clause for each token
  for i := 0 to High(Tokens) do
  begin
    Token := Tokens[i];
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    WhereClause := WhereClause + '(username LIKE :token' + IntToStr(i) + ' OR emails LIKE :token' + IntToStr(i) + ')';
  end;
  
  SQL := 'SELECT _id, username, emails, createdAt, lastLogin, isActive ' +
         'FROM users WHERE ' + WhereClause + ' AND isActive = TRUE ' +
         'ORDER BY username ASC LIMIT :limit';
  
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  
  // Set parameters
  for i := 0 to High(Tokens) do
  begin
    FQuery.ParamByName('token' + IntToStr(i)).AsString := '%' + Tokens[i] + '%';
  end;
  FQuery.ParamByName('limit').AsInteger := Limit;
  
  FQuery.Open;
  
  while not FQuery.Eof do
  begin
    Results.Add(TJSONObject.Create([
      '_id', FQuery.FieldByName('_id').AsString,
      'type', 'user',
      'title', FQuery.FieldByName('username').AsString,
      'description', FQuery.FieldByName('emails').AsString,
      'createdAt', FQuery.FieldByName('createdAt').AsString,
      'lastLogin', FQuery.FieldByName('lastLogin').AsString,
      'isActive', FQuery.FieldByName('isActive').AsBoolean,
      'relevance', CalculateRelevance(Query, FQuery.FieldByName('username').AsString + ' ' + FQuery.FieldByName('emails').AsString),
      'highlight', HighlightText(FQuery.FieldByName('username').AsString, Query)
    ]));
    FQuery.Next;
  end;
  
  Result := Results;
end;

function TSearchManager.SearchComments(const Query: string; const BoardId: string; const UserId: string; const Limit: Integer): TJSONArray;
var
  Results: TJSONArray;
  SQL: string;
  Tokens: TStringArray;
  i: Integer;
  Token: string;
  WhereClause: string;
begin
  Results := TJSONArray.Create;
  
  if Query = '' then
  begin
    Result := Results;
    Exit;
  end;
  
  Tokens := TokenizeQuery(Query);
  WhereClause := '';
  
  // Build WHERE clause for each token
  for i := 0 to High(Tokens) do
  begin
    Token := Tokens[i];
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    WhereClause := WhereClause + 'c.text LIKE :token' + IntToStr(i);
  end;
  
  // Add board permission check
  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND ';
  WhereClause := WhereClause + '(b.members LIKE :userId OR b.permission = ''public'')';
  
  if BoardId <> '' then
  begin
    if WhereClause <> '' then
      WhereClause := WhereClause + ' AND ';
    WhereClause := WhereClause + 'c.boardId = :boardId';
  end;
  
  SQL := 'SELECT c._id, c.text, c.cardId, c.boardId, b.title as boardTitle, u.username, c.createdAt ' +
         'FROM comments c ' +
         'JOIN boards b ON c.boardId = b._id ' +
         'JOIN users u ON c.authorId = u._id ' +
         'WHERE ' + WhereClause + ' ' +
         'ORDER BY c.createdAt DESC LIMIT :limit';
  
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  
  // Set parameters
  for i := 0 to High(Tokens) do
  begin
    FQuery.ParamByName('token' + IntToStr(i)).AsString := '%' + Tokens[i] + '%';
  end;
  FQuery.ParamByName('userId').AsString := '%' + UserId + '%';
  if BoardId <> '' then
    FQuery.ParamByName('boardId').AsString := BoardId;
  FQuery.ParamByName('limit').AsInteger := Limit;
  
  FQuery.Open;
  
  while not FQuery.Eof do
  begin
    Results.Add(TJSONObject.Create([
      '_id', FQuery.FieldByName('_id').AsString,
      'type', 'comment',
      'title', Copy(FQuery.FieldByName('text').AsString, 1, 100) + '...',
      'description', FQuery.FieldByName('text').AsString,
      'cardId', FQuery.FieldByName('cardId').AsString,
      'boardId', FQuery.FieldByName('boardId').AsString,
      'boardTitle', FQuery.FieldByName('boardTitle').AsString,
      'author', FQuery.FieldByName('username').AsString,
      'createdAt', FQuery.FieldByName('createdAt').AsString,
      'relevance', CalculateRelevance(Query, FQuery.FieldByName('text').AsString),
      'highlight', HighlightText(FQuery.FieldByName('text').AsString, Query)
    ]));
    FQuery.Next;
  end;
  
  Result := Results;
end;

// Advanced search functions
function TSearchManager.SearchByDateRange(const StartDate, EndDate: string; const BoardId: string; const UserId: string): TJSONArray;
var
  Results: TJSONArray;
  SQL: string;
  WhereClause: string;
begin
  Results := TJSONArray.Create;
  
  WhereClause := 'c.createdAt BETWEEN :startDate AND :endDate';
  
  if BoardId <> '' then
    WhereClause := WhereClause + ' AND c.boardId = :boardId';
  
  WhereClause := WhereClause + ' AND (b.members LIKE :userId OR b.permission = ''public'')';
  
  SQL := 'SELECT c._id, c.title, c.description, c.boardId, b.title as boardTitle, c.createdAt ' +
         'FROM cards c ' +
         'JOIN boards b ON c.boardId = b._id ' +
         'WHERE ' + WhereClause + ' AND c.archived = FALSE ' +
         'ORDER BY c.createdAt DESC';
  
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('startDate').AsString := StartDate;
  FQuery.ParamByName('endDate').AsString := EndDate;
  if BoardId <> '' then
    FQuery.ParamByName('boardId').AsString := BoardId;
  FQuery.ParamByName('userId').AsString := '%' + UserId + '%';
  
  FQuery.Open;
  
  while not FQuery.Eof do
  begin
    Results.Add(TJSONObject.Create([
      '_id', FQuery.FieldByName('_id').AsString,
      'type', 'card',
      'title', FQuery.FieldByName('title').AsString,
      'description', FQuery.FieldByName('description').AsString,
      'boardId', FQuery.FieldByName('boardId').AsString,
      'boardTitle', FQuery.FieldByName('boardTitle').AsString,
      'createdAt', FQuery.FieldByName('createdAt').AsString
    ]));
    FQuery.Next;
  end;
  
  Result := Results;
end;

function TSearchManager.SearchByUser(const UserId: string; const BoardId: string; const Limit: Integer): TJSONArray;
var
  Results: TJSONArray;
  SQL: string;
  WhereClause: string;
begin
  Results := TJSONArray.Create;
  
  WhereClause := 'c.members LIKE :userId';
  
  if BoardId <> '' then
    WhereClause := WhereClause + ' AND c.boardId = :boardId';
  
  WhereClause := WhereClause + ' AND (b.members LIKE :userId OR b.permission = ''public'')';
  
  SQL := 'SELECT c._id, c.title, c.description, c.boardId, b.title as boardTitle, c.createdAt ' +
         'FROM cards c ' +
         'JOIN boards b ON c.boardId = b._id ' +
         'WHERE ' + WhereClause + ' AND c.archived = FALSE ' +
         'ORDER BY c.modifiedAt DESC LIMIT :limit';
  
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('userId').AsString := '%' + UserId + '%';
  if BoardId <> '' then
    FQuery.ParamByName('boardId').AsString := BoardId;
  FQuery.ParamByName('limit').AsInteger := Limit;
  
  FQuery.Open;
  
  while not FQuery.Eof do
  begin
    Results.Add(TJSONObject.Create([
      '_id', FQuery.FieldByName('_id').AsString,
      'type', 'card',
      'title', FQuery.FieldByName('title').AsString,
      'description', FQuery.FieldByName('description').AsString,
      'boardId', FQuery.FieldByName('boardId').AsString,
      'boardTitle', FQuery.FieldByName('boardTitle').AsString,
      'createdAt', FQuery.FieldByName('createdAt').AsString
    ]));
    FQuery.Next;
  end;
  
  Result := Results;
end;

function TSearchManager.SearchByLabel(const LabelId: string; const BoardId: string; const UserId: string): TJSONArray;
var
  Results: TJSONArray;
  SQL: string;
  WhereClause: string;
begin
  Results := TJSONArray.Create;
  
  WhereClause := 'c.labels LIKE :labelId';
  
  if BoardId <> '' then
    WhereClause := WhereClause + ' AND c.boardId = :boardId';
  
  WhereClause := WhereClause + ' AND (b.members LIKE :userId OR b.permission = ''public'')';
  
  SQL := 'SELECT c._id, c.title, c.description, c.boardId, b.title as boardTitle, c.createdAt ' +
         'FROM cards c ' +
         'JOIN boards b ON c.boardId = b._id ' +
         'WHERE ' + WhereClause + ' AND c.archived = FALSE ' +
         'ORDER BY c.modifiedAt DESC';
  
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('labelId').AsString := '%' + LabelId + '%';
  if BoardId <> '' then
    FQuery.ParamByName('boardId').AsString := BoardId;
  FQuery.ParamByName('userId').AsString := '%' + UserId + '%';
  
  FQuery.Open;
  
  while not FQuery.Eof do
  begin
    Results.Add(TJSONObject.Create([
      '_id', FQuery.FieldByName('_id').AsString,
      'type', 'card',
      'title', FQuery.FieldByName('title').AsString,
      'description', FQuery.FieldByName('description').AsString,
      'boardId', FQuery.FieldByName('boardId').AsString,
      'boardTitle', FQuery.FieldByName('boardTitle').AsString,
      'createdAt', FQuery.FieldByName('createdAt').AsString
    ]));
    FQuery.Next;
  end;
  
  Result := Results;
end;

function TSearchManager.SearchByCustomField(const FieldId: string; const Value: string; const BoardId: string; const UserId: string): TJSONArray;
var
  Results: TJSONArray;
  SQL: string;
  WhereClause: string;
begin
  Results := TJSONArray.Create;
  
  WhereClause := 'cf.customFieldId = :fieldId AND cf.value LIKE :value';
  
  if BoardId <> '' then
    WhereClause := WhereClause + ' AND cf.boardId = :boardId';
  
  WhereClause := WhereClause + ' AND (b.members LIKE :userId OR b.permission = ''public'')';
  
  SQL := 'SELECT c._id, c.title, c.description, c.boardId, b.title as boardTitle, cf.value, c.createdAt ' +
         'FROM customFieldValues cf ' +
         'JOIN cards c ON cf.cardId = c._id ' +
         'JOIN boards b ON c.boardId = b._id ' +
         'WHERE ' + WhereClause + ' AND c.archived = FALSE ' +
         'ORDER BY c.modifiedAt DESC';
  
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('fieldId').AsString := FieldId;
  FQuery.ParamByName('value').AsString := '%' + Value + '%';
  if BoardId <> '' then
    FQuery.ParamByName('boardId').AsString := BoardId;
  FQuery.ParamByName('userId').AsString := '%' + UserId + '%';
  
  FQuery.Open;
  
  while not FQuery.Eof do
  begin
    Results.Add(TJSONObject.Create([
      '_id', FQuery.FieldByName('_id').AsString,
      'type', 'card',
      'title', FQuery.FieldByName('title').AsString,
      'description', FQuery.FieldByName('description').AsString,
      'boardId', FQuery.FieldByName('boardId').AsString,
      'boardTitle', FQuery.FieldByName('boardTitle').AsString,
      'customFieldValue', FQuery.FieldByName('value').AsString,
      'createdAt', FQuery.FieldByName('createdAt').AsString
    ]));
    FQuery.Next;
  end;
  
  Result := Results;
end;

// Search suggestions and history
function TSearchManager.GetSearchSuggestions(const Query: string; const UserId: string): TJSONArray;
var
  Suggestions: TJSONArray;
  SQL: string;
begin
  Suggestions := TJSONArray.Create;
  
  if Length(Query) < 2 then
  begin
    Result := Suggestions;
    Exit;
  end;
  
  // Get recent searches
  SQL := 'SELECT DISTINCT query FROM searchHistory WHERE userId = :userId AND query LIKE :query ORDER BY lastSearched DESC LIMIT 5';
  
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('userId').AsString := UserId;
  FQuery.ParamByName('query').AsString := Query + '%';
  FQuery.Open;
  
  while not FQuery.Eof do
  begin
    Suggestions.Add(TJSONObject.Create([
      'type', 'history',
      'text', FQuery.FieldByName('query').AsString
    ]));
    FQuery.Next;
  end;
  
  Result := Suggestions;
end;

function TSearchManager.GetRecentSearches(const UserId: string): TJSONArray;
var
  Recent: TJSONArray;
  SQL: string;
begin
  Recent := TJSONArray.Create;
  
  SQL := 'SELECT query, lastSearched FROM searchHistory WHERE userId = :userId ORDER BY lastSearched DESC LIMIT 10';
  
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('userId').AsString := UserId;
  FQuery.Open;
  
  while not FQuery.Eof do
  begin
    Recent.Add(TJSONObject.Create([
      'query', FQuery.FieldByName('query').AsString,
      'lastSearched', FQuery.FieldByName('lastSearched').AsString
    ]));
    FQuery.Next;
  end;
  
  Result := Recent;
end;

function TSearchManager.SaveSearchHistory(const Query: string; const UserId: string): Boolean;
var
  SQL: string;
begin
  Result := False;
  
  if Query = '' then
    Exit;
  
  SQL := 'INSERT OR REPLACE INTO searchHistory (userId, query, lastSearched) VALUES (:userId, :query, datetime(''now''))';
  
  FQuery.Close;
  FQuery.SQL.Text := SQL;
  FQuery.ParamByName('userId').AsString := UserId;
  FQuery.ParamByName('query').AsString := Query;
  
  try
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      LogError('Save search history error: ' + E.Message);
      Result := False;
    end;
  end;
end;

// Full-text search functions
function TSearchManager.CreateFullTextIndex: Boolean;
var
  SQL: string;
begin
  Result := False;
  
  try
    // Create FTS table for boards
    SQL := 'CREATE VIRTUAL TABLE IF NOT EXISTS boards_fts USING fts5(title, description, content=''boards'', content_rowid=''_id'')';
    FQuery.Close;
    FQuery.SQL.Text := SQL;
    FQuery.ExecSQL;
    
    // Create FTS table for cards
    SQL := 'CREATE VIRTUAL TABLE IF NOT EXISTS cards_fts USING fts5(title, description, content=''cards'', content_rowid=''_id'')';
    FQuery.Close;
    FQuery.SQL.Text := SQL;
    FQuery.ExecSQL;
    
    // Create FTS table for comments
    SQL := 'CREATE VIRTUAL TABLE IF NOT EXISTS comments_fts USING fts5(text, content=''comments'', content_rowid=''_id'')';
    FQuery.Close;
    FQuery.SQL.Text := SQL;
    FQuery.ExecSQL;
    
    FTransaction.Commit;
    Result := True;
    LogInfo('Full-text search index created successfully');
  except
    on E: Exception do
    begin
      LogError('Create full-text index error: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TSearchManager.RebuildFullTextIndex: Boolean;
var
  SQL: string;
begin
  Result := False;
  
  try
    // Rebuild FTS tables
    SQL := 'INSERT INTO boards_fts(boards_fts) VALUES(''rebuild'')';
    FQuery.Close;
    FQuery.SQL.Text := SQL;
    FQuery.ExecSQL;
    
    SQL := 'INSERT INTO cards_fts(cards_fts) VALUES(''rebuild'')';
    FQuery.Close;
    FQuery.SQL.Text := SQL;
    FQuery.ExecSQL;
    
    SQL := 'INSERT INTO comments_fts(comments_fts) VALUES(''rebuild'')';
    FQuery.Close;
    FQuery.SQL.Text := SQL;
    FQuery.ExecSQL;
    
    FTransaction.Commit;
    Result := True;
    LogInfo('Full-text search index rebuilt successfully');
  except
    on E: Exception do
    begin
      LogError('Rebuild full-text index error: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TSearchManager.DropFullTextIndex: Boolean;
var
  SQL: string;
begin
  Result := False;
  
  try
    // Drop FTS tables
    SQL := 'DROP TABLE IF EXISTS boards_fts';
    FQuery.Close;
    FQuery.SQL.Text := SQL;
    FQuery.ExecSQL;
    
    SQL := 'DROP TABLE IF EXISTS cards_fts';
    FQuery.Close;
    FQuery.SQL.Text := SQL;
    FQuery.ExecSQL;
    
    SQL := 'DROP TABLE IF EXISTS comments_fts';
    FQuery.Close;
    FQuery.SQL.Text := SQL;
    FQuery.ExecSQL;
    
    FTransaction.Commit;
    Result := True;
    LogInfo('Full-text search index dropped successfully');
  except
    on E: Exception do
    begin
      LogError('Drop full-text index error: ' + E.Message);
      Result := False;
    end;
  end;
end;

// TSearchIndexer implementation
constructor TSearchIndexer.Create(ADatabase: TSQLite3Connection; ATransaction: TSQLTransaction);
begin
  inherited Create(ADatabase, ATransaction);
end;

destructor TSearchIndexer.Destroy;
begin
  inherited Destroy;
end;

function TSearchIndexer.IndexBoard(const Board: TBoard): Boolean;
var
  SQL: string;
begin
  Result := False;
  
  if not Assigned(Board) then
    Exit;
  
  try
    SQL := 'INSERT OR REPLACE INTO boards_fts(_id, title, description) VALUES(:id, :title, :description)';
    FQuery.Close;
    FQuery.SQL.Text := SQL;
    FQuery.ParamByName('id').AsString := Board.Id;
    FQuery.ParamByName('title').AsString := Board.Title;
    FQuery.ParamByName('description').AsString := Board.Description;
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      LogError('Index board error: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TSearchIndexer.IndexList(const List: TList): Boolean;
begin
  // Lists are typically not indexed separately as they're part of boards
  Result := True;
end;

function TSearchIndexer.IndexCard(const Card: TCard): Boolean;
var
  SQL: string;
begin
  Result := False;
  
  if not Assigned(Card) then
    Exit;
  
  try
    SQL := 'INSERT OR REPLACE INTO cards_fts(_id, title, description) VALUES(:id, :title, :description)';
    FQuery.Close;
    FQuery.SQL.Text := SQL;
    FQuery.ParamByName('id').AsString := Card.Id;
    FQuery.ParamByName('title').AsString := Card.Title;
    FQuery.ParamByName('description').AsString := Card.Description;
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      LogError('Index card error: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TSearchIndexer.IndexUser(const User: TUser): Boolean;
begin
  // Users are typically not indexed in FTS as they have simple search
  Result := True;
end;

function TSearchIndexer.IndexComment(const Comment: TJSONObject): Boolean;
var
  SQL: string;
begin
  Result := False;
  
  if not Assigned(Comment) then
    Exit;
  
  try
    SQL := 'INSERT OR REPLACE INTO comments_fts(_id, text) VALUES(:id, :text)';
    FQuery.Close;
    FQuery.SQL.Text := SQL;
    FQuery.ParamByName('id').AsString := Comment.Get('_id', '');
    FQuery.ParamByName('text').AsString := Comment.Get('text', '');
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      LogError('Index comment error: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TSearchIndexer.IndexAttachment(const Attachment: TJSONObject): Boolean;
begin
  // Attachments are typically not indexed in FTS
  Result := True;
end;

function TSearchIndexer.RemoveFromIndex(const Id: string; const Type: TSearchResultType): Boolean;
var
  SQL: string;
  TableName: string;
begin
  Result := False;
  
  case Type of
    srtBoard: TableName := 'boards_fts';
    srtCard: TableName := 'cards_fts';
    srtComment: TableName := 'comments_fts';
    else
      Exit;
  end;
  
  try
    SQL := 'DELETE FROM ' + TableName + ' WHERE _id = :id';
    FQuery.Close;
    FQuery.SQL.Text := SQL;
    FQuery.ParamByName('id').AsString := Id;
    FQuery.ExecSQL;
    FTransaction.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      LogError('Remove from index error: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TSearchIndexer.UpdateIndex(const Id: string; const Type: TSearchResultType; const Data: TJSONObject): Boolean;
begin
  // Update index by removing and re-adding
  Result := RemoveFromIndex(Id, Type);
  if Result then
  begin
    case Type of
      srtBoard: Result := IndexBoard(nil); // Would need to reconstruct Board object
      srtCard: Result := IndexCard(nil);   // Would need to reconstruct Card object
      srtComment: Result := IndexComment(Data);
    end;
  end;
end;

// Utility functions
function ResultTypeToString(const ResultType: TSearchResultType): string;
begin
  case ResultType of
    srtBoard: Result := 'board';
    srtList: Result := 'list';
    srtCard: Result := 'card';
    srtUser: Result := 'user';
    srtComment: Result := 'comment';
    srtAttachment: Result := 'attachment';
  end;
end;

function StringToResultType(const Str: string): TSearchResultType;
begin
  case Str of
    'board': Result := srtBoard;
    'list': Result := srtList;
    'card': Result := srtCard;
    'user': Result := srtUser;
    'comment': Result := srtComment;
    'attachment': Result := srtAttachment;
    else
      Result := srtCard;
  end;
end;

function CalculateRelevance(const Query: string; const Text: string): Double;
var
  QueryLower, TextLower: string;
  QueryWords: TStringArray;
  i: Integer;
  Word: string;
  Matches: Integer;
  TotalWords: Integer;
begin
  Result := 0.0;
  
  if (Query = '') or (Text = '') then
    Exit;
  
  QueryLower := LowerCase(Query);
  TextLower := LowerCase(Text);
  
  // Exact match gets highest score
  if Pos(QueryLower, TextLower) > 0 then
  begin
    Result := 1.0;
    Exit;
  end;
  
  // Word-based matching
  QueryWords := TokenizeQuery(Query);
  Matches := 0;
  TotalWords := Length(QueryWords);
  
  for i := 0 to High(QueryWords) do
  begin
    Word := LowerCase(QueryWords[i]);
    if Pos(Word, TextLower) > 0 then
      Inc(Matches);
  end;
  
  if TotalWords > 0 then
    Result := Matches / TotalWords;
end;

function HighlightText(const Text: string; const Query: string): string;
var
  QueryLower, TextLower: string;
  QueryWords: TStringArray;
  i: Integer;
  Word: string;
  HighlightedText: string;
begin
  Result := Text;
  
  if (Query = '') or (Text = '') then
    Exit;
  
  QueryLower := LowerCase(Query);
  TextLower := LowerCase(Text);
  HighlightedText := Text;
  
  // Highlight exact match
  if Pos(QueryLower, TextLower) > 0 then
  begin
    HighlightedText := StringReplace(HighlightedText, Query, '<mark>' + Query + '</mark>', [rfIgnoreCase]);
  end
  else
  begin
    // Highlight individual words
    QueryWords := TokenizeQuery(Query);
    for i := 0 to High(QueryWords) do
    begin
      Word := QueryWords[i];
      HighlightedText := StringReplace(HighlightedText, Word, '<mark>' + Word + '</mark>', [rfIgnoreCase]);
    end;
  end;
  
  Result := HighlightedText;
end;

function TokenizeQuery(const Query: string): TStringArray;
var
  List: TStringList;
  i: Integer;
begin
  List := TStringList.Create;
  try
    List.Delimiter := ' ';
    List.DelimitedText := Trim(Query);
    
    SetLength(Result, List.Count);
    for i := 0 to List.Count - 1 do
    begin
      Result[i] := Trim(List[i]);
    end;
  finally
    List.Free;
  end;
end;

function BuildSearchSQL(const Query: string; const Table: string; const Fields: TStringArray; const Conditions: string): string;
var
  i: Integer;
  WhereClause: string;
  Field: string;
begin
  WhereClause := '';
  
  for i := 0 to High(Fields) do
  begin
    Field := Fields[i];
    if WhereClause <> '' then
      WhereClause := WhereClause + ' OR ';
    WhereClause := WhereClause + Field + ' LIKE :query';
  end;
  
  Result := 'SELECT * FROM ' + Table + ' WHERE (' + WhereClause + ')';
  
  if Conditions <> '' then
    Result := Result + ' AND (' + Conditions + ')';
end;

end.
