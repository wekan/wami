unit wekan_api;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, HTTPDefs, wekan_core, wekan_database, wekan_auth, wekan_models, wekan_web;

// API handler functions
function HandleGetBoards(ARequest: TRequest): string;
function HandleCreateBoard(ARequest: TRequest): string;
function HandleGetBoard(ARequest: TRequest): string;
function HandleUpdateBoard(ARequest: TRequest): string;
function HandleDeleteBoard(ARequest: TRequest): string;

function HandleGetLists(ARequest: TRequest): string;
function HandleCreateList(ARequest: TRequest): string;
function HandleGetList(ARequest: TRequest): string;
function HandleUpdateList(ARequest: TRequest): string;
function HandleDeleteList(ARequest: TRequest): string;

function HandleGetCards(ARequest: TRequest): string;
function HandleCreateCard(ARequest: TRequest): string;
function HandleGetCard(ARequest: TRequest): string;
function HandleUpdateCard(ARequest: TRequest): string;
function HandleDeleteCard(ARequest: TRequest): string;

function HandleGetUsers(ARequest: TRequest): string;
function HandleCreateUser(ARequest: TRequest): string;
function HandleGetUser(ARequest: TRequest): string;
function HandleUpdateUser(ARequest: TRequest): string;
function HandleDeleteUser(ARequest: TRequest): string;

function HandleUserLogin(ARequest: TRequest): string;
function HandleUserLogout(ARequest: TRequest): string;
function HandleUserRegister(ARequest: TRequest): string;
function HandleUserVerify(ARequest: TRequest): string;

function HandleUploadFile(ARequest: TRequest): string;
function GetFilePathById(const FileId: string): string;

function HandleImportTrello(ARequest: TRequest): string;
function HandleExportCSV(ARequest: TRequest): string;
function HandleCreateBackup(ARequest: TRequest): string;
function HandleRestoreBackup(ARequest: TRequest): string;

implementation

uses
  strutils, dateutils, fpjson, jsonparser, wekan_utils;

// Board API handlers
function HandleGetBoards(ARequest: TRequest): string;
var
  User: TUser;
  Boards: TJSONArray;
  Response: TJSONObject;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  Boards := BoardManager.GetBoardsByUser(User.Id);
  try
    Response := TJSONObject.Create;
    Response.Add('success', True);
    Response.Add('data', Boards);
    Result := Response.AsJSON;
  finally
    Response.Free;
  end;
end;

function HandleCreateBoard(ARequest: TRequest): string;
var
  User: TUser;
  BoardData: TJSONObject;
  Board: TBoard;
  Response: TJSONObject;
  Title, Description: string;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  BoardData := WebHandler.ParseJSONBody(ARequest);
  if not Assigned(BoardData) then
  begin
    Result := CreateJSONError('Invalid JSON data', 400);
    Exit;
  end;
  
  try
    Title := BoardData.Get('title', '');
    Description := BoardData.Get('description', '');
    
    if Title = '' then
    begin
      Result := CreateJSONError('Title is required', 400);
      Exit;
    end;
    
    Board := BoardManager.CreateBoard(Title, Description, User.Id);
    if Assigned(Board) then
    begin
      Response := TJSONObject.Create;
      Response.Add('success', True);
      Response.Add('data', Board.ToJSON);
      Result := Response.AsJSON;
      Board.Free;
    end
    else
    begin
      Result := CreateJSONError('Failed to create board', 500);
    end;
  finally
    BoardData.Free;
  end;
end;

function HandleGetBoard(ARequest: TRequest): string;
var
  User: TUser;
  BoardId: string;
  Board: TBoard;
  Response: TJSONObject;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  BoardId := WebHandler.GetRouteParam(ARequest, 'id');
  if BoardId = '' then
  begin
    Result := CreateJSONError('Board ID is required', 400);
    Exit;
  end;
  
  Board := BoardManager.GetBoardById(BoardId);
  if Assigned(Board) then
  begin
    try
      Response := TJSONObject.Create;
      Response.Add('success', True);
      Response.Add('data', Board.ToJSON);
      Result := Response.AsJSON;
    finally
      Board.Free;
    end;
  end
  else
  begin
    Result := CreateJSONError('Board not found', 404);
  end;
end;

function HandleUpdateBoard(ARequest: TRequest): string;
var
  User: TUser;
  BoardId: string;
  BoardData: TJSONObject;
  Board: TBoard;
  Response: TJSONObject;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  BoardId := WebHandler.GetRouteParam(ARequest, 'id');
  if BoardId = '' then
  begin
    Result := CreateJSONError('Board ID is required', 400);
    Exit;
  end;
  
  BoardData := WebHandler.ParseJSONBody(ARequest);
  if not Assigned(BoardData) then
  begin
    Result := CreateJSONError('Invalid JSON data', 400);
    Exit;
  end;
  
  Board := BoardManager.GetBoardById(BoardId);
  if not Assigned(Board) then
  begin
    Result := CreateJSONError('Board not found', 404);
    Exit;
  end;
  
  try
    // Update board fields
    if BoardData.Find('title') <> nil then
      Board.Title := BoardData.Get('title', Board.Title);
    if BoardData.Find('description') <> nil then
      Board.Description := BoardData.Get('description', Board.Description);
    if BoardData.Find('color') <> nil then
      Board.Color := BoardData.Get('color', Board.Color);
    if BoardData.Find('permission') <> nil then
      Board.Permission := BoardData.Get('permission', Board.Permission);
    
    if BoardManager.UpdateBoard(Board) then
    begin
      Response := TJSONObject.Create;
      Response.Add('success', True);
      Response.Add('data', Board.ToJSON);
      Result := Response.AsJSON;
    end
    else
    begin
      Result := CreateJSONError('Failed to update board', 500);
    end;
  finally
    Board.Free;
    BoardData.Free;
  end;
end;

function HandleDeleteBoard(ARequest: TRequest): string;
var
  User: TUser;
  BoardId: string;
  Response: TJSONObject;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  BoardId := WebHandler.GetRouteParam(ARequest, 'id');
  if BoardId = '' then
  begin
    Result := CreateJSONError('Board ID is required', 400);
    Exit;
  end;
  
  if BoardManager.DeleteBoard(BoardId) then
  begin
    Response := TJSONObject.Create;
    Response.Add('success', True);
    Response.Add('message', 'Board deleted successfully');
    Result := Response.AsJSON;
  end
  else
  begin
    Result := CreateJSONError('Failed to delete board', 500);
  end;
end;

// List API handlers
function HandleGetLists(ARequest: TRequest): string;
var
  User: TUser;
  BoardId: string;
  Lists: TJSONArray;
  Response: TJSONObject;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  BoardId := WebHandler.GetRouteParam(ARequest, 'boardId');
  if BoardId = '' then
  begin
    Result := CreateJSONError('Board ID is required', 400);
    Exit;
  end;
  
  Lists := ListManager.GetListsByBoard(BoardId);
  try
    Response := TJSONObject.Create;
    Response.Add('success', True);
    Response.Add('data', Lists);
    Result := Response.AsJSON;
  finally
    Response.Free;
  end;
end;

function HandleCreateList(ARequest: TRequest): string;
var
  User: TUser;
  BoardId: string;
  ListData: TJSONObject;
  List: TList;
  Response: TJSONObject;
  Title: string;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  BoardId := WebHandler.GetRouteParam(ARequest, 'boardId');
  if BoardId = '' then
  begin
    Result := CreateJSONError('Board ID is required', 400);
    Exit;
  end;
  
  ListData := WebHandler.ParseJSONBody(ARequest);
  if not Assigned(ListData) then
  begin
    Result := CreateJSONError('Invalid JSON data', 400);
    Exit;
  end;
  
  try
    Title := ListData.Get('title', '');
    
    if Title = '' then
    begin
      Result := CreateJSONError('Title is required', 400);
      Exit;
    end;
    
    List := ListManager.CreateList(Title, BoardId);
    if Assigned(List) then
    begin
      Response := TJSONObject.Create;
      Response.Add('success', True);
      Response.Add('data', List.ToJSON);
      Result := Response.AsJSON;
      List.Free;
    end
    else
    begin
      Result := CreateJSONError('Failed to create list', 500);
    end;
  finally
    ListData.Free;
  end;
end;

function HandleGetList(ARequest: TRequest): string;
var
  User: TUser;
  ListId: string;
  List: TList;
  Response: TJSONObject;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  ListId := WebHandler.GetRouteParam(ARequest, 'id');
  if ListId = '' then
  begin
    Result := CreateJSONError('List ID is required', 400);
    Exit;
  end;
  
  List := ListManager.GetListById(ListId);
  if Assigned(List) then
  begin
    try
      Response := TJSONObject.Create;
      Response.Add('success', True);
      Response.Add('data', List.ToJSON);
      Result := Response.AsJSON;
    finally
      List.Free;
    end;
  end
  else
  begin
    Result := CreateJSONError('List not found', 404);
  end;
end;

function HandleUpdateList(ARequest: TRequest): string;
var
  User: TUser;
  ListId: string;
  ListData: TJSONObject;
  List: TList;
  Response: TJSONObject;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  ListId := WebHandler.GetRouteParam(ARequest, 'id');
  if ListId = '' then
  begin
    Result := CreateJSONError('List ID is required', 400);
    Exit;
  end;
  
  ListData := WebHandler.ParseJSONBody(ARequest);
  if not Assigned(ListData) then
  begin
    Result := CreateJSONError('Invalid JSON data', 400);
    Exit;
  end;
  
  List := ListManager.GetListById(ListId);
  if not Assigned(List) then
  begin
    Result := CreateJSONError('List not found', 404);
    Exit;
  end;
  
  try
    // Update list fields
    if ListData.Find('title') <> nil then
      List.Title := ListData.Get('title', List.Title);
    if ListData.Find('color') <> nil then
      List.Color := ListData.Get('color', List.Color);
    if ListData.Find('wipLimit') <> nil then
      List.WipLimit := ListData.Get('wipLimit', List.WipLimit);
    
    if ListManager.UpdateList(List) then
    begin
      Response := TJSONObject.Create;
      Response.Add('success', True);
      Response.Add('data', List.ToJSON);
      Result := Response.AsJSON;
    end
    else
    begin
      Result := CreateJSONError('Failed to update list', 500);
    end;
  finally
    List.Free;
    ListData.Free;
  end;
end;

function HandleDeleteList(ARequest: TRequest): string;
var
  User: TUser;
  ListId: string;
  Response: TJSONObject;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  ListId := WebHandler.GetRouteParam(ARequest, 'id');
  if ListId = '' then
  begin
    Result := CreateJSONError('List ID is required', 400);
    Exit;
  end;
  
  if ListManager.DeleteList(ListId) then
  begin
    Response := TJSONObject.Create;
    Response.Add('success', True);
    Response.Add('message', 'List deleted successfully');
    Result := Response.AsJSON;
  end
  else
  begin
    Result := CreateJSONError('Failed to delete list', 500);
  end;
end;

// Card API handlers
function HandleGetCards(ARequest: TRequest): string;
var
  User: TUser;
  ListId: string;
  Cards: TJSONArray;
  Response: TJSONObject;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  ListId := WebHandler.GetRouteParam(ARequest, 'listId');
  if ListId = '' then
  begin
    Result := CreateJSONError('List ID is required', 400);
    Exit;
  end;
  
  Cards := CardManager.GetCardsByList(ListId);
  try
    Response := TJSONObject.Create;
    Response.Add('success', True);
    Response.Add('data', Cards);
    Result := Response.AsJSON;
  finally
    Response.Free;
  end;
end;

function HandleCreateCard(ARequest: TRequest): string;
var
  User: TUser;
  ListId, BoardId: string;
  CardData: TJSONObject;
  Card: TCard;
  Response: TJSONObject;
  Title: string;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  ListId := WebHandler.GetRouteParam(ARequest, 'listId');
  BoardId := WebHandler.GetRouteParam(ARequest, 'boardId');
  
  if (ListId = '') or (BoardId = '') then
  begin
    Result := CreateJSONError('List ID and Board ID are required', 400);
    Exit;
  end;
  
  CardData := WebHandler.ParseJSONBody(ARequest);
  if not Assigned(CardData) then
  begin
    Result := CreateJSONError('Invalid JSON data', 400);
    Exit;
  end;
  
  try
    Title := CardData.Get('title', '');
    
    if Title = '' then
    begin
      Result := CreateJSONError('Title is required', 400);
      Exit;
    end;
    
    Card := CardManager.CreateCard(Title, ListId, BoardId);
    if Assigned(Card) then
    begin
      Response := TJSONObject.Create;
      Response.Add('success', True);
      Response.Add('data', Card.ToJSON);
      Result := Response.AsJSON;
      Card.Free;
    end
    else
    begin
      Result := CreateJSONError('Failed to create card', 500);
    end;
  finally
    CardData.Free;
  end;
end;

function HandleGetCard(ARequest: TRequest): string;
var
  User: TUser;
  CardId: string;
  Card: TCard;
  Response: TJSONObject;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  CardId := WebHandler.GetRouteParam(ARequest, 'id');
  if CardId = '' then
  begin
    Result := CreateJSONError('Card ID is required', 400);
    Exit;
  end;
  
  Card := CardManager.GetCardById(CardId);
  if Assigned(Card) then
  begin
    try
      Response := TJSONObject.Create;
      Response.Add('success', True);
      Response.Add('data', Card.ToJSON);
      Result := Response.AsJSON;
    finally
      Card.Free;
    end;
  end
  else
  begin
    Result := CreateJSONError('Card not found', 404);
  end;
end;

function HandleUpdateCard(ARequest: TRequest): string;
var
  User: TUser;
  CardId: string;
  CardData: TJSONObject;
  Card: TCard;
  Response: TJSONObject;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  CardId := WebHandler.GetRouteParam(ARequest, 'id');
  if CardId = '' then
  begin
    Result := CreateJSONError('Card ID is required', 400);
    Exit;
  end;
  
  CardData := WebHandler.ParseJSONBody(ARequest);
  if not Assigned(CardData) then
  begin
    Result := CreateJSONError('Invalid JSON data', 400);
    Exit;
  end;
  
  Card := CardManager.GetCardById(CardId);
  if not Assigned(Card) then
  begin
    Result := CreateJSONError('Card not found', 404);
    Exit;
  end;
  
  try
    // Update card fields
    if CardData.Find('title') <> nil then
      Card.Title := CardData.Get('title', Card.Title);
    if CardData.Find('description') <> nil then
      Card.Description := CardData.Get('description', Card.Description);
    if CardData.Find('dueAt') <> nil then
      Card.DueAt := CardData.Get('dueAt', Card.DueAt);
    if CardData.Find('color') <> nil then
      Card.Color := CardData.Get('color', Card.Color);
    
    if CardManager.UpdateCard(Card) then
    begin
      Response := TJSONObject.Create;
      Response.Add('success', True);
      Response.Add('data', Card.ToJSON);
      Result := Response.AsJSON;
    end
    else
    begin
      Result := CreateJSONError('Failed to update card', 500);
    end;
  finally
    Card.Free;
    CardData.Free;
  end;
end;

function HandleDeleteCard(ARequest: TRequest): string;
var
  User: TUser;
  CardId: string;
  Response: TJSONObject;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  CardId := WebHandler.GetRouteParam(ARequest, 'id');
  if CardId = '' then
  begin
    Result := CreateJSONError('Card ID is required', 400);
    Exit;
  end;
  
  if CardManager.DeleteCard(CardId) then
  begin
    Response := TJSONObject.Create;
    Response.Add('success', True);
    Response.Add('message', 'Card deleted successfully');
    Result := Response.AsJSON;
  end
  else
  begin
    Result := CreateJSONError('Failed to delete card', 500);
  end;
end;

// User API handlers
function HandleGetUsers(ARequest: TRequest): string;
var
  User: TUser;
  Users: TJSONArray;
  Response: TJSONObject;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  if not User.IsAdmin then
  begin
    Result := CreateJSONError('Admin access required', 403);
    Exit;
  end;
  
  Users := UserManager.GetUsers;
  try
    Response := TJSONObject.Create;
    Response.Add('success', True);
    Response.Add('data', Users);
    Result := Response.AsJSON;
  finally
    Response.Free;
  end;
end;

function HandleCreateUser(ARequest: TRequest): string;
var
  User: TUser;
  UserData: TJSONObject;
  NewUser: TUser;
  Response: TJSONObject;
  Username, Email, Password: string;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  if not User.IsAdmin then
  begin
    Result := CreateJSONError('Admin access required', 403);
    Exit;
  end;
  
  UserData := WebHandler.ParseJSONBody(ARequest);
  if not Assigned(UserData) then
  begin
    Result := CreateJSONError('Invalid JSON data', 400);
    Exit;
  end;
  
  try
    Username := UserData.Get('username', '');
    Email := UserData.Get('email', '');
    Password := UserData.Get('password', '');
    
    if (Username = '') or (Email = '') or (Password = '') then
    begin
      Result := CreateJSONError('Username, email, and password are required', 400);
      Exit;
    end;
    
    NewUser := AuthManager.CreateUser(Username, Email, Password);
    if Assigned(NewUser) then
    begin
      Response := TJSONObject.Create;
      Response.Add('success', True);
      Response.Add('data', NewUser.ToJSON);
      Result := Response.AsJSON;
      NewUser.Free;
    end
    else
    begin
      Result := CreateJSONError('Failed to create user', 500);
    end;
  finally
    UserData.Free;
  end;
end;

function HandleGetUser(ARequest: TRequest): string;
var
  User: TUser;
  UserId: string;
  UserData: TJSONObject;
  Response: TJSONObject;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  UserId := WebHandler.GetRouteParam(ARequest, 'id');
  if UserId = '' then
  begin
    Result := CreateJSONError('User ID is required', 400);
    Exit;
  end;
  
  // Users can only view their own data unless they're admin
  if (UserId <> User.Id) and not User.IsAdmin then
  begin
    Result := CreateJSONError('Access denied', 403);
    Exit;
  end;
  
  UserData := UserManager.GetUserById(UserId);
  if Assigned(UserData) then
  begin
    try
      Response := TJSONObject.Create;
      Response.Add('success', True);
      Response.Add('data', UserData);
      Result := Response.AsJSON;
    finally
      Response.Free;
    end;
  end
  else
  begin
    Result := CreateJSONError('User not found', 404);
  end;
end;

function HandleUpdateUser(ARequest: TRequest): string;
var
  User: TUser;
  UserId: string;
  UserData: TJSONObject;
  Response: TJSONObject;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  UserId := WebHandler.GetRouteParam(ARequest, 'id');
  if UserId = '' then
  begin
    Result := CreateJSONError('User ID is required', 400);
    Exit;
  end;
  
  // Users can only update their own data unless they're admin
  if (UserId <> User.Id) and not User.IsAdmin then
  begin
    Result := CreateJSONError('Access denied', 403);
    Exit;
  end;
  
  UserData := WebHandler.ParseJSONBody(ARequest);
  if not Assigned(UserData) then
  begin
    Result := CreateJSONError('Invalid JSON data', 400);
    Exit;
  end;
  
  try
    if UserManager.UpdateUser(UserId, UserData) then
    begin
      Response := TJSONObject.Create;
      Response.Add('success', True);
      Response.Add('message', 'User updated successfully');
      Result := Response.AsJSON;
    end
    else
    begin
      Result := CreateJSONError('Failed to update user', 500);
    end;
  finally
    UserData.Free;
  end;
end;

function HandleDeleteUser(ARequest: TRequest): string;
var
  User: TUser;
  UserId: string;
  Response: TJSONObject;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  if not User.IsAdmin then
  begin
    Result := CreateJSONError('Admin access required', 403);
    Exit;
  end;
  
  UserId := WebHandler.GetRouteParam(ARequest, 'id');
  if UserId = '' then
  begin
    Result := CreateJSONError('User ID is required', 400);
    Exit;
  end;
  
  if UserManager.DeleteUser(UserId) then
  begin
    Response := TJSONObject.Create;
    Response.Add('success', True);
    Response.Add('message', 'User deleted successfully');
    Result := Response.AsJSON;
  end
  else
  begin
    Result := CreateJSONError('Failed to delete user', 500);
  end;
end;

// Authentication API handlers
function HandleUserLogin(ARequest: TRequest): string;
var
  LoginData: TJSONObject;
  Session: TSession;
  Response: TJSONObject;
  Username, Password: string;
  IpAddress, UserAgent: string;
begin
  LoginData := WebHandler.ParseJSONBody(ARequest);
  if not Assigned(LoginData) then
  begin
    Result := CreateJSONError('Invalid JSON data', 400);
    Exit;
  end;
  
  try
    Username := LoginData.Get('username', '');
    Password := LoginData.Get('password', '');
    
    if (Username = '') or (Password = '') then
    begin
      Result := CreateJSONError('Username and password are required', 400);
      Exit;
    end;
    
    IpAddress := ARequest.RemoteAddress;
    UserAgent := ARequest.GetHeader('User-Agent');
    
    Session := AuthManager.Login(Username, Password, IpAddress, UserAgent);
    if Assigned(Session) then
    begin
      try
        Response := TJSONObject.Create;
        Response.Add('success', True);
        Response.Add('sessionId', Session.SessionId);
        Response.Add('message', 'Login successful');
        Result := Response.AsJSON;
      finally
        Session.Free;
      end;
    end
    else
    begin
      Result := CreateJSONError('Invalid username or password', 401);
    end;
  finally
    LoginData.Free;
  end;
end;

function HandleUserLogout(ARequest: TRequest): string;
var
  SessionId: string;
  Response: TJSONObject;
begin
  SessionId := ARequest.CookieFields.Values['sessionId'];
  if SessionId = '' then
    SessionId := ARequest.GetHeader('X-Session-ID');
    
  if SessionId = '' then
  begin
    Result := CreateJSONError('Session ID is required', 400);
    Exit;
  end;
  
  if AuthManager.Logout(SessionId) then
  begin
    Response := TJSONObject.Create;
    Response.Add('success', True);
    Response.Add('message', 'Logout successful');
    Result := Response.AsJSON;
  end
  else
  begin
    Result := CreateJSONError('Failed to logout', 500);
  end;
end;

function HandleUserRegister(ARequest: TRequest): string;
var
  RegisterData: TJSONObject;
  NewUser: TUser;
  Response: TJSONObject;
  Username, Email, Password: string;
begin
  RegisterData := WebHandler.ParseJSONBody(ARequest);
  if not Assigned(RegisterData) then
  begin
    Result := CreateJSONError('Invalid JSON data', 400);
    Exit;
  end;
  
  try
    Username := RegisterData.Get('username', '');
    Email := RegisterData.Get('email', '');
    Password := RegisterData.Get('password', '');
    
    if (Username = '') or (Email = '') or (Password = '') then
    begin
      Result := CreateJSONError('Username, email, and password are required', 400);
      Exit;
    end;
    
    NewUser := AuthManager.CreateUser(Username, Email, Password);
    if Assigned(NewUser) then
    begin
      try
        Response := TJSONObject.Create;
        Response.Add('success', True);
        Response.Add('message', 'User registered successfully');
        Result := Response.AsJSON;
      finally
        NewUser.Free;
      end;
    end
    else
    begin
      Result := CreateJSONError('Failed to register user', 500);
    end;
  finally
    RegisterData.Free;
  end;
end;

function HandleUserVerify(ARequest: TRequest): string;
var
  SessionId: string;
  User: TUser;
  Response: TJSONObject;
begin
  SessionId := ARequest.CookieFields.Values['sessionId'];
  if SessionId = '' then
    SessionId := ARequest.GetHeader('X-Session-ID');
    
  if SessionId = '' then
  begin
    Result := CreateJSONError('Session ID is required', 400);
    Exit;
  end;
  
  User := AuthManager.VerifySession(SessionId);
  if Assigned(User) then
  begin
    try
      Response := TJSONObject.Create;
      Response.Add('success', True);
      Response.Add('user', User.ToJSON);
      Result := Response.AsJSON;
    finally
      User.Free;
    end;
  end
  else
  begin
    Result := CreateJSONError('Invalid session', 401);
  end;
end;

// File API handlers
function HandleUploadFile(ARequest: TRequest): string;
var
  User: TUser;
  FileId: string;
  Response: TJSONObject;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  // File upload implementation would go here
  // For now, return a placeholder response
  FileId := GenerateId;
  
  Response := TJSONObject.Create;
  Response.Add('success', True);
  Response.Add('fileId', FileId);
  Response.Add('message', 'File upload not implemented yet');
  Result := Response.AsJSON;
end;

function GetFilePathById(const FileId: string): string;
begin
  Result := FileManager.GetFilePath(FileId);
end;

// Import/Export API handlers
function HandleImportTrello(ARequest: TRequest): string;
var
  User: TUser;
  Response: TJSONObject;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  // Trello import implementation would go here
  Response := TJSONObject.Create;
  Response.Add('success', True);
  Response.Add('message', 'Trello import not implemented yet');
  Result := Response.AsJSON;
end;

function HandleExportCSV(ARequest: TRequest): string;
var
  User: TUser;
  Response: TJSONObject;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  // CSV export implementation would go here
  Response := TJSONObject.Create;
  Response.Add('success', True);
  Response.Add('message', 'CSV export not implemented yet');
  Result := Response.AsJSON;
end;

function HandleCreateBackup(ARequest: TRequest): string;
var
  User: TUser;
  Response: TJSONObject;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  if not User.IsAdmin then
  begin
    Result := CreateJSONError('Admin access required', 403);
    Exit;
  end;
  
  // Backup creation implementation would go here
  Response := TJSONObject.Create;
  Response.Add('success', True);
  Response.Add('message', 'Backup creation not implemented yet');
  Result := Response.AsJSON;
end;

function HandleRestoreBackup(ARequest: TRequest): string;
var
  User: TUser;
  Response: TJSONObject;
begin
  User := WebHandler.GetCurrentUser(ARequest);
  if not Assigned(User) then
  begin
    Result := CreateJSONError('Authentication required', 401);
    Exit;
  end;
  
  if not User.IsAdmin then
  begin
    Result := CreateJSONError('Admin access required', 403);
    Exit;
  end;
  
  // Backup restore implementation would go here
  Response := TJSONObject.Create;
  Response.Add('success', True);
  Response.Add('message', 'Backup restore not implemented yet');
  Result := Response.AsJSON;
end;

end.
