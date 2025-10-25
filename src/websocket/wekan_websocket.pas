unit wekan_websocket;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, fpwebsocket, wekan_core, wekan_auth, wekan_models;

type
  // WebSocket message types
  TWebSocketMessageType = (
    wmtConnect, wmtDisconnect, wmtPing, wmtPong,
    wmtBoardUpdate, wmtListUpdate, wmtCardUpdate, wmtCardMove,
    wmtUserJoin, wmtUserLeave, wmtTyping, wmtComment,
    wmtActivity, wmtNotification, wmtError
  );

  // WebSocket message structure
  TWebSocketMessage = record
    MessageType: TWebSocketMessageType;
    Data: TJSONObject;
    BoardId: string;
    UserId: string;
    Timestamp: TDateTime;
  end;

  // WebSocket client connection
  TWebSocketClient = class
  private
    FId: string;
    FUserId: string;
    FBoardId: string;
    FConnection: TWebSocketConnection;
    FLastPing: TDateTime;
    FIsActive: Boolean;
  public
    constructor Create(AConnection: TWebSocketConnection);
    destructor Destroy; override;
    
    procedure SendMessage(const Message: TWebSocketMessage);
    procedure SendJSON(const JSON: string);
    procedure Ping;
    procedure Pong;
    
    property Id: string read FId write FId;
    property UserId: string read FUserId write FUserId;
    property BoardId: string read FBoardId write FBoardId;
    property Connection: TWebSocketConnection read FConnection;
    property LastPing: TDateTime read FLastPing write FLastPing;
    property IsActive: Boolean read FIsActive write FIsActive;
  end;

  // WebSocket server manager
  TWebSocketServer = class
  private
    FClients: TList;
    FServer: TWebSocketServer;
    FPort: Integer;
    FIsRunning: Boolean;
    FHeartbeatInterval: Integer;
    FMaxConnections: Integer;
  public
    constructor Create(APort: Integer = 5501);
    destructor Destroy; override;
    
    procedure Start;
    procedure Stop;
    procedure Broadcast(const Message: TWebSocketMessage; const BoardId: string = '');
    procedure SendToUser(const UserId: string; const Message: TWebSocketMessage);
    procedure SendToBoard(const BoardId: string; const Message: TWebSocketMessage);
    procedure RemoveClient(const ClientId: string);
    procedure CleanupInactiveClients;
    
    // Event handlers
    procedure OnConnect(AConnection: TWebSocketConnection);
    procedure OnDisconnect(AConnection: TWebSocketConnection);
    procedure OnMessage(AConnection: TWebSocketConnection; const Message: string);
    procedure OnError(AConnection: TWebSocketConnection; const Error: string);
    
    property IsRunning: Boolean read FIsRunning;
    property Port: Integer read FPort;
  end;

  // Real-time event manager
  TRealtimeManager = class
  private
    FWebSocketServer: TWebSocketServer;
    FBoardManager: TBoardManager;
    FListManager: TListManager;
    FCardManager: TCardManager;
    FUserManager: TUserManager;
  public
    constructor Create(AWebSocketServer: TWebSocketServer; ABoardManager: TBoardManager;
      AListManager: TListManager; ACardManager: TCardManager; AUserManager: TUserManager);
    destructor Destroy; override;
    
    // Board events
    procedure OnBoardCreated(const Board: TBoard; const UserId: string);
    procedure OnBoardUpdated(const Board: TBoard; const UserId: string);
    procedure OnBoardDeleted(const BoardId: string; const UserId: string);
    procedure OnBoardMemberAdded(const BoardId: string; const UserId: string; const MemberId: string);
    procedure OnBoardMemberRemoved(const BoardId: string; const UserId: string; const MemberId: string);
    
    // List events
    procedure OnListCreated(const List: TList; const UserId: string);
    procedure OnListUpdated(const List: TList; const UserId: string);
    procedure OnListDeleted(const ListId: string; const UserId: string);
    procedure OnListMoved(const ListId: string; const NewSort: Integer; const UserId: string);
    
    // Card events
    procedure OnCardCreated(const Card: TCard; const UserId: string);
    procedure OnCardUpdated(const Card: TCard; const UserId: string);
    procedure OnCardDeleted(const CardId: string; const UserId: string);
    procedure OnCardMoved(const CardId: string; const NewListId: string; const NewSort: Integer; const UserId: string);
    procedure OnCardMemberAdded(const CardId: string; const UserId: string; const MemberId: string);
    procedure OnCardMemberRemoved(const CardId: string; const UserId: string; const MemberId: string);
    
    // User events
    procedure OnUserJoined(const UserId: string; const BoardId: string);
    procedure OnUserLeft(const UserId: string; const BoardId: string);
    procedure OnUserTyping(const UserId: string; const BoardId: string; const CardId: string);
    
    // Activity events
    procedure OnActivityCreated(const Activity: TJSONObject; const UserId: string);
    procedure OnCommentAdded(const Comment: TJSONObject; const UserId: string);
    procedure OnAttachmentAdded(const Attachment: TJSONObject; const UserId: string);
  end;

// Global instances
var
  WebSocketServer: TWebSocketServer;
  RealtimeManager: TRealtimeManager;

// Utility functions
function MessageTypeToString(const MessageType: TWebSocketMessageType): string;
function StringToMessageType(const Str: string): TWebSocketMessageType;
function CreateWebSocketMessage(const MessageType: TWebSocketMessageType; const Data: TJSONObject; const BoardId: string = ''; const UserId: string = ''): TWebSocketMessage;
function WebSocketMessageToJSON(const Message: TWebSocketMessage): string;
function JSONToWebSocketMessage(const JSON: string): TWebSocketMessage;

implementation

uses
  strutils, dateutils, wekan_utils;

// TWebSocketClient implementation
constructor TWebSocketClient.Create(AConnection: TWebSocketConnection);
begin
  inherited Create;
  FConnection := AConnection;
  FId := GenerateId;
  FLastPing := Now;
  FIsActive := True;
end;

destructor TWebSocketClient.Destroy;
begin
  inherited Destroy;
end;

procedure TWebSocketClient.SendMessage(const Message: TWebSocketMessage);
var
  JSON: string;
begin
  if Assigned(FConnection) and FIsActive then
  begin
    JSON := WebSocketMessageToJSON(Message);
    SendJSON(JSON);
  end;
end;

procedure TWebSocketClient.SendJSON(const JSON: string);
begin
  if Assigned(FConnection) and FIsActive then
  begin
    try
      FConnection.SendText(JSON);
    except
      on E: Exception do
      begin
        FIsActive := False;
        LogError('WebSocket send error: ' + E.Message);
      end;
    end;
  end;
end;

procedure TWebSocketClient.Ping;
var
  Message: TWebSocketMessage;
begin
  Message := CreateWebSocketMessage(wmtPing, TJSONObject.Create, FBoardId, FUserId);
  SendMessage(Message);
  FLastPing := Now;
end;

procedure TWebSocketClient.Pong;
var
  Message: TWebSocketMessage;
begin
  Message := CreateWebSocketMessage(wmtPong, TJSONObject.Create, FBoardId, FUserId);
  SendMessage(Message);
end;

// TWebSocketServer implementation
constructor TWebSocketServer.Create(APort: Integer);
begin
  inherited Create;
  FPort := APort;
  FClients := TList.Create;
  FServer := TWebSocketServer.Create(nil);
  FServer.Port := FPort;
  FHeartbeatInterval := 30; // 30 seconds
  FMaxConnections := 1000;
  FIsRunning := False;
  
  // Set event handlers
  FServer.OnConnect := @OnConnect;
  FServer.OnDisconnect := @OnDisconnect;
  FServer.OnMessage := @OnMessage;
  FServer.OnError := @OnError;
end;

destructor TWebSocketServer.Destroy;
var
  i: Integer;
begin
  Stop;
  
  // Clean up clients
  for i := FClients.Count - 1 downto 0 do
  begin
    TWebSocketClient(FClients[i]).Free;
  end;
  FClients.Free;
  
  if Assigned(FServer) then
    FServer.Free;
    
  inherited Destroy;
end;

procedure TWebSocketServer.Start;
begin
  if not FIsRunning then
  begin
    try
      FServer.Start;
      FIsRunning := True;
      LogInfo('WebSocket server started on port ' + IntToStr(FPort));
    except
      on E: Exception do
      begin
        LogError('Failed to start WebSocket server: ' + E.Message);
        FIsRunning := False;
      end;
    end;
  end;
end;

procedure TWebSocketServer.Stop;
begin
  if FIsRunning then
  begin
    FServer.Stop;
    FIsRunning := False;
    LogInfo('WebSocket server stopped');
  end;
end;

procedure TWebSocketServer.Broadcast(const Message: TWebSocketMessage; const BoardId: string);
var
  i: Integer;
  Client: TWebSocketClient;
  JSON: string;
begin
  JSON := WebSocketMessageToJSON(Message);
  
  for i := 0 to FClients.Count - 1 do
  begin
    Client := TWebSocketClient(FClients[i]);
    if Client.IsActive and ((BoardId = '') or (Client.BoardId = BoardId)) then
    begin
      try
        Client.SendJSON(JSON);
      except
        on E: Exception do
        begin
          LogError('Broadcast error: ' + E.Message);
          Client.IsActive := False;
        end;
      end;
    end;
  end;
end;

procedure TWebSocketServer.SendToUser(const UserId: string; const Message: TWebSocketMessage);
var
  i: Integer;
  Client: TWebSocketClient;
begin
  for i := 0 to FClients.Count - 1 do
  begin
    Client := TWebSocketClient(FClients[i]);
    if Client.IsActive and (Client.UserId = UserId) then
    begin
      try
        Client.SendMessage(Message);
      except
        on E: Exception do
        begin
          LogError('Send to user error: ' + E.Message);
          Client.IsActive := False;
        end;
      end;
    end;
  end;
end;

procedure TWebSocketServer.SendToBoard(const BoardId: string; const Message: TWebSocketMessage);
var
  i: Integer;
  Client: TWebSocketClient;
begin
  for i := 0 to FClients.Count - 1 do
  begin
    Client := TWebSocketClient(FClients[i]);
    if Client.IsActive and (Client.BoardId = BoardId) then
    begin
      try
        Client.SendMessage(Message);
      except
        on E: Exception do
        begin
          LogError('Send to board error: ' + E.Message);
          Client.IsActive := False;
        end;
      end;
    end;
  end;
end;

procedure TWebSocketServer.RemoveClient(const ClientId: string);
var
  i: Integer;
  Client: TWebSocketClient;
begin
  for i := FClients.Count - 1 downto 0 do
  begin
    Client := TWebSocketClient(FClients[i]);
    if Client.Id = ClientId then
    begin
      Client.Free;
      FClients.Delete(i);
      Break;
    end;
  end;
end;

procedure TWebSocketServer.CleanupInactiveClients;
var
  i: Integer;
  Client: TWebSocketClient;
  InactiveTime: TDateTime;
begin
  InactiveTime := Now - (FHeartbeatInterval * 2) / (24 * 60 * 60); // 2x heartbeat interval
  
  for i := FClients.Count - 1 downto 0 do
  begin
    Client := TWebSocketClient(FClients[i]);
    if not Client.IsActive or (Client.LastPing < InactiveTime) then
    begin
      LogInfo('Removing inactive client: ' + Client.Id);
      Client.Free;
      FClients.Delete(i);
    end;
  end;
end;

procedure TWebSocketServer.OnConnect(AConnection: TWebSocketConnection);
var
  Client: TWebSocketClient;
begin
  if FClients.Count < FMaxConnections then
  begin
    Client := TWebSocketClient.Create(AConnection);
    FClients.Add(Client);
    LogInfo('WebSocket client connected: ' + Client.Id);
  end
  else
  begin
    LogWarning('Maximum connections reached, rejecting new connection');
    AConnection.Close;
  end;
end;

procedure TWebSocketServer.OnDisconnect(AConnection: TWebSocketConnection);
var
  i: Integer;
  Client: TWebSocketClient;
begin
  for i := 0 to FClients.Count - 1 do
  begin
    Client := TWebSocketClient(FClients[i]);
    if Client.Connection = AConnection then
    begin
      LogInfo('WebSocket client disconnected: ' + Client.Id);
      Client.IsActive := False;
      Break;
    end;
  end;
end;

procedure TWebSocketServer.OnMessage(AConnection: TWebSocketConnection; const Message: string);
var
  i: Integer;
  Client: TWebSocketClient;
  WSMessage: TWebSocketMessage;
  Response: TWebSocketMessage;
begin
  // Find client
  Client := nil;
  for i := 0 to FClients.Count - 1 do
  begin
    if TWebSocketClient(FClients[i]).Connection = AConnection then
    begin
      Client := TWebSocketClient(FClients[i]);
      Break;
    end;
  end;
  
  if not Assigned(Client) then
    Exit;
    
  try
    WSMessage := JSONToWebSocketMessage(Message);
    
    case WSMessage.MessageType of
      wmtConnect:
        begin
          Client.UserId := WSMessage.Data.Get('userId', '');
          Client.BoardId := WSMessage.Data.Get('boardId', '');
          LogInfo('Client ' + Client.Id + ' joined board ' + Client.BoardId);
        end;
        
      wmtPing:
        begin
          Client.Pong;
        end;
        
      wmtPong:
        begin
          Client.LastPing := Now;
        end;
        
      wmtBoardUpdate:
        begin
          // Handle board update
          if Assigned(RealtimeManager) then
            RealtimeManager.OnBoardUpdated(nil, Client.UserId);
        end;
        
      wmtListUpdate:
        begin
          // Handle list update
          if Assigned(RealtimeManager) then
            RealtimeManager.OnListUpdated(nil, Client.UserId);
        end;
        
      wmtCardUpdate:
        begin
          // Handle card update
          if Assigned(RealtimeManager) then
            RealtimeManager.OnCardUpdated(nil, Client.UserId);
        end;
        
      wmtCardMove:
        begin
          // Handle card move
          if Assigned(RealtimeManager) then
            RealtimeManager.OnCardMoved(WSMessage.Data.Get('cardId', ''), 
              WSMessage.Data.Get('newListId', ''), 
              WSMessage.Data.Get('newSort', 0), 
              Client.UserId);
        end;
        
      wmtUserJoin:
        begin
          if Assigned(RealtimeManager) then
            RealtimeManager.OnUserJoined(Client.UserId, Client.BoardId);
        end;
        
      wmtUserLeave:
        begin
          if Assigned(RealtimeManager) then
            RealtimeManager.OnUserLeft(Client.UserId, Client.BoardId);
        end;
        
      wmtTyping:
        begin
          if Assigned(RealtimeManager) then
            RealtimeManager.OnUserTyping(Client.UserId, Client.BoardId, 
              WSMessage.Data.Get('cardId', ''));
        end;
        
      wmtComment:
        begin
          if Assigned(RealtimeManager) then
            RealtimeManager.OnCommentAdded(WSMessage.Data, Client.UserId);
        end;
    end;
    
  except
    on E: Exception do
    begin
      LogError('WebSocket message error: ' + E.Message);
      Response := CreateWebSocketMessage(wmtError, 
        TJSONObject.Create(['error', E.Message]), Client.BoardId, Client.UserId);
      Client.SendMessage(Response);
    end;
  end;
end;

procedure TWebSocketServer.OnError(AConnection: TWebSocketConnection; const Error: string);
var
  i: Integer;
  Client: TWebSocketClient;
begin
  for i := 0 to FClients.Count - 1 do
  begin
    Client := TWebSocketClient(FClients[i]);
    if Client.Connection = AConnection then
    begin
      LogError('WebSocket error for client ' + Client.Id + ': ' + Error);
      Client.IsActive := False;
      Break;
    end;
  end;
end;

// TRealtimeManager implementation
constructor TRealtimeManager.Create(AWebSocketServer: TWebSocketServer; ABoardManager: TBoardManager;
  AListManager: TListManager; ACardManager: TCardManager; AUserManager: TUserManager);
begin
  inherited Create;
  FWebSocketServer := AWebSocketServer;
  FBoardManager := ABoardManager;
  FListManager := AListManager;
  FCardManager := ACardManager;
  FUserManager := AUserManager;
end;

destructor TRealtimeManager.Destroy;
begin
  inherited Destroy;
end;

// Board events
procedure TRealtimeManager.OnBoardCreated(const Board: TBoard; const UserId: string);
var
  Message: TWebSocketMessage;
  Data: TJSONObject;
begin
  if Assigned(Board) then
  begin
    Data := TJSONObject.Create;
    Data.Add('board', Board.ToJSON);
    Data.Add('userId', UserId);
    
    Message := CreateWebSocketMessage(wmtBoardUpdate, Data, Board.Id, UserId);
    FWebSocketServer.SendToBoard(Board.Id, Message);
  end;
end;

procedure TRealtimeManager.OnBoardUpdated(const Board: TBoard; const UserId: string);
var
  Message: TWebSocketMessage;
  Data: TJSONObject;
begin
  if Assigned(Board) then
  begin
    Data := TJSONObject.Create;
    Data.Add('board', Board.ToJSON);
    Data.Add('userId', UserId);
    
    Message := CreateWebSocketMessage(wmtBoardUpdate, Data, Board.Id, UserId);
    FWebSocketServer.SendToBoard(Board.Id, Message);
  end;
end;

procedure TRealtimeManager.OnBoardDeleted(const BoardId: string; const UserId: string);
var
  Message: TWebSocketMessage;
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('boardId', BoardId);
  Data.Add('userId', UserId);
  
  Message := CreateWebSocketMessage(wmtBoardUpdate, Data, BoardId, UserId);
  FWebSocketServer.SendToBoard(BoardId, Message);
end;

procedure TRealtimeManager.OnBoardMemberAdded(const BoardId: string; const UserId: string; const MemberId: string);
var
  Message: TWebSocketMessage;
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('boardId', BoardId);
  Data.Add('userId', UserId);
  Data.Add('memberId', MemberId);
  
  Message := CreateWebSocketMessage(wmtBoardUpdate, Data, BoardId, UserId);
  FWebSocketServer.SendToBoard(BoardId, Message);
end;

procedure TRealtimeManager.OnBoardMemberRemoved(const BoardId: string; const UserId: string; const MemberId: string);
var
  Message: TWebSocketMessage;
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('boardId', BoardId);
  Data.Add('userId', UserId);
  Data.Add('memberId', MemberId);
  
  Message := CreateWebSocketMessage(wmtBoardUpdate, Data, BoardId, UserId);
  FWebSocketServer.SendToBoard(BoardId, Message);
end;

// List events
procedure TRealtimeManager.OnListCreated(const List: TList; const UserId: string);
var
  Message: TWebSocketMessage;
  Data: TJSONObject;
begin
  if Assigned(List) then
  begin
    Data := TJSONObject.Create;
    Data.Add('list', List.ToJSON);
    Data.Add('userId', UserId);
    
    Message := CreateWebSocketMessage(wmtListUpdate, Data, List.BoardId, UserId);
    FWebSocketServer.SendToBoard(List.BoardId, Message);
  end;
end;

procedure TRealtimeManager.OnListUpdated(const List: TList; const UserId: string);
var
  Message: TWebSocketMessage;
  Data: TJSONObject;
begin
  if Assigned(List) then
  begin
    Data := TJSONObject.Create;
    Data.Add('list', List.ToJSON);
    Data.Add('userId', UserId);
    
    Message := CreateWebSocketMessage(wmtListUpdate, Data, List.BoardId, UserId);
    FWebSocketServer.SendToBoard(List.BoardId, Message);
  end;
end;

procedure TRealtimeManager.OnListDeleted(const ListId: string; const UserId: string);
var
  Message: TWebSocketMessage;
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('listId', ListId);
  Data.Add('userId', UserId);
  
  Message := CreateWebSocketMessage(wmtListUpdate, Data, '', UserId);
  FWebSocketServer.Broadcast(Message);
end;

procedure TRealtimeManager.OnListMoved(const ListId: string; const NewSort: Integer; const UserId: string);
var
  Message: TWebSocketMessage;
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('listId', ListId);
  Data.Add('newSort', NewSort);
  Data.Add('userId', UserId);
  
  Message := CreateWebSocketMessage(wmtListUpdate, Data, '', UserId);
  FWebSocketServer.Broadcast(Message);
end;

// Card events
procedure TRealtimeManager.OnCardCreated(const Card: TCard; const UserId: string);
var
  Message: TWebSocketMessage;
  Data: TJSONObject;
begin
  if Assigned(Card) then
  begin
    Data := TJSONObject.Create;
    Data.Add('card', Card.ToJSON);
    Data.Add('userId', UserId);
    
    Message := CreateWebSocketMessage(wmtCardUpdate, Data, Card.BoardId, UserId);
    FWebSocketServer.SendToBoard(Card.BoardId, Message);
  end;
end;

procedure TRealtimeManager.OnCardUpdated(const Card: TCard; const UserId: string);
var
  Message: TWebSocketMessage;
  Data: TJSONObject;
begin
  if Assigned(Card) then
  begin
    Data := TJSONObject.Create;
    Data.Add('card', Card.ToJSON);
    Data.Add('userId', UserId);
    
    Message := CreateWebSocketMessage(wmtCardUpdate, Data, Card.BoardId, UserId);
    FWebSocketServer.SendToBoard(Card.BoardId, Message);
  end;
end;

procedure TRealtimeManager.OnCardDeleted(const CardId: string; const UserId: string);
var
  Message: TWebSocketMessage;
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('cardId', CardId);
  Data.Add('userId', UserId);
  
  Message := CreateWebSocketMessage(wmtCardUpdate, Data, '', UserId);
  FWebSocketServer.Broadcast(Message);
end;

procedure TRealtimeManager.OnCardMoved(const CardId: string; const NewListId: string; const NewSort: Integer; const UserId: string);
var
  Message: TWebSocketMessage;
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('cardId', CardId);
  Data.Add('newListId', NewListId);
  Data.Add('newSort', NewSort);
  Data.Add('userId', UserId);
  
  Message := CreateWebSocketMessage(wmtCardMove, Data, '', UserId);
  FWebSocketServer.Broadcast(Message);
end;

procedure TRealtimeManager.OnCardMemberAdded(const CardId: string; const UserId: string; const MemberId: string);
var
  Message: TWebSocketMessage;
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('cardId', CardId);
  Data.Add('userId', UserId);
  Data.Add('memberId', MemberId);
  
  Message := CreateWebSocketMessage(wmtCardUpdate, Data, '', UserId);
  FWebSocketServer.Broadcast(Message);
end;

procedure TRealtimeManager.OnCardMemberRemoved(const CardId: string; const UserId: string; const MemberId: string);
var
  Message: TWebSocketMessage;
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('cardId', CardId);
  Data.Add('userId', UserId);
  Data.Add('memberId', MemberId);
  
  Message := CreateWebSocketMessage(wmtCardUpdate, Data, '', UserId);
  FWebSocketServer.Broadcast(Message);
end;

// User events
procedure TRealtimeManager.OnUserJoined(const UserId: string; const BoardId: string);
var
  Message: TWebSocketMessage;
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('userId', UserId);
  Data.Add('boardId', BoardId);
  
  Message := CreateWebSocketMessage(wmtUserJoin, Data, BoardId, UserId);
  FWebSocketServer.SendToBoard(BoardId, Message);
end;

procedure TRealtimeManager.OnUserLeft(const UserId: string; const BoardId: string);
var
  Message: TWebSocketMessage;
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('userId', UserId);
  Data.Add('boardId', BoardId);
  
  Message := CreateWebSocketMessage(wmtUserLeave, Data, BoardId, UserId);
  FWebSocketServer.SendToBoard(BoardId, Message);
end;

procedure TRealtimeManager.OnUserTyping(const UserId: string; const BoardId: string; const CardId: string);
var
  Message: TWebSocketMessage;
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  Data.Add('userId', UserId);
  Data.Add('boardId', BoardId);
  Data.Add('cardId', CardId);
  
  Message := CreateWebSocketMessage(wmtTyping, Data, BoardId, UserId);
  FWebSocketServer.SendToBoard(BoardId, Message);
end;

// Activity events
procedure TRealtimeManager.OnActivityCreated(const Activity: TJSONObject; const UserId: string);
var
  Message: TWebSocketMessage;
  Data: TJSONObject;
begin
  if Assigned(Activity) then
  begin
    Data := TJSONObject.Create;
    Data.Add('activity', Activity);
    Data.Add('userId', UserId);
    
    Message := CreateWebSocketMessage(wmtActivity, Data, Activity.Get('boardId', ''), UserId);
    FWebSocketServer.SendToBoard(Activity.Get('boardId', ''), Message);
  end;
end;

procedure TRealtimeManager.OnCommentAdded(const Comment: TJSONObject; const UserId: string);
var
  Message: TWebSocketMessage;
  Data: TJSONObject;
begin
  if Assigned(Comment) then
  begin
    Data := TJSONObject.Create;
    Data.Add('comment', Comment);
    Data.Add('userId', UserId);
    
    Message := CreateWebSocketMessage(wmtComment, Data, Comment.Get('boardId', ''), UserId);
    FWebSocketServer.SendToBoard(Comment.Get('boardId', ''), Message);
  end;
end;

procedure TRealtimeManager.OnAttachmentAdded(const Attachment: TJSONObject; const UserId: string);
var
  Message: TWebSocketMessage;
  Data: TJSONObject;
begin
  if Assigned(Attachment) then
  begin
    Data := TJSONObject.Create;
    Data.Add('attachment', Attachment);
    Data.Add('userId', UserId);
    
    Message := CreateWebSocketMessage(wmtActivity, Data, Attachment.Get('boardId', ''), UserId);
    FWebSocketServer.SendToBoard(Attachment.Get('boardId', ''), Message);
  end;
end;

// Utility functions
function MessageTypeToString(const MessageType: TWebSocketMessageType): string;
begin
  case MessageType of
    wmtConnect: Result := 'connect';
    wmtDisconnect: Result := 'disconnect';
    wmtPing: Result := 'ping';
    wmtPong: Result := 'pong';
    wmtBoardUpdate: Result := 'board_update';
    wmtListUpdate: Result := 'list_update';
    wmtCardUpdate: Result := 'card_update';
    wmtCardMove: Result := 'card_move';
    wmtUserJoin: Result := 'user_join';
    wmtUserLeave: Result := 'user_leave';
    wmtTyping: Result := 'typing';
    wmtComment: Result := 'comment';
    wmtActivity: Result := 'activity';
    wmtNotification: Result := 'notification';
    wmtError: Result := 'error';
  end;
end;

function StringToMessageType(const Str: string): TWebSocketMessageType;
begin
  case Str of
    'connect': Result := wmtConnect;
    'disconnect': Result := wmtDisconnect;
    'ping': Result := wmtPing;
    'pong': Result := wmtPong;
    'board_update': Result := wmtBoardUpdate;
    'list_update': Result := wmtListUpdate;
    'card_update': Result := wmtCardUpdate;
    'card_move': Result := wmtCardMove;
    'user_join': Result := wmtUserJoin;
    'user_leave': Result := wmtUserLeave;
    'typing': Result := wmtTyping;
    'comment': Result := wmtComment;
    'activity': Result := wmtActivity;
    'notification': Result := wmtNotification;
    'error': Result := wmtError;
    else
      Result := wmtError;
  end;
end;

function CreateWebSocketMessage(const MessageType: TWebSocketMessageType; const Data: TJSONObject; const BoardId: string; const UserId: string): TWebSocketMessage;
begin
  Result.MessageType := MessageType;
  Result.Data := Data;
  Result.BoardId := BoardId;
  Result.UserId := UserId;
  Result.Timestamp := Now;
end;

function WebSocketMessageToJSON(const Message: TWebSocketMessage): string;
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.Add('type', MessageTypeToString(Message.MessageType));
    JSON.Add('data', Message.Data);
    JSON.Add('boardId', Message.BoardId);
    JSON.Add('userId', Message.UserId);
    JSON.Add('timestamp', FormatDateTime(Message.Timestamp));
    Result := JSON.AsJSON;
  finally
    JSON.Free;
  end;
end;

function JSONToWebSocketMessage(const JSON: string): TWebSocketMessage;
var
  Parser: TJSONParser;
  JSONObj: TJSONObject;
begin
  Parser := TJSONParser.Create(JSON);
  try
    JSONObj := Parser.Parse as TJSONObject;
    try
      Result.MessageType := StringToMessageType(JSONObj.Get('type', ''));
      Result.Data := JSONObj.Objects['data'];
      Result.BoardId := JSONObj.Get('boardId', '');
      Result.UserId := JSONObj.Get('userId', '');
      Result.Timestamp := ParseDateTime(JSONObj.Get('timestamp', ''));
    finally
      JSONObj.Free;
    end;
  finally
    Parser.Free;
  end;
end;

end.
