unit wekan_templates;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, wekan_core, wekan_utils;

type
  // Template engine for HTML generation
  TTemplateEngine = class
  private
    FTemplatePath: string;
    FVariables: TJSONObject;
  public
    constructor Create(const TemplatePath: string);
    destructor Destroy; override;
    
    function Render(const TemplateName: string; const Variables: TJSONObject = nil): string;
    function LoadTemplate(const TemplateName: string): string;
    function ProcessTemplate(const Template: string; const Variables: TJSONObject): string;
    function RenderPartial(const PartialName: string; const Variables: TJSONObject = nil): string;
    
    // Template helpers
    function FormatDate(const DateStr: string): string;
    function FormatDateTime(const DateTimeStr: string): string;
    function TruncateText(const Text: string; const Length: Integer): string;
    function EscapeHTML(const Text: string): string;
    function GenerateAvatar(const UserId: string; const Size: Integer = 32): string;
    function GenerateBoardIcon(const BoardId: string; const Color: string): string;
    function GenerateCardColor(const Color: string): string;
    
    property TemplatePath: string read FTemplatePath write FTemplatePath;
  end;

  // HTML template generator
  THTMLGenerator = class
  private
    FTemplateEngine: TTemplateEngine;
  public
    constructor Create(const TemplatePath: string);
    destructor Destroy; override;
    
    // Page templates
    function GenerateHomePage(const User: TJSONObject = nil): string;
    function GenerateLoginPage(const Error: string = ''): string;
    function GenerateRegisterPage(const Error: string = ''): string;
    function GenerateBoardsPage(const User: TJSONObject; const Boards: TJSONArray): string;
    function GenerateBoardPage(const User: TJSONObject; const Board: TJSONObject; const Lists: TJSONArray): string;
    function GenerateCardPage(const User: TJSONObject; const Card: TJSONObject; const Board: TJSONObject): string;
    
    // Component templates
    function GenerateBoardCard(const Board: TJSONObject): string;
    function GenerateListCard(const List: TJSONObject): string;
    function GenerateCardMini(const Card: TJSONObject): string;
    function GenerateUserAvatar(const User: TJSONObject; const Size: Integer = 32): string;
    function GenerateActivityItem(const Activity: TJSONObject): string;
    function GenerateCommentItem(const Comment: TJSONObject): string;
    
    // Layout templates
    function GenerateHeader(const User: TJSONObject = nil): string;
    function GenerateSidebar(const User: TJSONObject; const Boards: TJSONArray): string;
    function GenerateFooter: string;
    function GenerateNavigation(const User: TJSONObject = nil): string;
    
    // Form templates
    function GenerateLoginForm(const Error: string = ''): string;
    function GenerateRegisterForm(const Error: string = ''): string;
    function GenerateBoardForm(const Board: TJSONObject = nil): string;
    function GenerateListForm(const List: TJSONObject = nil): string;
    function GenerateCardForm(const Card: TJSONObject = nil): string;
    
    // Error templates
    function GenerateErrorPage(const Code: Integer; const Message: string): string;
    function GenerateNotFoundPage: string;
    function GenerateUnauthorizedPage: string;
  end;

// Global template engine
var
  TemplateEngine: TTemplateEngine;
  HTMLGenerator: THTMLGenerator;

implementation

uses
  strutils, dateutils, wekan_auth;

// TTemplateEngine implementation
constructor TTemplateEngine.Create(const TemplatePath: string);
begin
  inherited Create;
  FTemplatePath := TemplatePath;
  FVariables := TJSONObject.Create;
end;

destructor TTemplateEngine.Destroy;
begin
  if Assigned(FVariables) then
    FVariables.Free;
  inherited Destroy;
end;

function TTemplateEngine.Render(const TemplateName: string; const Variables: TJSONObject): string;
var
  Template: string;
begin
  Template := LoadTemplate(TemplateName);
  Result := ProcessTemplate(Template, Variables);
end;

function TTemplateEngine.LoadTemplate(const TemplateName: string): string;
var
  TemplateFile: string;
  TemplateList: TStringList;
begin
  TemplateFile := FTemplatePath + TemplateName + '.html';
  if FileExists(TemplateFile) then
  begin
    TemplateList := TStringList.Create;
    try
      TemplateList.LoadFromFile(TemplateFile);
      Result := TemplateList.Text;
    finally
      TemplateList.Free;
    end;
  end
  else
    Result := '<html><body><h1>Template not found: ' + TemplateName + '</h1></body></html>';
end;

function TTemplateEngine.ProcessTemplate(const Template: string; const Variables: TJSONObject): string;
var
  i: Integer;
  Key, Value: string;
  ProcessedTemplate: string;
begin
  ProcessedTemplate := Template;
  
  if Assigned(Variables) then
  begin
    for i := 0 to Variables.Count - 1 do
    begin
      Key := Variables.Names[i];
      Value := Variables.Items[i].AsString;
      ProcessedTemplate := StringReplace(ProcessedTemplate, '{{' + Key + '}}', Value, [rfReplaceAll]);
    end;
  end;
  
  // Process partials
  while Pos('{{>', ProcessedTemplate) > 0 do
  begin
    // Extract partial name and replace with rendered content
    // This is a simplified implementation
    ProcessedTemplate := StringReplace(ProcessedTemplate, '{{>header}}', RenderPartial('header'), [rfReplaceAll]);
    ProcessedTemplate := StringReplace(ProcessedTemplate, '{{>footer}}', RenderPartial('footer'), [rfReplaceAll]);
    ProcessedTemplate := StringReplace(ProcessedTemplate, '{{>sidebar}}', RenderPartial('sidebar'), [rfReplaceAll]);
  end;
  
  Result := ProcessedTemplate;
end;

function TTemplateEngine.RenderPartial(const PartialName: string; const Variables: TJSONObject): string;
var
  Partial: string;
begin
  Partial := LoadTemplate('partials/' + PartialName);
  Result := ProcessTemplate(Partial, Variables);
end;

// Template helpers
function TTemplateEngine.FormatDate(const DateStr: string): string;
var
  DateTime: TDateTime;
begin
  DateTime := ParseDateTime(DateStr);
  Result := FormatDateTime('yyyy-mm-dd', DateTime);
end;

function TTemplateEngine.FormatDateTime(const DateTimeStr: string): string;
var
  DateTime: TDateTime;
begin
  DateTime := ParseDateTime(DateTimeStr);
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', DateTime);
end;

function TTemplateEngine.TruncateText(const Text: string; const Length: Integer): string;
begin
  if Length(Text) <= Length then
    Result := Text
  else
    Result := Copy(Text, 1, Length) + '...';
end;

function TTemplateEngine.EscapeHTML(const Text: string): string;
begin
  Result := SanitizeInput(Text);
end;

function TTemplateEngine.GenerateAvatar(const UserId: string; const Size: Integer): string;
begin
  Result := '<div class="avatar" style="width: ' + IntToStr(Size) + 'px; height: ' + IntToStr(Size) + 'px; background-color: #' + 
            Copy(MD5Hash(UserId), 1, 6) + '; border-radius: 50%; display: inline-block; text-align: center; line-height: ' + 
            IntToStr(Size) + 'px; color: white; font-weight: bold;">' + 
            UpperCase(Copy(UserId, 1, 2)) + '</div>';
end;

function TTemplateEngine.GenerateBoardIcon(const BoardId: string; const Color: string): string;
begin
  Result := '<div class="board-icon" style="background-color: ' + Color + '; width: 40px; height: 40px; border-radius: 4px; display: inline-block;"></div>';
end;

function TTemplateEngine.GenerateCardColor(const Color: string): string;
begin
  if Color = '' then
    Result := '#ffffff'
  else
    Result := Color;
end;

// THTMLGenerator implementation
constructor THTMLGenerator.Create(const TemplatePath: string);
begin
  inherited Create;
  FTemplateEngine := TTemplateEngine.Create(TemplatePath);
end;

destructor THTMLGenerator.Destroy;
begin
  if Assigned(FTemplateEngine) then
    FTemplateEngine.Free;
  inherited Destroy;
end;

// Page templates
function THTMLGenerator.GenerateHomePage(const User: TJSONObject): string;
var
  Variables: TJSONObject;
begin
  Variables := TJSONObject.Create;
  try
    Variables.Add('title', 'WeKan WAMI - Home');
    Variables.Add('version', '1.0.0');
    if Assigned(User) then
      Variables.Add('user', User.Get('username', 'Guest'))
    else
      Variables.Add('user', 'Guest');
    Variables.Add('welcome_message', 'Welcome to WeKan WAMI - Your FreePascal Kanban Board');
    
    Result := FTemplateEngine.Render('home', Variables);
  finally
    Variables.Free;
  end;
end;

function THTMLGenerator.GenerateLoginPage(const Error: string): string;
var
  Variables: TJSONObject;
begin
  Variables := TJSONObject.Create;
  try
    Variables.Add('title', 'Login - WeKan WAMI');
    Variables.Add('error', Error);
    Variables.Add('login_form', GenerateLoginForm(Error));
    
    Result := FTemplateEngine.Render('login', Variables);
  finally
    Variables.Free;
  end;
end;

function THTMLGenerator.GenerateRegisterPage(const Error: string): string;
var
  Variables: TJSONObject;
begin
  Variables := TJSONObject.Create;
  try
    Variables.Add('title', 'Register - WeKan WAMI');
    Variables.Add('error', Error);
    Variables.Add('register_form', GenerateRegisterForm(Error));
    
    Result := FTemplateEngine.Render('register', Variables);
  finally
    Variables.Free;
  end;
end;

function THTMLGenerator.GenerateBoardsPage(const User: TJSONObject; const Boards: TJSONArray): string;
var
  Variables: TJSONObject;
  i: Integer;
  Board: TJSONObject;
  BoardCards: string;
begin
  Variables := TJSONObject.Create;
  try
    Variables.Add('title', 'Boards - WeKan WAMI');
    if Assigned(User) then
      Variables.Add('user', User.Get('username', 'Guest'))
    else
      Variables.Add('user', 'Guest');
    
    // Generate board cards
    BoardCards := '';
    if Assigned(Boards) then
    begin
      for i := 0 to Boards.Count - 1 do
      begin
        Board := Boards.Objects[i];
        BoardCards := BoardCards + GenerateBoardCard(Board);
      end;
    end;
    Variables.Add('boards', BoardCards);
    
    Result := FTemplateEngine.Render('boards', Variables);
  finally
    Variables.Free;
  end;
end;

function THTMLGenerator.GenerateBoardPage(const User: TJSONObject; const Board: TJSONObject; const Lists: TJSONArray): string;
var
  Variables: TJSONObject;
  i: Integer;
  List: TJSONObject;
  ListCards: string;
begin
  Variables := TJSONObject.Create;
  try
    Variables.Add('title', Board.Get('title', 'Board') + ' - WeKan WAMI');
    if Assigned(User) then
      Variables.Add('user', User.Get('username', 'Guest'))
    else
      Variables.Add('user', 'Guest');
    Variables.Add('board', Board.Get('title', 'Board'));
    Variables.Add('board_id', Board.Get('_id', ''));
    Variables.Add('board_description', Board.Get('description', ''));
    
    // Generate list cards
    ListCards := '';
    if Assigned(Lists) then
    begin
      for i := 0 to Lists.Count - 1 do
      begin
        List := Lists.Objects[i];
        ListCards := ListCards + GenerateListCard(List);
      end;
    end;
    Variables.Add('lists', ListCards);
    
    Result := FTemplateEngine.Render('board', Variables);
  finally
    Variables.Free;
  end;
end;

function THTMLGenerator.GenerateCardPage(const User: TJSONObject; const Card: TJSONObject; const Board: TJSONObject): string;
var
  Variables: TJSONObject;
begin
  Variables := TJSONObject.Create;
  try
    Variables.Add('title', Card.Get('title', 'Card') + ' - WeKan WAMI');
    if Assigned(User) then
      Variables.Add('user', User.Get('username', 'Guest'))
    else
      Variables.Add('user', 'Guest');
    Variables.Add('card', Card.Get('title', 'Card'));
    Variables.Add('card_id', Card.Get('_id', ''));
    Variables.Add('card_description', Card.Get('description', ''));
    Variables.Add('board', Board.Get('title', 'Board'));
    Variables.Add('board_id', Board.Get('_id', ''));
    
    Result := FTemplateEngine.Render('card', Variables);
  finally
    Variables.Free;
  end;
end;

// Component templates
function THTMLGenerator.GenerateBoardCard(const Board: TJSONObject): string;
var
  Variables: TJSONObject;
begin
  Variables := TJSONObject.Create;
  try
    Variables.Add('board_id', Board.Get('_id', ''));
    Variables.Add('board_title', Board.Get('title', 'Untitled Board'));
    Variables.Add('board_description', Board.Get('description', ''));
    Variables.Add('board_color', Board.Get('color', 'belize'));
    Variables.Add('board_icon', FTemplateEngine.GenerateBoardIcon(Board.Get('_id', ''), Board.Get('color', 'belize')));
    Variables.Add('board_created', FTemplateEngine.FormatDate(Board.Get('createdAt', '')));
    Variables.Add('board_modified', FTemplateEngine.FormatDate(Board.Get('modifiedAt', '')));
    
    Result := FTemplateEngine.Render('partials/board-card', Variables);
  finally
    Variables.Free;
  end;
end;

function THTMLGenerator.GenerateListCard(const List: TJSONObject): string;
var
  Variables: TJSONObject;
begin
  Variables := TJSONObject.Create;
  try
    Variables.Add('list_id', List.Get('_id', ''));
    Variables.Add('list_title', List.Get('title', 'Untitled List'));
    Variables.Add('list_color', List.Get('color', ''));
    Variables.Add('list_sort', IntToStr(List.Get('sort', 0)));
    
    Result := FTemplateEngine.Render('partials/list-card', Variables);
  finally
    Variables.Free;
  end;
end;

function THTMLGenerator.GenerateCardMini(const Card: TJSONObject): string;
var
  Variables: TJSONObject;
begin
  Variables := TJSONObject.Create;
  try
    Variables.Add('card_id', Card.Get('_id', ''));
    Variables.Add('card_title', Card.Get('title', 'Untitled Card'));
    Variables.Add('card_description', FTemplateEngine.TruncateText(Card.Get('description', ''), 100));
    Variables.Add('card_color', FTemplateEngine.GenerateCardColor(Card.Get('color', '')));
    Variables.Add('card_due', Card.Get('dueAt', ''));
    Variables.Add('card_created', FTemplateEngine.FormatDate(Card.Get('createdAt', '')));
    
    Result := FTemplateEngine.Render('partials/card-mini', Variables);
  finally
    Variables.Free;
  end;
end;

function THTMLGenerator.GenerateUserAvatar(const User: TJSONObject; const Size: Integer): string;
begin
  Result := FTemplateEngine.GenerateAvatar(User.Get('_id', ''), Size);
end;

function THTMLGenerator.GenerateActivityItem(const Activity: TJSONObject): string;
var
  Variables: TJSONObject;
begin
  Variables := TJSONObject.Create;
  try
    Variables.Add('activity_id', Activity.Get('_id', ''));
    Variables.Add('activity_type', Activity.Get('type', ''));
    Variables.Add('activity_title', Activity.Get('title', ''));
    Variables.Add('activity_user', Activity.Get('username', ''));
    Variables.Add('activity_created', FTemplateEngine.FormatDateTime(Activity.Get('createdAt', '')));
    Variables.Add('user_avatar', FTemplateEngine.GenerateAvatar(Activity.Get('userId', ''), 24));
    
    Result := FTemplateEngine.Render('partials/activity-item', Variables);
  finally
    Variables.Free;
  end;
end;

function THTMLGenerator.GenerateCommentItem(const Comment: TJSONObject): string;
var
  Variables: TJSONObject;
begin
  Variables := TJSONObject.Create;
  try
    Variables.Add('comment_id', Comment.Get('_id', ''));
    Variables.Add('comment_text', Comment.Get('text', ''));
    Variables.Add('comment_author', Comment.Get('author', ''));
    Variables.Add('comment_created', FTemplateEngine.FormatDateTime(Comment.Get('createdAt', '')));
    Variables.Add('author_avatar', FTemplateEngine.GenerateAvatar(Comment.Get('authorId', ''), 32));
    
    Result := FTemplateEngine.Render('partials/comment-item', Variables);
  finally
    Variables.Free;
  end;
end;

// Layout templates
function THTMLGenerator.GenerateHeader(const User: TJSONObject): string;
var
  Variables: TJSONObject;
begin
  Variables := TJSONObject.Create;
  try
    Variables.Add('title', 'WeKan WAMI');
    Variables.Add('version', '1.0.0');
    if Assigned(User) then
    begin
      Variables.Add('user', User.Get('username', 'Guest'));
      Variables.Add('user_avatar', GenerateUserAvatar(User, 32));
      Variables.Add('is_logged_in', 'true');
    end
    else
    begin
      Variables.Add('user', 'Guest');
      Variables.Add('is_logged_in', 'false');
    end;
    
    Result := FTemplateEngine.Render('partials/header', Variables);
  finally
    Variables.Free;
  end;
end;

function THTMLGenerator.GenerateSidebar(const User: TJSONObject; const Boards: TJSONArray): string;
var
  Variables: TJSONObject;
  i: Integer;
  Board: TJSONObject;
  BoardList: string;
begin
  Variables := TJSONObject.Create;
  try
    if Assigned(User) then
      Variables.Add('user', User.Get('username', 'Guest'))
    else
      Variables.Add('user', 'Guest');
    
    // Generate board list
    BoardList := '';
    if Assigned(Boards) then
    begin
      for i := 0 to Boards.Count - 1 do
      begin
        Board := Boards.Objects[i];
        BoardList := BoardList + '<li><a href="/b/' + Board.Get('_id', '') + '/' + 
                    FTemplateEngine.GenerateSlug(Board.Get('title', '')) + '">' + 
                    Board.Get('title', 'Untitled Board') + '</a></li>';
      end;
    end;
    Variables.Add('boards', BoardList);
    
    Result := FTemplateEngine.Render('partials/sidebar', Variables);
  finally
    Variables.Free;
  end;
end;

function THTMLGenerator.GenerateFooter: string;
var
  Variables: TJSONObject;
begin
  Variables := TJSONObject.Create;
  try
    Variables.Add('year', IntToStr(YearOf(Now)));
    Variables.Add('version', '1.0.0');
    
    Result := FTemplateEngine.Render('partials/footer', Variables);
  finally
    Variables.Free;
  end;
end;

function THTMLGenerator.GenerateNavigation(const User: TJSONObject): string;
var
  Variables: TJSONObject;
begin
  Variables := TJSONObject.Create;
  try
    if Assigned(User) then
    begin
      Variables.Add('user', User.Get('username', 'Guest'));
      Variables.Add('is_logged_in', 'true');
    end
    else
    begin
      Variables.Add('user', 'Guest');
      Variables.Add('is_logged_in', 'false');
    end;
    
    Result := FTemplateEngine.Render('partials/navigation', Variables);
  finally
    Variables.Free;
  end;
end;

// Form templates
function THTMLGenerator.GenerateLoginForm(const Error: string): string;
var
  Variables: TJSONObject;
begin
  Variables := TJSONObject.Create;
  try
    Variables.Add('error', Error);
    Variables.Add('error_class', IfThen(Error <> '', 'error', ''));
    
    Result := FTemplateEngine.Render('partials/login-form', Variables);
  finally
    Variables.Free;
  end;
end;

function THTMLGenerator.GenerateRegisterForm(const Error: string): string;
var
  Variables: TJSONObject;
begin
  Variables := TJSONObject.Create;
  try
    Variables.Add('error', Error);
    Variables.Add('error_class', IfThen(Error <> '', 'error', ''));
    
    Result := FTemplateEngine.Render('partials/register-form', Variables);
  finally
    Variables.Free;
  end;
end;

function THTMLGenerator.GenerateBoardForm(const Board: TJSONObject): string;
var
  Variables: TJSONObject;
begin
  Variables := TJSONObject.Create;
  try
    if Assigned(Board) then
    begin
      Variables.Add('board_id', Board.Get('_id', ''));
      Variables.Add('board_title', Board.Get('title', ''));
      Variables.Add('board_description', Board.Get('description', ''));
      Variables.Add('board_color', Board.Get('color', 'belize'));
      Variables.Add('form_action', '/api/boards/' + Board.Get('_id', ''));
      Variables.Add('form_method', 'PUT');
      Variables.Add('submit_text', 'Update Board');
    end
    else
    begin
      Variables.Add('board_id', '');
      Variables.Add('board_title', '');
      Variables.Add('board_description', '');
      Variables.Add('board_color', 'belize');
      Variables.Add('form_action', '/api/boards');
      Variables.Add('form_method', 'POST');
      Variables.Add('submit_text', 'Create Board');
    end;
    
    Result := FTemplateEngine.Render('partials/board-form', Variables);
  finally
    Variables.Free;
  end;
end;

function THTMLGenerator.GenerateListForm(const List: TJSONObject): string;
var
  Variables: TJSONObject;
begin
  Variables := TJSONObject.Create;
  try
    if Assigned(List) then
    begin
      Variables.Add('list_id', List.Get('_id', ''));
      Variables.Add('list_title', List.Get('title', ''));
      Variables.Add('list_color', List.Get('color', ''));
      Variables.Add('form_action', '/api/lists/' + List.Get('_id', ''));
      Variables.Add('form_method', 'PUT');
      Variables.Add('submit_text', 'Update List');
    end
    else
    begin
      Variables.Add('list_id', '');
      Variables.Add('list_title', '');
      Variables.Add('list_color', '');
      Variables.Add('form_action', '/api/lists');
      Variables.Add('form_method', 'POST');
      Variables.Add('submit_text', 'Create List');
    end;
    
    Result := FTemplateEngine.Render('partials/list-form', Variables);
  finally
    Variables.Free;
  end;
end;

function THTMLGenerator.GenerateCardForm(const Card: TJSONObject): string;
var
  Variables: TJSONObject;
begin
  Variables := TJSONObject.Create;
  try
    if Assigned(Card) then
    begin
      Variables.Add('card_id', Card.Get('_id', ''));
      Variables.Add('card_title', Card.Get('title', ''));
      Variables.Add('card_description', Card.Get('description', ''));
      Variables.Add('card_due', Card.Get('dueAt', ''));
      Variables.Add('card_color', Card.Get('color', ''));
      Variables.Add('form_action', '/api/cards/' + Card.Get('_id', ''));
      Variables.Add('form_method', 'PUT');
      Variables.Add('submit_text', 'Update Card');
    end
    else
    begin
      Variables.Add('card_id', '');
      Variables.Add('card_title', '');
      Variables.Add('card_description', '');
      Variables.Add('card_due', '');
      Variables.Add('card_color', '');
      Variables.Add('form_action', '/api/cards');
      Variables.Add('form_method', 'POST');
      Variables.Add('submit_text', 'Create Card');
    end;
    
    Result := FTemplateEngine.Render('partials/card-form', Variables);
  finally
    Variables.Free;
  end;
end;

// Error templates
function THTMLGenerator.GenerateErrorPage(const Code: Integer; const Message: string): string;
var
  Variables: TJSONObject;
begin
  Variables := TJSONObject.Create;
  try
    Variables.Add('title', 'Error ' + IntToStr(Code) + ' - WeKan WAMI');
    Variables.Add('error_code', IntToStr(Code));
    Variables.Add('error_message', Message);
    
    case Code of
      400: Variables.Add('error_title', 'Bad Request');
      401: Variables.Add('error_title', 'Unauthorized');
      403: Variables.Add('error_title', 'Forbidden');
      404: Variables.Add('error_title', 'Not Found');
      500: Variables.Add('error_title', 'Internal Server Error');
      else
        Variables.Add('error_title', 'Error');
    end;
    
    Result := FTemplateEngine.Render('error', Variables);
  finally
    Variables.Free;
  end;
end;

function THTMLGenerator.GenerateNotFoundPage: string;
begin
  Result := GenerateErrorPage(404, 'The page you are looking for could not be found.');
end;

function THTMLGenerator.GenerateUnauthorizedPage: string;
begin
  Result := GenerateErrorPage(401, 'You are not authorized to access this page.');
end;

end.
