program wekan;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  SysUtils, fphttpapp, HTTPDefs, httproute, Classes, fpjson, jsonparser, StrUtils, DateUtils, Process;

// MongoDB CLI interaction functions
function ExecuteMongoshCommand(const Command: string): string; forward;
function FindDocuments(const Collection, Query: string): TJSONArray; forward;
function FindOneDocument(const Collection, Query: string): TJSONObject; forward;
function InsertDocument(const Collection, Document: string): string; forward;
function UpdateDocument(const Collection, Query, Update: string): string; forward;
function DeleteDocument(const Collection, Query: string): string; forward;

// Translation-related functions
function Translate(const Key: string): string; forward;
function DetectLanguage(AcceptLanguageHeader: string): string; forward;
function LoadTranslations(const LanguageCode: string): Boolean; forward;
procedure InitLanguages; forward;
function IsCurrentLanguageRTL: Boolean; forward;
procedure SetLanguageFromRequest(aRequest: TRequest); forward;
function GenerateHtmlHead(const Title: string): string; forward;
function GenerateHtmlFooter: string; forward;
function GenerateLanguageSwitcher: string; forward;
function ExtractCookieValue(const CookieHeader, CookieName: string): string; forward;

// Forward declarations for endpoint procedures
procedure allPagesEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure loginEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure registerEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure forgotPasswordEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure allBoardsEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure userSettingsEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure notificationsEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure publicEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure boardEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure cardEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure shortCutsEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure templatesEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure myCardsEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure dueCardsEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure globalSearchEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure brokenCardsEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure importEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure adminSettingEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure informationEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure peopleEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure adminReportsEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure uploadEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure translationEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure jsonEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure screenInfoEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure serveStaticFileEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
procedure catchallEndpoint(aRequest: TRequest; aResponse: TResponse); forward;
// apiProfileEndpoint implementation provided below

type
  TTranslation = record
    Key: string;
    Value: string;
  end;

  TLanguageInfo = record
    Code: string;
    Tag: string;
    Name: string;
    RTL: Boolean;
  end;

var
  MongoUrlValue: string = 'mongodb://localhost:27019/wekan';
  // Global translation dictionary
  Translations: array of TTranslation;
  CurrentLanguage: string = 'en';
  LanguageInfos: array of TLanguageInfo;

// Helper function to extract cookie value from Cookie header
function ExtractCookieValue(const CookieHeader, CookieName: string): string;
var
  CookieStart, CookieEnd: Integer;
  SearchStr: string;
begin
  Result := '';
  SearchStr := CookieName + '=';
  CookieStart := Pos(SearchStr, CookieHeader);
  
  if CookieStart > 0 then
  begin
    CookieStart := CookieStart + Length(SearchStr);
    CookieEnd := Pos(';', Copy(CookieHeader, CookieStart, Length(CookieHeader)));
    
    if CookieEnd = 0 then
      Result := Copy(CookieHeader, CookieStart, Length(CookieHeader))
    else
      Result := Copy(CookieHeader, CookieStart, CookieEnd - 1);
  end;
end;

// MongoDB CLI interaction functions

// Execute a command in mongosh and return the JSON output
function ExecuteMongoshCommand(const Command: string): string;
var
  TempScript: string;
  ScriptFile: TextFile;
  Process: TProcess;
  ResultFile: TStringList;
  BytesRead: LongInt;
  Buffer: array[0..4095] of Byte;
  MemStream: TMemoryStream;
begin
  // Create a temporary script file
  TempScript := GetTempFileName + '.js';
  AssignFile(ScriptFile, TempScript);
  Rewrite(ScriptFile);
  try
    // Write the mongosh command to the script file
    Writeln(ScriptFile, 'conn = db.getMongo("mongodb://localhost:27019/wekan");');
    Writeln(ScriptFile, 'result = ' + Command + ';');
    Writeln(ScriptFile, 'print(JSON.stringify(result));');
    Writeln(ScriptFile, 'exit;');
  finally
    CloseFile(ScriptFile);
  end;
  
  // Execute mongosh directly using TProcess
  Process := TProcess.Create(nil);
  MemStream := TMemoryStream.Create;
  try
    Process.Executable := 'mongosh';
    Process.Parameters.Add('--quiet');
    Process.Parameters.Add('--file');
    Process.Parameters.Add(TempScript);
    
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];
    Process.Execute;
    
    // Read output
    repeat
      BytesRead := Process.Output.Read(Buffer, SizeOf(Buffer));
      if BytesRead > 0 then
        MemStream.Write(Buffer, BytesRead);
    until BytesRead = 0;
    
    MemStream.Position := 0;
    ResultFile := TStringList.Create;
    try
      ResultFile.LoadFromStream(MemStream);
      Result := ResultFile.Text;
    finally
      ResultFile.Free;
    end;
  finally
    Process.Free;
    MemStream.Free;
    DeleteFile(TempScript);
  end;
  
  // Clean up any newlines and whitespace
  Result := Trim(Result);
end;

// Function to find documents in a collection
function FindDocuments(const Collection, Query: string): TJSONArray;
var
  Command, OutputStr: string;
  JsonData: TJSONData;
begin
  Command := 'db.' + Collection + '.find(' + Query + ').toArray()';
  OutputStr := ExecuteMongoshCommand(Command);
  
  try
    JsonData := GetJSON(OutputStr);
    if Assigned(JsonData) and (JsonData.JSONType = jtArray) then
      Result := TJSONArray(JsonData)
    else
      Result := TJSONArray.Create;
  except
    on E: Exception do
    begin
      Writeln('Error parsing JSON result: ' + E.Message);
      Result := TJSONArray.Create;
    end;
  end;
end;

// Function to find a single document in a collection
function FindOneDocument(const Collection, Query: string): TJSONObject;
var
  Command, OutputStr: string;
  JsonData: TJSONData;
begin
  Command := 'db.' + Collection + '.findOne(' + Query + ')';
  OutputStr := ExecuteMongoshCommand(Command);
  
  try
    JsonData := GetJSON(OutputStr);
    if Assigned(JsonData) and (JsonData.JSONType = jtObject) then
      Result := TJSONObject(JsonData)
    else
      Result := TJSONObject.Create;
  except
    on E: Exception do
    begin
      Writeln('Error parsing JSON result: ' + E.Message);
      Result := TJSONObject.Create;
    end;
  end;
end;

// Function to insert a document into a collection
function InsertDocument(const Collection, Document: string): string;
var
  Command: string;
begin
  Command := 'db.' + Collection + '.insertOne(' + Document + ')';
  Result := ExecuteMongoshCommand(Command);
end;

// Function to update a document in a collection
function UpdateDocument(const Collection, Query, Update: string): string;
var
  Command: string;
begin
  Command := 'db.' + Collection + '.updateOne(' + Query + ', ' + Update + ')';
  Result := ExecuteMongoshCommand(Command);
end;

// Function to delete a document from a collection
function DeleteDocument(const Collection, Query: string): string;
var
  Command: string;
begin
  Command := 'db.' + Collection + '.deleteOne(' + Query + ')';
  Result := ExecuteMongoshCommand(Command);
end;

// WebBrowserName function and other procedures remain unchanged...

// Implementation of translationEndpoint
procedure translationEndpoint(aRequest: TRequest; aResponse: TResponse);
var
  QueryLang: string;
  RedirectURL: string;
  LangInfo: TJSONObject;
  i: Integer;
  IsRTL: Boolean;
begin
  // Check if a specific language is requested
  QueryLang := aRequest.QueryFields.Values['lang'];
  if QueryLang <> '' then
  begin
    // Set the requested language
    if LoadTranslations(QueryLang) then
    begin
      CurrentLanguage := QueryLang;
      
      // Set a cookie to remember the language choice
      aResponse.SetCustomHeader('Set-Cookie', 'wekan_language=' + CurrentLanguage + '; Path=/; Max-Age=31536000');
      
      // Check if we have a redirect URL after language change
      RedirectURL := aRequest.QueryFields.Values['redirect'];
      if RedirectURL <> '' then
      begin
        aResponse.Code := 302; // Found - redirect
        aResponse.SetCustomHeader('Location', RedirectURL);
        aResponse.SendContent;
        Exit;
      end;
    end
    else
    begin
      aResponse.Code := 404;
      aResponse.Content := Format('Language "%s" not found', [QueryLang]);
      aResponse.ContentType := 'text/plain';
      aResponse.SendContent;
      Exit;
    end;
  end;

  // Return information about the current language
  IsRTL := IsCurrentLanguageRTL;
  LangInfo := TJSONObject.Create;
  try
    LangInfo.Add('currentLanguage', CurrentLanguage);
    LangInfo.Add('isRTL', IsRTL);
    
    // List available languages
    LangInfo.Add('availableLanguages', TJSONArray.Create);
    for i := 0 to Length(LanguageInfos) - 1 do
    begin
      if LanguageInfos[i].Tag <> '' then
      begin
        with TJSONObject.Create do
        begin
          Add('tag', LanguageInfos[i].Tag);
          Add('code', LanguageInfos[i].Code);
          Add('name', LanguageInfos[i].Name);
          Add('rtl', LanguageInfos[i].RTL);
          // Add the language object to the availableLanguages array
          TJSONArray(LangInfo.Find('availableLanguages')).Add(TJSONObject(Items[Count-1]));
        end;
      end;
    end;
    
    // If it's an API request, return JSON
    if SameText(aRequest.GetFieldByName('Accept'), 'application/json') or
       (aRequest.QueryFields.Values['format'] = 'json') then
    begin
      aResponse.Content := LangInfo.AsJSON;
      aResponse.Code := 200;
      aResponse.ContentType := 'application/json';
      aResponse.ContentLength := Length(aResponse.Content);
      aResponse.SendContent;
    end
    else
    begin
      // Otherwise, return a language selection page
      with aResponse.Contents do
      begin
        Add(GenerateHtmlHead(Translate('select-language')));
        Add('<h1>' + Translate('select-language') + '</h1>');
        
        Add('<div class="language-list">');
        Add('<ul>');
        for i := 0 to Length(LanguageInfos) - 1 do
        begin
          if (LanguageInfos[i].Tag <> '') and (LanguageInfos[i].Name <> '') then
          begin
            Add(Format('<li><a href="/translation?lang=%s">%s</a></li>', 
                      [LanguageInfos[i].Tag, LanguageInfos[i].Name]));
          end;
        end;
        Add('</ul>');
        Add('</div>');
        
        Add(GenerateHtmlFooter);
      end;
      
      aResponse.Code := 200;
      aResponse.ContentType := 'text/html';
      aResponse.ContentLength := Length(aResponse.Content);
      aResponse.SendContent;
    end;
  finally
    LangInfo.Free;
  end;
end;

// Implementation of endpoint procedures
procedure allPagesEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  SetLanguageFromRequest(aRequest);
  
  with aResponse.Contents do
  begin
    Add(GenerateHtmlHead(Translate('app-title')));
    Add(GenerateLanguageSwitcher);
    Add('<h1>' + Translate('app-title') + '</h1>');
    Add('<p>' + Translate('welcome-message') + '</p>');
    Add('<ul>');
    Add('<li><a href="/sign-in">' + Translate('sign-in') + '</a></li>');
    Add('<li><a href="/sign-up">' + Translate('sign-up') + '</a></li>');
    Add('</ul>');
    Add(GenerateHtmlFooter);
  end;
  
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure loginEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  SetLanguageFromRequest(aRequest);
  
  with aResponse.Contents do
  begin
    Add(GenerateHtmlHead(Translate('sign-in')));
    Add(GenerateLanguageSwitcher);
    Add('<h1>' + Translate('sign-in') + '</h1>');
    Add('<form action="/api/users/login" method="post">');
    Add('  <div>');
    Add('    <label for="username">' + Translate('username') + '</label>');
    Add('    <input type="text" id="username" name="username" required>');
    Add('  </div>');
    Add('  <div>');
    Add('    <label for="password">' + Translate('password') + '</label>');
    Add('    <input type="password" id="password" name="password" required>');
    Add('  </div>');
    Add('  <div>');
    Add('    <button type="submit">' + Translate('sign-in') + '</button>');
    Add('  </div>');
    Add('</form>');
    Add('<p><a href="/forgot-password">' + Translate('forgot-password') + '</a></p>');
    Add(GenerateHtmlFooter);
  end;
  
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure registerEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  SetLanguageFromRequest(aRequest);
  
  with aResponse.Contents do
  begin
    Add(GenerateHtmlHead(Translate('sign-up')));
    Add(GenerateLanguageSwitcher);
    Add('<h1>' + Translate('sign-up') + '</h1>');
    Add('<form action="/api/users/register" method="post">');
    Add('  <div>');
    Add('    <label for="username">' + Translate('username') + '</label>');
    Add('    <input type="text" id="username" name="username" required>');
    Add('  </div>');
    Add('  <div>');
    Add('    <label for="email">' + Translate('email') + '</label>');
    Add('    <input type="email" id="email" name="email" required>');
    Add('  </div>');
    Add('  <div>');
    Add('    <label for="password">' + Translate('password') + '</label>');
    Add('    <input type="password" id="password" name="password" required>');
    Add('  </div>');
    Add('  <div>');
    Add('    <label for="confirm">' + Translate('confirm-password') + '</label>');
    Add('    <input type="password" id="confirm" name="confirm" required>');
    Add('  </div>');
    Add('  <div>');
    Add('    <button type="submit">' + Translate('sign-up') + '</button>');
    Add('  </div>');
    Add('</form>');
    Add(GenerateHtmlFooter);
  end;
  
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure forgotPasswordEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  SetLanguageFromRequest(aRequest);
  
  with aResponse.Contents do
  begin
    Add(GenerateHtmlHead(Translate('forgot-password')));
    Add(GenerateLanguageSwitcher);
    Add('<h1>' + Translate('forgot-password') + '</h1>');
    Add('<form action="/api/users/forgot" method="post">');
    Add('  <div>');
    Add('    <label for="email">' + Translate('email') + '</label>');
    Add('    <input type="email" id="email" name="email" required>');
    Add('  </div>');
    Add('  <div>');
    Add('    <button type="submit">' + Translate('reset-password') + '</button>');
    Add('  </div>');
    Add('</form>');
    Add(GenerateHtmlFooter);
  end;
  
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure allBoardsEndpoint(aRequest: TRequest; aResponse: TResponse);
var
  BoardsArray: TJSONArray;
  i: Integer;
  BoardObject: TJSONObject;
  BoardId, BoardTitle, BoardSlug, BoardColor: string;
  HasBoards: Boolean;
begin
  SetLanguageFromRequest(aRequest);
  
  with aResponse.Contents do
  begin
    Add(GenerateHtmlHead(Translate('all-boards')));
    Add('<link rel="stylesheet" href="/public/client/components/boards/boardsList.css">');
    Add('<style>');
    Add('  .boards-wrapper {');
    Add('    max-width: 960px;');
    Add('    margin: 0 auto;');
    Add('    padding: 20px;');
    Add('  }');
    Add('  .boards-gallery {');
    Add('    display: flex;');
    Add('    flex-wrap: wrap;');
    Add('    margin: 0 -10px;');
    Add('  }');
    Add('  .board-thumbnail {');
    Add('    width: calc(33.333% - 20px);');
    Add('    margin: 10px;');
    Add('    height: 120px;');
    Add('    border-radius: 3px;');
    Add('    color: white;');
    Add('    padding: 10px;');
    Add('    display: flex;');
    Add('    flex-direction: column;');
    Add('    justify-content: space-between;');
    Add('    text-decoration: none;');
    Add('    position: relative;');
    Add('    overflow: hidden;');
    Add('  }');
    Add('  .board-thumbnail-title {');
    Add('    font-weight: bold;');
    Add('    font-size: 16px;');
    Add('    overflow: hidden;');
    Add('    text-overflow: ellipsis;');
    Add('    white-space: nowrap;');
    Add('  }');
    Add('  .create-board-tile {');
    Add('    background-color: rgba(9,30,66,.04);');
    Add('    color: #172b4d;');
    Add('    display: flex;');
    Add('    align-items: center;');
    Add('    justify-content: center;');
    Add('    cursor: pointer;');
    Add('  }');
    Add('  .create-board-tile:hover {');
    Add('    background-color: rgba(9,30,66,.08);');
    Add('  }');
    Add('  @media (max-width: 768px) {');
    Add('    .board-thumbnail {');
    Add('      width: calc(50% - 20px);');
    Add('    }');
    Add('  }');
    Add('  @media (max-width: 480px) {');
    Add('    .board-thumbnail {');
    Add('      width: calc(100% - 20px);');
    Add('    }');
    Add('  }');
    Add('</style>');
    Add(GenerateLanguageSwitcher);
    
    Add('<div class="boards-wrapper">');
    Add('<h1>' + Translate('all-boards') + '</h1>');
    
    Add('<div class="boards-gallery">');
    
    HasBoards := False;
    
    // Get all boards
    BoardsArray := FindDocuments('boards', '{}');
    if Assigned(BoardsArray) then
    begin
      if BoardsArray.Count > 0 then
        HasBoards := True;
        
      for i := 0 to BoardsArray.Count - 1 do
      begin
        if BoardsArray.Items[i].JSONType = jtObject then
        begin
          BoardObject := TJSONObject(BoardsArray.Items[i]);
          BoardId := BoardObject.Get('_id', '');
          BoardTitle := BoardObject.Get('title', '');
          BoardSlug := BoardObject.Get('slug', '');
          BoardColor := BoardObject.Get('color', 'rgb(0, 121, 191)');
          
          if BoardSlug = '' then
            BoardSlug := StringReplace(LowerCase(BoardTitle), ' ', '-', [rfReplaceAll]);
            
          // Convert color name to RGB if needed
          if BoardColor = 'belize' then
            BoardColor := 'rgb(0, 121, 191)'
          else if BoardColor = 'nephritis' then
            BoardColor := 'rgb(39, 174, 96)'
          else if BoardColor = 'pomegranate' then
            BoardColor := 'rgb(192, 57, 43)'
          else if BoardColor = 'pumpkin' then
            BoardColor := 'rgb(211, 84, 0)'
          else if BoardColor = 'wisteria' then
            BoardColor := 'rgb(142, 68, 173)';
          
          Add('<a href="/b/' + BoardId + '/' + BoardSlug + '" class="board-thumbnail" style="background-color: ' + BoardColor + '">');
          Add('  <div class="board-thumbnail-title">' + BoardTitle + '</div>');
          Add('</a>');
        end;
      end;
      BoardsArray.Free;
    end;
    
    // Add Create Board tile
    Add('<div class="board-thumbnail create-board-tile">');
    Add('  <div class="board-thumbnail-title">+ ' + Translate('create-board') + '</div>');
    Add('</div>');
    
    Add('</div>'); // End boards gallery
    
    if not HasBoards then
    begin
      Add('<p>' + Translate('no-boards') + '</p>');
    end;
    
    // Add create board form (hidden by default)
    Add('<div id="create-board-form" style="display: none; margin-top: 20px;">');
    Add('  <h2>' + Translate('create-board') + '</h2>');
    Add('  <form id="new-board-form">');
    Add('    <div>');
    Add('      <label for="boardTitle">' + Translate('board-title') + '</label>');
    Add('      <input type="text" id="boardTitle" name="title" required>');
    Add('    </div>');
    Add('    <div>');
    Add('      <label for="boardColor">' + Translate('board-color') + '</label>');
    Add('      <select id="boardColor" name="color">');
    Add('        <option value="rgb(0, 121, 191)">Blue</option>');
    Add('        <option value="rgb(39, 174, 96)">Green</option>');
    Add('        <option value="rgb(192, 57, 43)">Red</option>');
    Add('        <option value="rgb(211, 84, 0)">Orange</option>');
    Add('        <option value="rgb(142, 68, 173)">Purple</option>');
    Add('      </select>');
    Add('    </div>');
    Add('    <button type="submit">' + Translate('create') + '</button>');
    Add('    <button type="button" id="cancel-create">' + Translate('cancel') + '</button>');
    Add('  </form>');
    Add('</div>');
    
    // Add script for board creation form
    Add('<script>');
    Add('document.addEventListener("DOMContentLoaded", function() {');
    Add('  const createBoardTile = document.querySelector(".create-board-tile");');
    Add('  const createBoardForm = document.getElementById("create-board-form");');
    Add('  const cancelButton = document.getElementById("cancel-create");');
    Add('  const newBoardForm = document.getElementById("new-board-form");');
    Add('  ');
    Add('  // Show form when clicking on create board tile');
    Add('  createBoardTile.addEventListener("click", function() {');
    Add('    createBoardForm.style.display = "block";');
    Add('  });');
    Add('  ');
    Add('  // Hide form when clicking cancel');
    Add('  cancelButton.addEventListener("click", function() {');
    Add('    createBoardForm.style.display = "none";');
    Add('  });');
    Add('  ');
    Add('  // Handle form submission');
    Add('  newBoardForm.addEventListener("submit", function(e) {');
    Add('    e.preventDefault();');
    Add('    ');
    Add('    const boardTitle = this.elements.title.value;');
    Add('    const boardColor = this.elements.color.value;');
    Add('    ');
    Add('    fetch("/api/boards", {');
    Add('      method: "POST",');
    Add('      headers: {');
    Add('        "Content-Type": "application/json"');
    Add('      },');
    Add('      body: JSON.stringify({');
    Add('        title: boardTitle,');
    Add('        color: boardColor');
    Add('      })');
    Add('    })');
    Add('    .then(response => response.json())');
    Add('    .then(data => {');
    Add('      if (data._id) {');
    Add('        // Redirect to the new board');
    Add('        const slug = boardTitle.toLowerCase().replace(/\s+/g, "-");');
    Add('        window.location.href = "/b/" + data._id + "/" + slug;');
    Add('      }');
    Add('    })');
    Add('    .catch(error => {');
    Add('      console.error("Error:", error);');
    Add('    });');
    Add('  });');
    Add('</script>');
    
    Add('</div>'); // End boards wrapper
    
    Add(GenerateHtmlFooter);
  end;
  
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure userSettingsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  SetLanguageFromRequest(aRequest);
  
  with aResponse.Contents do
  begin
    Add(GenerateHtmlHead(Translate('user-settings')));
    Add(GenerateLanguageSwitcher);
    Add('<h1>' + Translate('user-settings') + '</h1>');
    Add('<p>' + Translate('settings-loading') + '</p>');
    Add(GenerateHtmlFooter);
  end;
  
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure notificationsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  SetLanguageFromRequest(aRequest);
  
  with aResponse.Contents do
  begin
    Add(GenerateHtmlHead(Translate('notifications')));
    Add(GenerateLanguageSwitcher);
    Add('<h1>' + Translate('notifications') + '</h1>');
    Add('<p>' + Translate('notifications-loading') + '</p>');
    Add(GenerateHtmlFooter);
  end;
  
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure publicEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  SetLanguageFromRequest(aRequest);
  
  with aResponse.Contents do
  begin
    Add(GenerateHtmlHead(Translate('public-boards')));
    Add(GenerateLanguageSwitcher);
    Add('<h1>' + Translate('public-boards') + '</h1>');
    Add('<p>' + Translate('public-boards-loading') + '</p>');
    Add(GenerateHtmlFooter);
  end;
  
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure boardEndpoint(aRequest: TRequest; aResponse: TResponse);
var
  BoardId, BoardSlug: string;
  BoardDoc: TJSONObject;
  ListsArray, CardsArray: TJSONArray;
  i, j: Integer;
  ListObject, CardObject: TJSONObject;
  ListId, ListTitle, CardTitle, CardDescription: string;
  CardId, CardColor: string;
  HasBoards: Boolean;
begin
  SetLanguageFromRequest(aRequest);
  
  // Extract board ID from route params if available
  BoardId := '';
  BoardSlug := '';
  // Get route parameters
  BoardId := aRequest.RouteParams['boardId'];
  BoardSlug := aRequest.RouteParams['slug'];
  
  with aResponse.Contents do
  begin
    Add(GenerateHtmlHead(Translate('board')));
    Add('<link rel="stylesheet" href="/public/client/components/boards/boardBody.css">');
    Add('<link rel="stylesheet" href="/public/client/components/lists/list.css">');
    Add('<link rel="stylesheet" href="/public/client/components/cards/cards.css">');
    Add('<style>');
    Add('  .board-wrapper {');
    Add('    display: flex;');
    Add('    flex-direction: column;');
    Add('    height: 100vh;');
    Add('  }');
    Add('  .board-header {');
    Add('    background-color: #026aa7;');
    Add('    color: #fff;');
    Add('    padding: 10px;');
    Add('    display: flex;');
    Add('    justify-content: space-between;');
    Add('    align-items: center;');
    Add('  }');
    Add('  .board-canvas {');
    Add('    flex: 1;');
    Add('    overflow-x: auto;');
    Add('    padding: 10px;');
    Add('    display: flex;');
    Add('    align-items: flex-start;');
    Add('    background-color: #0079bf;');
    Add('  }');
    Add('  .list {');
    Add('    background-color: #ebecf0;');
    Add('    border-radius: 3px;');
    Add('    width: 270px;');
    Add('    margin-right: 10px;');
    Add('    padding: 10px;');
    Add('  }');
    Add('  .list-header {');
    Add('    padding: 10px;');
    Add('    font-weight: bold;');
    Add('  }');
    Add('  .card {');
    Add('    background-color: #fff;');
    Add('    border-radius: 3px;');
    Add('    box-shadow: 0 1px 0 rgba(9,30,66,.25);');
    Add('    margin-bottom: 8px;');
    Add('    padding: 8px;');
    Add('    cursor: pointer;');
    Add('  }');
    Add('  .add-list {');
    Add('    background-color: rgba(255, 255, 255, 0.3);');
    Add('    border-radius: 3px;');
    Add('    width: 270px;');
    Add('    padding: 10px;');
    Add('    color: #fff;');
    Add('    cursor: pointer;');
    Add('  }');
    Add('  .add-card {');
    Add('    color: #5e6c84;');
    Add('    padding: 8px;');
    Add('    cursor: pointer;');
    Add('  }');
    Add('  .add-card:hover {');
    Add('    background-color: rgba(9,30,66,0.08);');
    Add('    border-radius: 3px;');
    Add('  }');
    Add('  .card-color-tag {');
    Add('    height: 6px;');
    Add('    border-top-left-radius: 2px;');
    Add('    border-top-right-radius: 2px;');
    Add('    margin-bottom: 6px;');
    Add('  }');
    Add('</style>');
    Add(GenerateLanguageSwitcher);
    
    Add('<div class="board-wrapper">');
    
    // Board header
    Add('<div class="board-header">');
    
    if BoardId <> '' then
    begin
      BoardDoc := FindOneDocument('boards', '{ "_id": ObjectId("' + BoardId + '") }');
      if Assigned(BoardDoc) then
      begin
        Add('<h1>' + BoardDoc.Get('title', Translate('board')) + '</h1>');
      end
      else
      begin
        Add('<h1>' + Translate('board') + '</h1>');
      end;
    end
    else
    begin
      Add('<h1>' + Translate('board') + '</h1>');
    end;
    
    Add('<div class="board-header-right">');
    Add('  <a href="/allboards" class="btn">' + Translate('all-boards') + '</a>');
    Add('</div>');
    Add('</div>'); // End board header
    
    // Board canvas with lists
    Add('<div class="board-canvas">');
    
    HasBoards := False;
    
    if BoardId <> '' then
    begin
      // Get lists for this board
      ListsArray := FindDocuments('lists', '{ "boardId": "' + BoardId + '" }');
      if Assigned(ListsArray) then
      begin
        HasBoards := True;
        for i := 0 to ListsArray.Count - 1 do
        begin
          if ListsArray.Items[i].JSONType = jtObject then
          begin
            ListObject := TJSONObject(ListsArray.Items[i]);
            ListId := ListObject.Get('_id', '');
            ListTitle := ListObject.Get('title', '');
            
            Add('<div class="list">');
            Add('  <div class="list-header">' + ListTitle + '</div>');
            
            // Get cards for this list
            if ListId <> '' then
            begin
              CardsArray := FindDocuments('cards', '{ "listId": "' + ListId + '" }');
              if Assigned(CardsArray) then
              begin
                for j := 0 to CardsArray.Count - 1 do
                begin
                  if CardsArray.Items[j].JSONType = jtObject then
                  begin
                    CardObject := TJSONObject(CardsArray.Items[j]);
                    CardId := CardObject.Get('_id', '');
                    CardTitle := CardObject.Get('title', '');
                    CardDescription := CardObject.Get('description', '');
                    CardColor := CardObject.Get('color', '');
                    
                    Add('<div class="card" onclick="window.location.href=''/b/' + BoardId + '/' + BoardSlug + '/' + CardId + '''">');
                    if CardColor <> '' then
                      Add('  <div class="card-color-tag" style="background-color:' + CardColor + '"></div>');
                    Add('  <div class="card-title">' + CardTitle + '</div>');
                    Add('</div>');
                  end;
                end;
                CardsArray.Free;
              end;
            end;
            
            Add('  <div class="add-card">+ ' + Translate('add-card') + '</div>');
            Add('</div>'); // End list
          end;
        end;
        ListsArray.Free;
      end;
      
      // Add new list button
      Add('<div class="add-list">+ ' + Translate('add-list') + '</div>');
      
      if Assigned(BoardDoc) then
        BoardDoc.Free;
    end;
    
    if not HasBoards then
    begin
      Add('<p>' + Translate('board-loading') + '</p>');
    end;
    
    Add('</div>'); // End board canvas
    Add('</div>'); // End board wrapper
    
    // Add client-side script for interactivity
    Add('<script>');
    Add('document.addEventListener("DOMContentLoaded", function() {');
    Add('  // Add interactivity here');
    Add('});');
    Add('</script>');
    
    Add(GenerateHtmlFooter);
  end;
  
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure cardEndpoint(aRequest: TRequest; aResponse: TResponse);
var
  BoardId, CardId: string;
  BoardDoc, CardDoc: TJSONObject;
  CommentsArray, ChecklistsArray, AttachmentsArray: TJSONArray;
  i, j: Integer;
  CommentObject, ChecklistObject, ChecklistItemObject, AttachmentObject: TJSONObject;
  UserObject: TJSONObject;
  CommentText, AuthorId, AuthorName, CreatedAt: string;
  ChecklistTitle, ChecklistItemTitle: string;
  IsChecked: Boolean;
  AttachmentName, AttachmentUrl: string;
  CardTitle, CardDescription: string;
  HasCard: Boolean;
begin
  SetLanguageFromRequest(aRequest);
  
  // Extract board ID and card ID from route params if available
  BoardId := '';
  CardId := '';
  // Get route parameters
  BoardId := aRequest.RouteParams['boardId'];
  CardId := aRequest.RouteParams['cardId'];
  
  with aResponse.Contents do
  begin
    Add(GenerateHtmlHead(Translate('card')));
    Add('<link rel="stylesheet" href="/public/client/components/cards/cardDetails.css">');
    Add('<style>');
    Add('  .card-detail-wrapper {');
    Add('    display: flex;');
    Add('    flex-direction: column;');
    Add('    max-width: 800px;');
    Add('    margin: 20px auto;');
    Add('    background-color: #f4f5f7;');
    Add('    border-radius: 3px;');
    Add('    padding: 20px;');
    Add('  }');
    Add('  .card-detail-header {');
    Add('    display: flex;');
    Add('    justify-content: space-between;');
    Add('    align-items: center;');
    Add('    margin-bottom: 20px;');
    Add('  }');
    Add('  .card-detail-data {');
    Add('    display: flex;');
    Add('  }');
    Add('  .card-detail-main {');
    Add('    flex: 3;');
    Add('    padding-right: 20px;');
    Add('  }');
    Add('  .card-detail-sidebar {');
    Add('    flex: 1;');
    Add('  }');
    Add('  .card-section {');
    Add('    margin-bottom: 20px;');
    Add('    background-color: #fff;');
    Add('    border-radius: 3px;');
    Add('    padding: 15px;');
    Add('  }');
    Add('  .card-description {');
    Add('    white-space: pre-wrap;');
    Add('  }');
    Add('  .comment {');
    Add('    margin-bottom: 10px;');
    Add('    padding: 10px;');
    Add('    background-color: #fff;');
    Add('    border-radius: 3px;');
    Add('  }');
    Add('  .comment-author {');
    Add('    font-weight: bold;');
    Add('  }');
    Add('  .comment-date {');
    Add('    color: #5e6c84;');
    Add('    font-size: 0.8em;');
    Add('  }');
    Add('  .checklist-item {');
    Add('    display: flex;');
    Add('    align-items: center;');
    Add('    margin-bottom: 5px;');
    Add('  }');
    Add('  .attachment {');
    Add('    display: flex;');
    Add('    align-items: center;');
    Add('    padding: 8px;');
    Add('    margin-bottom: 5px;');
    Add('    background-color: #f7f7f7;');
    Add('    border-radius: 3px;');
    Add('  }');
    Add('  .attachment-icon {');
    Add('    margin-right: 10px;');
    Add('  }');
    Add('</style>');
    Add(GenerateLanguageSwitcher);
    
    HasCard := False;
    
    // Card details
    if (BoardId <> '') and (CardId <> '') then
    begin
      CardDoc := FindOneDocument('cards', '{ "_id": ObjectId("' + CardId + '") }');
      BoardDoc := FindOneDocument('boards', '{ "_id": ObjectId("' + BoardId + '") }');
      
      if Assigned(CardDoc) and Assigned(BoardDoc) then
      begin
        HasCard := True;
        CardTitle := CardDoc.Get('title', Translate('card'));
        CardDescription := CardDoc.Get('description', '');
        
        Add('<div class="card-detail-wrapper">');
        
        // Card header
        Add('<div class="card-detail-header">');
        Add('  <h1>' + CardTitle + '</h1>');
        Add('  <a href="/b/' + BoardId + '/' + BoardDoc.Get('slug', '') + '" class="btn">' + 
             Translate('back-to-board') + ': ' + BoardDoc.Get('title', '') + '</a>');
        Add('</div>');
        
        // Card content - main section and sidebar
        Add('<div class="card-detail-data">');
        
        // Main section - Description, Comments, Checklists
        Add('<div class="card-detail-main">');
        
        // Description
        Add('<div class="card-section">');
        Add('  <h3>' + Translate('description') + '</h3>');
        if CardDescription <> '' then
          Add('  <div class="card-description">' + CardDescription + '</div>')
        else
          Add('  <p><em>' + Translate('no-description') + '</em></p>');
        Add('</div>');
        
        // Comments
        Add('<div class="card-section">');
        Add('  <h3>' + Translate('comments') + '</h3>');
        
        CommentsArray := FindDocuments('comments', '{ "cardId": "' + CardId + '" }');
        if Assigned(CommentsArray) and (CommentsArray.Count > 0) then
        begin
          for i := 0 to CommentsArray.Count - 1 do
          begin
            if CommentsArray.Items[i].JSONType = jtObject then
            begin
              CommentObject := TJSONObject(CommentsArray.Items[i]);
              CommentText := CommentObject.Get('text', '');
              AuthorId := CommentObject.Get('authorId', '');
              CreatedAt := CommentObject.Get('createdAt', '');
              AuthorName := 'Anonymous';
              
              // Get author name
              if AuthorId <> '' then
              begin
                UserObject := FindOneDocument('users', '{ "_id": ObjectId("' + AuthorId + '") }');
                if Assigned(UserObject) then
                begin
                  AuthorName := UserObject.Get('username', 'Anonymous');
                  UserObject.Free;
                end;
              end;
              
              Add('<div class="comment">');
              Add('  <div class="comment-author">' + AuthorName + '</div>');
              Add('  <div class="comment-date">' + CreatedAt + '</div>');
              Add('  <div class="comment-text">' + CommentText + '</div>');
              Add('</div>');
            end;
          end;
          CommentsArray.Free;
        end
        else
        begin
          Add('<p><em>' + Translate('no-comments') + '</em></p>');
        end;
        
        // Add comment form
        Add('<div class="add-comment">');
        Add('  <form id="comment-form">');
        Add('    <textarea name="comment" placeholder="' + Translate('add-comment') + '"></textarea>');
        Add('    <button type="submit">' + Translate('save') + '</button>');
        Add('  </form>');
        Add('</div>');
        Add('</div>');
        
        // Checklists
        Add('<div class="card-section">');
        Add('  <h3>' + Translate('checklists') + '</h3>');
        
        ChecklistsArray := FindDocuments('checklists', '{ "cardId": "' + CardId + '" }');
        if Assigned(ChecklistsArray) and (ChecklistsArray.Count > 0) then
        begin
          for i := 0 to ChecklistsArray.Count - 1 do
          begin
            if ChecklistsArray.Items[i].JSONType = jtObject then
            begin
              ChecklistObject := TJSONObject(ChecklistsArray.Items[i]);
              ChecklistTitle := ChecklistObject.Get('title', 'Checklist');
              
              Add('<div class="checklist">');
              Add('  <h4>' + ChecklistTitle + '</h4>');
              
              // Checklist items
              if ChecklistObject.Find('items') <> nil then
              begin
                for j := 0 to TJSONArray(ChecklistObject.Find('items')).Count - 1 do
                begin
                  if TJSONArray(ChecklistObject.Find('items')).Items[j].JSONType = jtObject then
                  begin
                    ChecklistItemObject := TJSONObject(TJSONArray(ChecklistObject.Find('items')).Items[j]);
                    ChecklistItemTitle := ChecklistItemObject.Get('title', '');
                    IsChecked := ChecklistItemObject.Get('isChecked', False);
                    
                    Add('<div class="checklist-item">');
                    Add('  <input type="checkbox" ' + IfThen(IsChecked, 'checked', '') + '>');
                    Add('  <span>' + ChecklistItemTitle + '</span>');
                    Add('</div>');
                  end;
                end;
              end;
              
              Add('</div>');
            end;
          end;
          ChecklistsArray.Free;
        end
        else
        begin
          Add('<p><em>' + Translate('no-checklists') + '</em></p>');
        end;
        Add('</div>');
        
        Add('</div>'); // End main section
        
        // Sidebar - Members, Labels, Attachments, Actions
        Add('<div class="card-detail-sidebar">');
        
        // Actions
        Add('<div class="card-section">');
        Add('  <h3>' + Translate('actions') + '</h3>');
        Add('  <ul>');
        Add('    <li><a href="#" class="action-link">' + Translate('move-card') + '</a></li>');
        Add('    <li><a href="#" class="action-link">' + Translate('archive') + '</a></li>');
        Add('    <li><a href="#" class="action-link">' + Translate('delete') + '</a></li>');
        Add('  </ul>');
        Add('</div>');
        
        // Attachments
        Add('<div class="card-section">');
        Add('  <h3>' + Translate('attachments') + '</h3>');
        
        AttachmentsArray := FindDocuments('attachments', '{ "cardId": "' + CardId + '" }');
        if Assigned(AttachmentsArray) and (AttachmentsArray.Count > 0) then
        begin
          for i := 0 to AttachmentsArray.Count - 1 do
          begin
            if AttachmentsArray.Items[i].JSONType = jtObject then
            begin
              AttachmentObject := TJSONObject(AttachmentsArray.Items[i]);
              AttachmentName := AttachmentObject.Get('name', '');
              AttachmentUrl := AttachmentObject.Get('url', '');
              
              Add('<div class="attachment">');
              Add('  <div class="attachment-icon">📎</div>');
              Add('  <a href="' + AttachmentUrl + '" target="_blank">' + AttachmentName + '</a>');
              Add('</div>');
            end;
          end;
          AttachmentsArray.Free;
        end
        else
        begin
          Add('<p><em>' + Translate('no-attachments') + '</em></p>');
        end;
        
        // Add attachment form
        Add('<form action="/api/boards/' + BoardId + '/cards/' + CardId + '/attachments" method="post" enctype="multipart/form-data">');
        Add('  <input type="file" name="attachment">');
        Add('  <button type="submit">' + Translate('add-attachment') + '</button>');
        Add('</form>');
        Add('</div>');
        
        Add('</div>'); // End sidebar
        
        Add('</div>'); // End card detail data
        Add('</div>'); // End card detail wrapper
        
        // Add client-side script for interactivity
        Add('<script>');
        Add('document.addEventListener("DOMContentLoaded", function() {');
        Add('  // Handle comment form submission');
        Add('  document.getElementById("comment-form").addEventListener("submit", function(e) {');
        Add('    e.preventDefault();');
        Add('    var commentText = this.elements.comment.value;');
        Add('    if (commentText) {');
        Add('      fetch("/api/boards/' + BoardId + '/cards/' + CardId + '/comments", {');
        Add('        method: "POST",');
        Add('        headers: {');
        Add('          "Content-Type": "application/json"');
        Add('        },');
        Add('        body: JSON.stringify({ text: commentText })');
        Add('      })');
        Add('      .then(response => response.json())');
        Add('      .then(data => {');
        Add('        if (data.status === "success") {');
        Add('          window.location.reload();');
        Add('        }');
        Add('      });');
        Add('    }');
        Add('  });');
        Add('</script>');
        
        CardDoc.Free;
        BoardDoc.Free;
      end;
    end;
    
    if not HasCard then
    begin
      Add('<div style="text-align: center; margin-top: 50px;">');
      Add('  <h1>' + Translate('card') + '</h1>');
      Add('  <p>' + Translate('card-loading') + '</p>');
      Add('  <a href="/allboards" class="btn">' + Translate('all-boards') + '</a>');
      Add('</div>');
    end;
    
    Add(GenerateHtmlFooter);
  end;
  
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure shortCutsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  SetLanguageFromRequest(aRequest);
  
  with aResponse.Contents do
  begin
    Add(GenerateHtmlHead(Translate('shortcuts')));
    Add(GenerateLanguageSwitcher);
    Add('<h1>' + Translate('shortcuts') + '</h1>');
    Add('<p>' + Translate('shortcuts-loading') + '</p>');
    Add(GenerateHtmlFooter);
  end;
  
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure templatesEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  SetLanguageFromRequest(aRequest);
  
  with aResponse.Contents do
  begin
    Add(GenerateHtmlHead(Translate('templates')));
    Add(GenerateLanguageSwitcher);
    Add('<h1>' + Translate('templates') + '</h1>');
    Add('<p>' + Translate('templates-loading') + '</p>');
    Add(GenerateHtmlFooter);
  end;
  
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure myCardsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  SetLanguageFromRequest(aRequest);
  
  with aResponse.Contents do
  begin
    Add(GenerateHtmlHead(Translate('my-cards')));
    Add(GenerateLanguageSwitcher);
    Add('<h1>' + Translate('my-cards') + '</h1>');
    Add('<p>' + Translate('my-cards-loading') + '</p>');
    Add(GenerateHtmlFooter);
  end;
  
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure dueCardsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  SetLanguageFromRequest(aRequest);
  
  with aResponse.Contents do
  begin
    Add(GenerateHtmlHead(Translate('due-cards')));
    Add(GenerateLanguageSwitcher);
    Add('<h1>' + Translate('due-cards') + '</h1>');
    Add('<p>' + Translate('due-cards-loading') + '</p>');
    Add(GenerateHtmlFooter);
  end;
  
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure globalSearchEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  SetLanguageFromRequest(aRequest);
  
  with aResponse.Contents do
  begin
    Add(GenerateHtmlHead(Translate('global-search')));
    Add(GenerateLanguageSwitcher);
    Add('<h1>' + Translate('global-search') + '</h1>');
    Add('<p>' + Translate('search-loading') + '</p>');
    Add(GenerateHtmlFooter);
  end;
  
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure brokenCardsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  SetLanguageFromRequest(aRequest);
  
  with aResponse.Contents do
  begin
    Add(GenerateHtmlHead(Translate('broken-cards')));
    Add(GenerateLanguageSwitcher);
    Add('<h1>' + Translate('broken-cards') + '</h1>');
    Add('<p>' + Translate('broken-cards-loading') + '</p>');
    Add(GenerateHtmlFooter);
  end;
  
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure importEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  SetLanguageFromRequest(aRequest);
  
  with aResponse.Contents do
  begin
    Add(GenerateHtmlHead(Translate('import')));
    Add(GenerateLanguageSwitcher);
    Add('<h1>' + Translate('import') + '</h1>');
    Add('<p>' + Translate('import-loading') + '</p>');
    Add(GenerateHtmlFooter);
  end;
  
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure adminSettingEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  SetLanguageFromRequest(aRequest);
  
  with aResponse.Contents do
  begin
    Add(GenerateHtmlHead(Translate('admin-settings')));
    Add(GenerateLanguageSwitcher);
    Add('<h1>' + Translate('admin-settings') + '</h1>');
    Add('<p>' + Translate('admin-settings-loading') + '</p>');
    Add(GenerateHtmlFooter);
  end;
  
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure informationEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  SetLanguageFromRequest(aRequest);
  
  with aResponse.Contents do
  begin
    Add(GenerateHtmlHead(Translate('information')));
    Add(GenerateLanguageSwitcher);
    Add('<h1>' + Translate('information') + '</h1>');
    Add('<p>' + Translate('information-loading') + '</p>');
    Add(GenerateHtmlFooter);
  end;
  
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure peopleEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  SetLanguageFromRequest(aRequest);
  
  with aResponse.Contents do
  begin
    Add(GenerateHtmlHead(Translate('people')));
    Add(GenerateLanguageSwitcher);
    Add('<h1>' + Translate('people') + '</h1>');
    Add('<p>' + Translate('people-loading') + '</p>');
    Add(GenerateHtmlFooter);
  end;
  
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure adminReportsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  SetLanguageFromRequest(aRequest);
  
  with aResponse.Contents do
  begin
    Add(GenerateHtmlHead(Translate('admin-reports')));
    Add(GenerateLanguageSwitcher);
    Add('<h1>' + Translate('admin-reports') + '</h1>');
    Add('<p>' + Translate('admin-reports-loading') + '</p>');
    Add(GenerateHtmlFooter);
  end;
  
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure uploadEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  SetLanguageFromRequest(aRequest);
  
  if aRequest.Method = 'POST' then
  begin
    // Handle file upload (to be implemented)
    aResponse.Code := 200;
    aResponse.ContentType := 'application/json';
    aResponse.Content := '{"status": "success", "message": "' + Translate('upload-success') + '"}';
    aResponse.ContentLength := Length(aResponse.Content);
    aResponse.SendContent;
  end
  else
  begin
    // Display upload form
    with aResponse.Contents do
    begin
      Add(GenerateHtmlHead(Translate('upload')));
      Add(GenerateLanguageSwitcher);
      Add('<h1>' + Translate('upload') + '</h1>');
      Add('<form action="/upload" method="post" enctype="multipart/form-data">');
      Add('  <div>');
      Add('    <label for="file">' + Translate('select-file') + '</label>');
      Add('    <input type="file" id="file" name="file" required>');
      Add('  </div>');
      Add('  <div>');
      Add('    <button type="submit">' + Translate('upload') + '</button>');
      Add('  </div>');
      Add('</form>');
      Add(GenerateHtmlFooter);
    end;
    
    aResponse.Code := 200;
    aResponse.ContentType := 'text/html';
    aResponse.ContentLength := Length(aResponse.Content);
    aResponse.SendContent;
  end;
end;

procedure jsonEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  // Example JSON endpoint
  aResponse.Code := 200;
  aResponse.ContentType := 'application/json';
  aResponse.Content := '{"status": "success", "message": "' + Translate('api-success') + '"}';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure screenInfoEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  // Simple endpoint to return screen information
  aResponse.Code := 200;
  aResponse.ContentType := 'application/json';
  aResponse.Content := '{"width": 1920, "height": 1080}';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure serveStaticFileEndpoint(aRequest: TRequest; aResponse: TResponse);
var
  FilePath: string;
  FileStream: TFileStream;
  ContentType: string;
  FileExt: string;
begin
  // Get the file path from the request path
  FilePath := 'public' + aRequest.PathInfo;
  
  // Remove any query parameters
  if Pos('?', FilePath) > 0 then
    FilePath := Copy(FilePath, 1, Pos('?', FilePath) - 1);
  
  // Default index file
  if (FilePath = 'public/') or (FilePath = 'public') then
    FilePath := 'public/index.html';
  
  // Check if the file exists
  if FileExists(FilePath) then
  begin
    try
      // Determine content type based on file extension
      FileExt := LowerCase(ExtractFileExt(FilePath));
      
      if FileExt = '.html' then
        ContentType := 'text/html'
      else if FileExt = '.css' then
        ContentType := 'text/css'
      else if FileExt = '.js' then
        ContentType := 'application/javascript'
      else if FileExt = '.png' then
        ContentType := 'image/png'
      else if (FileExt = '.jpg') or (FileExt = '.jpeg') then
        ContentType := 'image/jpeg'
      else if FileExt = '.gif' then
        ContentType := 'image/gif'
      else if FileExt = '.svg' then
        ContentType := 'image/svg+xml'
      else if FileExt = '.json' then
        ContentType := 'application/json'
      else
        ContentType := 'application/octet-stream';
      
      // Open the file
      FileStream := TFileStream.Create(FilePath, fmOpenRead);
      try
        // Set response headers
        aResponse.ContentType := ContentType;
        aResponse.ContentStream := FileStream;
        aResponse.ContentLength := FileStream.Size;
        aResponse.Code := 200;
        aResponse.SendContent;
      except
        FileStream.Free;
        aResponse.Code := 500; // Internal Server Error
        aResponse.Content := 'Error reading file';
        aResponse.ContentType := 'text/plain';
        aResponse.ContentLength := Length(aResponse.Content);
        aResponse.SendContent;
      end;
    except
      aResponse.Code := 500; // Internal Server Error
      aResponse.Content := 'Error processing file';
      aResponse.ContentType := 'text/plain';
      aResponse.ContentLength := Length(aResponse.Content);
      aResponse.SendContent;
    end;
  end
  else
  begin
    // File not found
    aResponse.Code := 404;
    aResponse.Content := 'File not found: ' + FilePath;
    aResponse.ContentType := 'text/plain';
    aResponse.ContentLength := Length(aResponse.Content);
    aResponse.SendContent;
  end;
end;

procedure catchallEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  // Catch-all endpoint for unhandled routes
  aResponse.Code := 404;
  aResponse.ContentType := 'text/html';
  
  SetLanguageFromRequest(aRequest);
  
  with aResponse.Contents do
  begin
    Add(GenerateHtmlHead(Translate('page-not-found')));
    Add(GenerateLanguageSwitcher);
    Add('<h1>' + Translate('page-not-found') + '</h1>');
    Add('<p>' + Translate('page-not-found-message') + '</p>');
    Add('<p><a href="/">' + Translate('go-home') + '</a></p>');
    Add(GenerateHtmlFooter);
  end;
  
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Stub implementation for apiProfileEndpoint
procedure apiProfileEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Code := 501; // Not implemented
  aResponse.Content := 'Not implemented';
  aResponse.SendContent;
end;

// Main program block
begin
  Application.Port:=5500;
  
  // Initialize language system
  InitLanguages;
  
  // Load default English translations
  if not LoadTranslations('en') then
    Writeln('Warning: Failed to load default English translations');
  
  // Main page routes
  HTTPRouter.RegisterRoute('/', rmGet, @allPagesEndpoint);
  HTTPRouter.RegisterRoute('/sign-in', rmGet, @loginEndpoint);
  HTTPRouter.RegisterRoute('/sign-up', rmGet, @registerEndpoint);
  HTTPRouter.RegisterRoute('/forgot-password', rmGet, @forgotPasswordEndpoint);
  HTTPRouter.RegisterRoute('/allboards', rmGet, @allBoardsEndpoint);
  HTTPRouter.RegisterRoute('/usersettings', rmGet, @userSettingsEndpoint);
  HTTPRouter.RegisterRoute('/notifications', rmGet, @notificationsEndpoint);
  HTTPRouter.RegisterRoute('/public', rmGet, @publicEndpoint);
  HTTPRouter.RegisterRoute('/board', rmGet, @boardEndpoint);
  // Board with specific ID
  HTTPRouter.RegisterRoute('/b/:boardId', rmGet, @boardEndpoint);
  HTTPRouter.RegisterRoute('/b/:boardId/:slug', rmGet, @boardEndpoint);
  HTTPRouter.RegisterRoute('/card', rmGet, @cardEndpoint);
  // Card with specific ID
  HTTPRouter.RegisterRoute('/b/:boardId/:slug/:cardId', rmGet, @cardEndpoint);
  HTTPRouter.RegisterRoute('/shortcuts', rmGet, @shortCutsEndpoint);
  HTTPRouter.RegisterRoute('/templates', rmGet, @templatesEndpoint);
  HTTPRouter.RegisterRoute('/my-cards', rmGet, @myCardsEndpoint);
  HTTPRouter.RegisterRoute('/due-cards', rmGet, @dueCardsEndpoint);
  HTTPRouter.RegisterRoute('/global-search', rmGet, @globalSearchEndpoint);
  HTTPRouter.RegisterRoute('/broken-cards', rmGet, @brokenCardsEndpoint);
  HTTPRouter.RegisterRoute('/import', rmGet, @importEndpoint);
  HTTPRouter.RegisterRoute('/setting', rmGet, @adminSettingEndpoint);
  HTTPRouter.RegisterRoute('/information', rmGet, @informationEndpoint);
  HTTPRouter.RegisterRoute('/people', rmGet, @peopleEndpoint);
  HTTPRouter.RegisterRoute('/admin-reports', rmGet, @adminReportsEndpoint);
  HTTPRouter.RegisterRoute('/upload', rmGet, @uploadEndpoint);
  HTTPRouter.RegisterRoute('/upload', rmPost, @uploadEndpoint);
  HTTPRouter.RegisterRoute('/translation', rmGet, @translationEndpoint);
  HTTPRouter.RegisterRoute('/json', rmGet, @jsonEndpoint);
  HTTPRouter.RegisterRoute('/screeninfo', @screenInfoEndpoint);
  
  // API endpoints
  HTTPRouter.RegisterRoute('/api/users/login', rmPost, @apiUsersLoginEndpoint);
  HTTPRouter.RegisterRoute('/api/users/register', rmPost, @apiUsersRegisterEndpoint);
  HTTPRouter.RegisterRoute('/api/users/forgot', rmPost, @apiUsersForgotEndpoint);
  HTTPRouter.RegisterRoute('/api/users/me', rmGet, @apiUsersMeEndpoint);
  HTTPRouter.RegisterRoute('/api/users/profile', rmGet, @apiUsersProfileEndpoint);
  HTTPRouter.RegisterRoute('/api/users/profile', rmPut, @apiUsersProfileEndpoint);
  HTTPRouter.RegisterRoute('/api/boards', rmGet, @apiBoardsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards', rmPost, @apiBoardsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId', rmGet, @apiBoardsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId', rmPut, @apiBoardsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId', rmDelete, @apiBoardsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/lists', rmGet, @apiListsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/lists', rmPost, @apiListsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/lists/:listId', rmGet, @apiListsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/lists/:listId', rmPut, @apiListsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/lists/:listId', rmDelete, @apiListsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/cards', rmGet, @apiCardsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/cards', rmPost, @apiCardsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/cards/:cardId', rmGet, @apiCardsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/cards/:cardId', rmPut, @apiCardsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/cards/:cardId', rmDelete, @apiCardsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/cards/:cardId/comments', rmGet, @apiCommentsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/cards/:cardId/comments', rmPost, @apiCommentsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/cards/:cardId/comments/:commentId', rmGet, @apiCommentsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/cards/:cardId/comments/:commentId', rmPut, @apiCommentsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/cards/:cardId/comments/:commentId', rmDelete, @apiCommentsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/cards/:cardId/attachments', rmGet, @apiAttachmentsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/cards/:cardId/attachments', rmPost, @apiAttachmentsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/cards/:cardId/attachments/:attachmentId', rmGet, @apiAttachmentsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/cards/:cardId/attachments/:attachmentId', rmDelete, @apiAttachmentsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/cards/:cardId/checklists', rmGet, @apiChecklistsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/cards/:cardId/checklists', rmPost, @apiChecklistsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/cards/:cardId/checklists/:checklistId', rmGet, @apiChecklistsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/cards/:cardId/checklists/:checklistId', rmPut, @apiChecklistsEndpoint);
  HTTPRouter.RegisterRoute('/api/boards/:boardId/cards/:cardId/checklists/:checklistId', rmDelete, @apiChecklistsEndpoint);
  HTTPRouter.RegisterRoute('/api/activities', rmGet, @apiActivitiesEndpoint);
  HTTPRouter.RegisterRoute('/api/settings', rmGet, @apiSettingsEndpoint);
  HTTPRouter.RegisterRoute('/api/settings', rmPut, @apiSettingsEndpoint);
  
  // Add static file server for files in the public directory
  // This route should come before the catchall
  HTTPRouter.RegisterRoute('/*', rmGet, @serveStaticFileEndpoint);
  
  // Fallback for any remaining requests
  HTTPRouter.RegisterRoute('/catchall', rmAll, @catchallEndpoint, true);
  
  // Create public directory if it doesn't exist
  if not DirectoryExists('public') then
    CreateDir('public');
    
  Application.Threaded:=false;
  Application.Initialize;
  Writeln('Server is ready at localhost:' + IntToStr(Application.Port));
  Writeln('Static files are served from the public/ directory');
  
  // Initialize languages
  InitLanguages;
  
  // Load default translations
  LoadTranslations(CurrentLanguage);
  
  Application.Run;
end.
