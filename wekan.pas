program wekan;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  SysUtils, fphttpapp, HTTPDefs, httproute, Classes; // Add Classes unit

var
  MongoUrlValue: string;

function WebBrowserName(const UserAgent: string): String;
// Ubuntu Chrome: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/137.0.0.0 Safari/537.36
// MorphOS IBrowse 3.0a: IBrowse/3.0 (Amiga; MorphOS 3.19; Build 30.8 68K)
// NetSurf 3.11 (28th December 2023): Mozilla/5.0 (X11; Linux) NetSurf/3.11
// FreeDOS Dillo: Mozilla/4.0 (compatible; Dillo 3.0)
// Ubuntu Desktop Dillo:  Dillo/3.0.5
// iPhone: Mozilla/5.0 (iPhone; CPU iPhone OS 19_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/19.0 Mobile/15E148 Safari/604.1
// Ubuntu Touch Morph Browser: Mozilla/5.0 (Linux; Ubuntu 24.04 like Android 9) AppleWebKit/537.36 Chrome/87.0.4280.144 Mobile Safari/537.36
// Ubuntu Desktop Morph Browser: Mozilla/5.0 (Linux; Ubuntu 25.04) AppleWebKit/537.36 Chrome/87.0.4280.144 Safari/537.36
var
  BrowserName: String;
begin
  BrowserName := '';
  if Pos('IBrowse', UserAgent) > 0 then
  begin
    BrowserName := 'IBrowse';
  end
  else if Pos('NetSurf', UserAgent) > 0 then
  begin
    BrowserName := 'NetSurf';
  end
  else if Pos('Dillo', UserAgent) > 0 then
  begin
    if Pos('Mozilla', UserAgent) > 0 then
    begin
      BrowserName := 'DilloFreeDOS';
    end
    else
    begin
      BrowserName := 'DilloDesktop';
    end
  end
  else if Pos('iPhone', UserAgent) > 0 then
  begin
    BrowserName := 'iPhoneSafari';
  end
  else if Pos('Ubuntu', UserAgent) > 0 then
  begin
    if Pos('Android', UserAgent) > 0 then
    begin
      BrowserName := 'UbuntuTouchMorph';
    end
    else
    begin
      BrowserName := 'UbuntuDesktopMorph';
    end
  end
  else if Pos('Chrome', UserAgent) > 0 then
  begin
    BrowserName := 'Chrome/Brave';
  end
  else
  begin
    BrowserName := 'Unknown';
  end;
  Result := BrowserName;
end;


// Example: Add(BoardIcon('At touchscreen', 1, 'white', 'blue'));
function BoardIcon(BoardTitle: String; TabIndex: Integer; Color: String; BackgroundColor: String): String;
begin
  Result := '<table bgcolor="' + BackgroundColor + '" tabindex="' + IntToStr(TabIndex) + '" style="border-collapse: collapse;" width="200"' +
            ' height="80" border="0" padding="0" spacing="0" id="drag-' + IntToStr(TabIndex) + '" class="draggable" border-collapse="collapse">' + LineEnding +
          '  <tbody>' + LineEnding +
          '    <tr border="0" padding="0" spacing="0">' + LineEnding +
          '      <td width="20" height="20"></td>' + LineEnding +
          '      <td width="160" height="40" valign="middle" align="top"><font size="1" color="' + Color + '" face="arial"><b>' + BoardTitle + '</b><p></p></font></td>' + LineEnding +
          '      <td width="20" height="20"></td>' + LineEnding +
          '    </tr>' + LineEnding +
          '    <tr border="0" padding="0" spacing="0">' + LineEnding +
          '      <td width="20" height="20"></td>' + LineEnding +
          '      <td width="160" height="20"></td>' + LineEnding +
          '      <td width="20" height="20"></td>' + LineEnding +
          '    </tr>' + LineEnding +
          '    <tr border="0" padding="0" spacing="0">' + LineEnding +
          '      <td width="20" height="20"></td>' + LineEnding +
          '      <td width="160" height="20"></td>' + LineEnding +
          '      <td width="20" height="20"></td>' + LineEnding +
          '    </tr>' + LineEnding +
          '  </tbody>' + LineEnding +
          '</table>' + LineEnding +
          '<br>' + LineEnding;
end;

function DrawLine(X1: Integer; Y1: Integer; X2: Integer; Y2: Integer; Width: Integer; Height: Integer; Color: String; StrokeWidth: String): String;
begin
  // Color can be for example red or rgb(255,0,0) depending how it's defined
  // Strokewidth usually is 2
  // Example: DrawLine(0,0,270,150,500,210,'red','2');
  Result := '<div class="lines">' + LineEnding +
            '  <svg width="' + IntToStr(Width) + '" height="' + IntToStr(Height) + '">' + LineEnding +
            '    <line x1="' + IntToStr(X1) + '" y1="' + IntToStr(Y1) + '" x2="' + IntToStr(X2) + '" y2="' + IntToStr(Y2) + '" style="stroke:' + Color + ';stroke-width:' + StrokeWidth + '" />' + LineEnding +
            '  </svg>' + LineEnding +
            '  <v:group coordorigin="' + IntToStr(X1) + ' ' + IntToStr(Y1) + '" coordsize="' + IntToStr(Width) + ' ' + IntToStr(Height) + '" style="width:' + IntToStr(Width) + 'px;height:' + IntToStr(Height) + 'px;">' + LineEnding +
            '    <v:line from="' + IntToStr(X1) + ',' + IntToStr(Y1) + '" to="' + IntToStr(X2) + ',' + IntToStr(Y2) + '" strokecolor="' + Color + '" strokeweight="' + StrokeWidth + 'pt" />' + LineEnding +
            '  </v:group>' + LineEnding +
            '</div>' + LineEnding;
end;

procedure catchallEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content:='This endpoint is not available';
  aResponse.Code:=404;
  aResponse.ContentType:='text/plain';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure allPagesEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  MongoUrlValue := GetEnvironmentVariable('MONGO_URL');
  if MongoUrlValue <> '' then
  begin
    Writeln('MONGO_URL environment variable value:');
    Writeln(MongoUrlValue);
  end
  else
  begin
    Writeln('MONGO_URL environment variable not found.');
  end;
  aResponse.Content := '';
  with aResponse.Contents do
  begin
    Add('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">');
    Add('<html><head><title>WeKan</title></head><body>');
    Add('WeKan');
    Add('<p>Serverside UserAgent: ' + aRequest.UserAgent);
    Writeln('Serverside UserAgent: ' + aRequest.UserAgent);
    Add('<p>Detected Browser: ' + WebBrowserName(aRequest.UserAgent) + '</p>');
    Writeln('Detected Browser: ' + WebBrowserName(aRequest.UserAgent));
    Add('<p>Serverside IPv4: ' + aRequest.RemoteAddr + '</p>');
    Writeln('Serverside IPv4: ' + aRequest.RemoteAddr);
    // New code to show screen width and height
    Add('<p><noscript><p>Browser does not support Javascript</noscript><script>document.write("Browser supports Javascript");</script>');
    Add('<p><span id="screenInfo"></span></p>');
    Add('<script language="JavaScript">');
    Add('function showBrowserWindowSize() {');
    Add('  var myWidth = 0, myHeight = 0;');
    Add('  if (typeof window.innerWidth === "number") {');
    Add('    myWidth = window.innerWidth;');
    Add('    myHeight = window.innerHeight;');
    Add('  } else {');
    Add('    myWidth = document.documentElement.clientWidth || document.body.clientWidth;');
    Add('    myHeight = document.documentElement.clientHeight || document.body.clientHeight;');
    Add('  }');
    Add('  document.getElementById("screenInfo").innerHTML = "Browser Javascript: window inner size: " + myWidth + " x " + myHeight;');
    Add('}');
    Add('window.onload = showBrowserWindowSize;');
    // Chrome: Shows window size
    // Netsurf: Supports document.write, but window size: undefined x undefined
    // IBrowse: Supports document.write, but does not show window size text at all
    Add('</script>');

    Add('<p><a href=".">All Pages</a> - <a href="multidrag/">Multidrag</a></p>');
    Add('<p><b>Login</b>: <a href="sign-in">Sign In</a>');
    Add(' - <a href="sign-up">Sign Up</a>');
    Add(' - <a href="forgot-password">Forgot Password</a></p>');
    Add('<p><b>Boards</b>: <a href="allboards">All Boards</a>');
    Add(' - <a href="public">Public</a> - <a href="/board">Board</a>');
    Add(' - <a href="shortcuts">Shortcuts</a>');
    Add(' - <a href="templates">Templates</a></p>');
    Add('<p><b>Cards</b>: <a href="my-cards">My Cards</a>');
    Add(' - <a href="due-cards">Due Cards</a>');
    Add(' - <a href="import">Import</a></p>');
    Add('<p><b><a href="calendar">Calendar</a></b></p>');
    Add('<p><b>Upload</b>: <a href="upload">Upload</a></p>');
    Add('<p><b>Search</b>: <a href="global-search">Global Search</a>');
    Add('<p><b>Admin</b>: <a href="broken-cards">Broken Cards</a>');
    Add(' - <a href="setting">Setting</a>');
    Add(' - <a href="information">Information</a>');
    Add(' - <a href="people">People</a>');
    Add(' - <a href="admin-reports">Admin Reports</a>');
    Add('- <a href="attachments">Attachments</a>');
    Add(' - <a href="translation">Translation</a></p>');
    Add('</body></html>');
  end;
  aResponse.Code:=200;
  aResponse.ContentType:='text/html';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure screenInfoEndpoint(aRequest: TRequest; aResponse: TResponse);
var
  ScreenWidth, ScreenHeight: string;
begin
  ScreenWidth := aRequest.ContentFields.Values['width'];
  ScreenHeight := aRequest.ContentFields.Values['height'];
  aResponse.Content := Format('Screen Width: %s, Screen Height: %s', [ScreenWidth, ScreenHeight]);
  aResponse.ContentType := 'text/plain';
  aResponse.SendContent;
end;

procedure loginEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '';
  with aResponse.Contents do
  begin
    Add('<p></p>');
    Add('<center>');
    Add('  <table border="0" padding="0" spacing="0" margin="0">');
    Add('    <tr>');
    Add('      <td style="background-color: #f7f7f7;">');
    Add('        <h1 style="font-size: 140%; padding-top: 10px;');
    Add('                   padding-left: 20px; padding-bottom: 0px;">');
    Add('          Login');
    Add('        </h1>');
    Add('      </td>');
    Add('    </tr>');
    Add('    <tr>');
    Add('      <td style="padding-top: 20px; padding-left: 20px;');
    Add('                 padding-right: 20px; background-color: white;">');
    Add('        <form    role="form"');
    Add('                 novalidate=""');
    Add('                 action="sign-in"');
    Add('                 method="POST">');
    Add('          <label for="select-authentication">');
    Add('                 Authentication method</label><br>');
    Add('          <input type="radio"');
    Add('                 name="select-authentication"');
    Add('                 value="password"');
    Add('                 required>');
    Add('          <label for="auth-password">Password</label><br>');
    Add('          <input type="radio"');
    Add('                 name="select-authentication"');
    Add('                 value="oauth2">');
    Add('          <label for="auth-oauth2">OAuth2</label><br>');
    Add('          <input type="radio"');
    Add('                 name="select-authentication"');
    Add('                 value="ldap">');
    Add('          <label for="auth-ldap">LDAP</label><br><br>');
    Add('          <label for="at-field-username_and_email">');
    Add('                 Username or Email</label><br>');
    Add('          <input type="text" size="41"');
    Add('                 name="username_or_email"');
    Add('                 placeholder=""');
    Add('                 autocapitalize="none"');
    Add('                 autocorrect="off"');
    Add('                 required><br><br>');
    Add('          <label for="at-field-password">Password</label><br />');
    Add('          <input type="password"');
    Add('                 size="41"');
    Add('                 name="password"');
    Add('                 placeholder=""');
    Add('                 autocapitalize="none"');
    Add('                 autocorrect="off"');
    Add('                 required><br />');
    Add('          <input type="submit"');
    Add('                 name="login"');
    Add('                 value="Login"');
    Add('       </form>');
    Add('       <p><a href="sign-up">Register</a></p>');
    Add('       <p><a href="forgot-password">Forgot password</a></p>');
    Add('    </td>');
    Add('  </tr>');
    Add('  </table>');
    Add('</center>');
  end;
  aResponse.Code:=200;
  aResponse.ContentType:='text/html';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure uploadEndpoint(aRequest: TRequest; aResponse: TResponse);
const
  UPLOAD_FORM = 
    '<form action="/upload" method="post" enctype="multipart/form-data">' +
    '<input type="file" name="file" required>' +
    '<input type="submit" value="Upload">' +
    '</form>';
  BUFFER_SIZE = 16384; // 16KB buffer
var
  ContentType: string;
  UploadStream: TFileStream;
  Buffer: array[0..BUFFER_SIZE-1] of Byte;
  BytesRead: Integer;
  FileName: string;
  UploadDir: string;
begin
  if aRequest.Method = 'GET' then
  begin
    // Show upload form
    aResponse.Content := UPLOAD_FORM;
    aResponse.Code := 200;
    aResponse.ContentType := 'text/html';
    aResponse.ContentLength := Length(aResponse.Content);
    aResponse.SendContent;
    Exit;
  end;

  if aRequest.Method = 'POST' then
  begin
    ContentType := aRequest.ContentType;
    if Pos('multipart/form-data', ContentType) = 0 then
    begin
      aResponse.Code := 400;
      aResponse.Content := 'Invalid content type';
      aResponse.SendContent;
      Exit;  
    end;
    
    try
      // Create uploads directory if it doesn't exist
      UploadDir := 'uploads';
      if not DirectoryExists(UploadDir) then
      begin
        if not CreateDir(UploadDir) then
        begin
          aResponse.Code := 500;
          aResponse.Content := 'Failed to create upload directory';
          aResponse.SendContent;
          Exit;
        end;
      end;

      // Parse multipart form data to get filename
      FileName := ExtractFileName(aRequest.Files[0].FileName);
      
      // Create output file stream
      UploadStream := TFileStream.Create('uploads/' + FileName, fmCreate);
      try
        // Stream file data in chunks
        while True do
        begin
          BytesRead := aRequest.Files[0].Stream.Read(Buffer, BUFFER_SIZE);
          if BytesRead = 0 then Break;
          UploadStream.WriteBuffer(Buffer, BytesRead);
        end;

        aResponse.Code := 200;
        aResponse.Content := 'File uploaded successfully';
      finally
        UploadStream.Free;
      end;
    except
      on E: Exception do
      begin
        aResponse.Code := 500;
        aResponse.Content := 'Upload failed: ' + E.Message;
      end;
    end;

    aResponse.ContentType := 'text/plain';
    aResponse.ContentLength := Length(aResponse.Content);
    aResponse.SendContent;
  end;
end;

procedure registerEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content:='Register';
  aResponse.Code:=200;
  aResponse.ContentType:='text/plain';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure forgotPasswordEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content:='Forgot Password';
  aResponse.Code:=200;
  aResponse.ContentType:='text/plain';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure userSettingsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '';
  with aResponse.Contents do
  begin
    Add('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">');
    Add('<html><head><title>User Settings</title></head>');
    Add('<body>');
    Add('  <table border="0" cellspacing="0" cellpadding="10" width="100%" id="bodytable">');
    Add('    <tr>');
    Add('      <td colspan="8" valign="top" valign="center" align="left" bgcolor="#2573a7">');
    Add('        <img src="images/logo-header.png" alt="WeKan ®" title="WeKan ®">  ');
    Add('        <img src="font/arrow/white/home.gif" width="20" height="20">');
    Add('        <font size="3" color="#FFFFFF" face="arial">All Boards</font>  ');
    Add('        <a href="/b/D2SzJKZDS4Z48yeQH/wekan-r-open-source-kanban-board-with-mit-license">');
    Add('          <font size="3" color="#FFFFFF" face="arial">WeKan ® Open Source Kanban board with MIT license</font>');
    Add('        </a>  ');
    Add('        <a href="/b/JctQEtkayWXTTJyzt/wekan-multiverse"><font size="3" color="#FFFFFF" face="arial">WeKan Multiverse</font></a>  ');
    Add('        <a title="Show desktop drag handles" alt="Show desktop drag handles" href="#" aria-label="Show desktop drag handles">');
    Add('          <img src="font/arrow/white/arrows.gif" width="20" height="20">');
    Add('          <img src="font/arrow/white/ban.gif" width="20" height="20">');
    Add('        </a>  ');
    Add('        <a href="notifications" name="Notifications"><img src="font/notification/white/bell.gif" width="20" height="20"></a>  ');
    Add('        <a href="usersettings" name="UserSettings"><img src="font/setting/white/cog.gif" width="20" heigth="20">  ');
    Add('        <font size="3" color="#FFFFFF" face="arial">User Name</font></a>');
    Add('      </td>');
    Add('    </tr>');
    Add('    <tr>');
    Add('      <td colspan="8" bgcolor="#2980b9"><font size="5" color="#FFFFFF" face="arial"><b>User Settings</b></font></td>');
    Add('    </tr>');
    Add('  </table>');
    Add('</body>');
    Add('</html>');
  end;
  aResponse.Code:=200;
  aResponse.ContentType:='text/html';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure notificationsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '';
  with aResponse.Contents do
  begin
    Add('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">');
    Add('<html><head><title>Notifications</title></head>');
    Add('<body>');
    Add('  <table border="0" cellspacing="0" cellpadding="10" width="100%" id="bodytable">');
    Add('    <tr>');
    Add('      <td colspan="8" valign="top" valign="center" align="left" bgcolor="#2573a7">');
    Add('        <img src="images/logo-header.png" alt="WeKan ®" title="WeKan ®"><img src="font/arrow/white/home.gif" width="20" height="20">');
    Add('        <font size="3" color="#FFFFFF" face="arial">All Boards</font>');
    Add('        <a href="/b/D2SzJKZDS4Z48yeQH/wekan-r-open-source-kanban-board-with-mit-license">');
    Add('        <font size="3" color="#FFFFFF" face="arial">WeKan ® Open Source Kanban board with MIT license</font></a> ');
    Add('        <a href="/b/JctQEtkayWXTTJyzt/wekan-multiverse"><font size="3" color="#FFFFFF" face="arial">WeKan Multiverse</font></a> ');
    Add('        <a title="Show desktop drag handles" alt="Show desktop drag handles" href="#" aria-label="Show desktop drag handles">');
    Add('        <img src="font/arrow/white/arrows.gif" width="20" height="20"> <img src="font/arrow/white/ban.gif" width="20" height="20"></a> ');
    Add('        <a href="notifications" name="Notifications"><img src="font/notification/white/bell.gif" width="20" height="20"></a> ');
    Add('        <a href="usersettings" name="UserSettings"><img src="font/setting/white/cog.gif" width="20" heigth="20"> ');
    Add('        <font size="3" color="#FFFFFF" face="arial">User Name</font></a>');
    Add('      </td>');
    Add('    </tr>');
    Add('    <tr>');
    Add('      <td colspan="8" bgcolor="#2980b9"><font size="5" color="#FFFFFF" face="arial"><b>Notifications</b></font></td>');
    Add('    </tr>');
    Add('  </table>');
    Add('</body>');
    Add('</html>');
  end;
  aResponse.Code:=200;
  aResponse.ContentType:='text/html';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure allBoardsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '';
  with aResponse.Contents do
  begin
    // This All Boards page has been tested to be visible at: Amiga IBrowse, FreeDOS Dillo, Linux Dillo/Netsurf/Chrome'
    // TODO: At IBrowse, cards are at 1 list only. Change to be at different lists.
    Add('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">');
    Add('<html xmlns:v="urn:schemas-microsoft-com:vml" xmlns:o="urn:schemas-microsoft-com:office:office">');
    Add('<head>');
    Add('<title>WeKan</title>');
    Add('<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">');
    Add('<meta name="viewport" content="maximum-scale=1.0,width=device-width">');
    Add('<meta http-equiv="X-UA-Compatible" content="IE=edge">');
    Add('<meta name="application-name" content="WeKan">');
    Add('<meta name="msapplication-TileColor" content="#00aba9">');
    Add('<meta name="theme-color" content="#fff">');
    Add('<link rel="stylesheet" type="text/css" href="css/interact.css">');
    Add('<script src="js/interact.js"></script>');
    Add('</head>');
    Add('<body>');
    Add('  <table border="0" cellspacing="0" cellpadding="10" width="100%" id="bodytable"  bgcolor="#2573a7">');
    Add('    <tr>');
    Add('      <td colspan="8" align="left" valign="middle">');
    Add('        <img src="images/logo-header.png" alt="WeKan" title="WeKan">&nbsp;');
    Add('        <a href="#"><img src="font/arrow/white/home.gif" width="20"><font size="3" color="#FFFFFF" face="arial">All Boards</font></a>&nbsp;');
    Add('        <a title="Show desktop drag handles" alt="Show desktop drag handles" href="#" aria-label="Show desktop drag handles"> ');
    Add('          <img src="font/arrow/white/arrows.gif" width="20" height="20"><img src="font/arrow/white/ban.gif" width="20" height="20"> ');
    Add('        </a>&nbsp;');
    Add('        <a href="/b/D2SzJKZDS4Z48yeQH/wekan-r-open-source-kanban-board-with-mit-license"><font size="3" color="#FFFFFF" face="arial">WeKan Open Source Kanban board with MIT license</font></a>');
    Add('        <a href="/b/JctQEtkayWXTTJyzt/wekan-multiverse"><font size="3" color="#FFFFFF" face="arial">WeKan Multiverse</font></a>');
    Add('        <a href="notifications" name="Notifications"><img src="font/notification/white/bell.gif" width="20" height="20" hspace="10"></a>');
    Add('        <a href="usersettings" name="UserSettings"><font size="3" color="#FFFFFF" face="arial">[?] User Name</font></a>');
    Add('        <a href="boardsettings" name="BoardSettings"><img src="font/setting/white/cog.gif" width="20" heigth="20" hspace="10"></a>');
    Add('      </td>');
    Add('    </tr>');
    Add('    <tr>');
    Add('      <td colspan="8" bgcolor="#2980b9"><font size="5" color="#FFFFFF" face="arial"><b>My Boards</b></font></td>');
    Add('    </tr>');
    // Works with this code:
    //  - IE6 at Win2k does work with this button HTML code.
    //  - Amiga IBrowse:
    //    - Does not support image inside of button.
    //    - Dot show tooltip at all. Not for image, not for button.  Tooltip is with alt and aria-label.
    //  - FreeDOS Dillo:
    //    - Supports image inside of button, but better to get image visible at IBrowser, having image outside of button.
    //    - Requires image width and height in pixels. Setting it to "auto" produces error.
    //    - Requires buttons to be at table from left to right, because without table button width is full page width.
    //    - Can not change button background color.
    //    - Shows image tooltip. It does not show button tooltip. Tooltip is with alt and aria-label.
    // Does not support HTML button at all:
    //  - IE3 at Win95 does not support HTML button at all. Buttons are useful for submit form buttons and accessibility.
    //  - Netscape at OS/2
    Add('    <tr bgcolor="#2980b9">');
    Add('      <td align="left= valign=top"><button class="js-add-board" title="Add Board" aria-label="Add Board" width="20" href="">Add Board</button></td>');
    Add('      <td align="right" valign="top"><img src="font/star.gif" width="20" height="20" title="Click to star board. It will show at top of your board list." aria-label="Click to star board. It will show at top of your board list."></td>');
    Add('      <td align="left" valign="top"><button class="js-star-board" title="Click to star board. It will show at top of your board list." aria-label="Click to star board. It will show at top of your board list." href="#">Star board</button></td>');
    Add('      <td align="right" valign="top"><img src="font/arrow/white/duplicate.gif" width="20" height="20"  title="Duplicate board" aria-label="Duplicate board"></td>');
    Add('      <td align="left" valign="top"><button class="js-clone-board" title="Duplicate board" aria-label="Duplicate board" href="#">Duplicate board</button></td>');
    Add('      <td align="right" valign="top"><img src="font/arrow/white/archive.gif" width="20" height="20" title="Move to Archive" aria-label="Move to Archive"></td>');
    Add('      <td align="left" valign="top"><button class="js-archive-board" title="Move to Archive" aria-label="Move to Archive" href="#">Archive board</button></td>');
    Add('      <td><b><font color="#FFFFFF">Teams/Orgs</font></b></td>');
    Add('    </tr>');
    Add('  </table>');
    // <h2>MultiDrag test, for future version of <a href="https://wekan.github.io">WeKan Open Source kanban</a></h2>
    // <p>This MultiDrag test uses <a href="https://interactjs.io">https://interactjs.io</a> drag drop</p>
    // <p>Features:</p>
    // <ul>
    // <li>Newest browsers, MultiDrag: Touchscreen, you can drag many cards at once with each finger.
    //     Use case: Many users can drag their own card at big touch screen. Or one user can drag many cards.
    //     There is not any kanban software with MultiDrag feature yet.
    // </li>
    // <li>Ladybird, OneDrag: Touchscreen, drag only one card at once. Is it possible to get MultiDrag?</li>
    // <li>Legacy Browsers: Uses HTML4 tables, and GIF image for rounded corners, to be visible at Netsurf and Amiga IBrowse.
    //     No drag drop features, so there will be buttons to move cards.
    // </li>
    // </ul> 
    // Background GIF produces dithered background at Amiga IBrowse,
    // so better to use table background color to get solid color:
    // table background="img/round-blue.gif" => table bgcolor="#5e98c2"
    // But this causes card to not have rounded corners at Netsurf and Amiga IBrowse.
    // So, to have rounded corners at Netsurf and Amiga IBrowse, use background GIF.
    //
    // valid_colors = ['white', 'green', 'yellow', 'orange', 'red', 'purple', 'blue', 'sky', 'lime', 'pink', 'black',
    //                 'silver', 'peachpuff', 'crimson', 'plum', 'darkgreen', 'slateblue', 'magenta', 'gold', 'navy',
    //                 'gray', 'saddlebrown', 'paleturquoise', 'mistyrose', 'indigo']
    //
    Add(BoardIcon('At touchscreen', 1, 'white', 'blue'));
    Add(BoardIcon('Drag many at once', 2, 'white', 'green'));
    Add(BoardIcon('Visible at Netsurf', 3, 'white', 'red'));
    Add(BoardIcon('Amiga IBrowse', 4, 'black', 'yellow'));
    Add(BoardIcon('For all browsers', 5, 'white', 'gray'));
    Add(BoardIcon('And all screen sizes', 6, 'white', 'black'));
    Add(BoardIcon('And all OS', 7, 'black', 'cyan'));
    Add(BoardIcon('And CPUs', 8, 'black', 'pink'));
    Add(BoardIcon('At Earth', 9, 'black', 'orange'));
    Add(BoardIcon('And Space', 10, 'black', 'lightblue'));
    Add('<br>');
    Add(DrawLine(0,0,270,150,500,210,'red','2'));
    Add('<script src="js/interact-bottom.js"></script>');
    Add('</body>');
    Add('</html>');
  end;
  aResponse.Code:=200;
  aResponse.ContentType:='text/html';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure publicEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content:='Public';
  aResponse.Code:=200;
  aResponse.ContentType:='text/plain';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure boardEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '';
  with aResponse.Contents do
  begin
    Add('<style>');
    Add('  .swimlane { margin-bottom: 20px; }');
    Add('  .kanban-lists-container {');
    Add('    display: flex;');
    Add('    flex-direction: row;');
    Add('    gap: 20px;');
    Add('    overflow-x: auto;');
    Add('  }');
    Add('  .kanban-list {');
    Add('    min-width: 250px;');
    Add('    background: #f4f4f4;');
    Add('    padding: 10px;');
    Add('    border-radius: 4px;');
    Add('  }');
    Add('  .selected {');
    Add('    outline: 2px solid #0066ff;');
    Add('    box-shadow: 0 0 5px rgba(0,102,255,0.5);');
    Add('  }');
    Add('  .moving {');
    Add('    opacity: 0.7;');
    Add('    background: #e0e0e0;');
    Add('  }');
    Add('  .kanban-card {');
    Add('    background: white;');
    Add('    padding: 8px;');
    Add('    margin: 8px 0;');
    Add('    border-radius: 3px;');
    Add('    box-shadow: 0 1px 3px rgba(0,0,0,0.12);');
    Add('    cursor: pointer;');
    Add('  }');
    Add('  .card-placeholder {');
    Add('    border: 2px dashed #ccc;');
    Add('    background: #f9f9f9;');
    Add('    height: 30px;');
    Add('    margin: 8px 0;');
    Add('  }');
    Add('  .swimlane-placeholder {');
    Add('    border: 2px dashed #ccc;');
    Add('    background: #f9f9f9;');
    Add('    height: 50px;');
    Add('    margin-bottom: 20px;');
    Add('  }');
    Add('  .kanban-list-title {');
    Add('    cursor: move;');
    Add('  }');
    Add('</style>');
    Add('<section id="kanban-board" aria-label="Kanban Board" role="region" tabindex="0">');
    Add('  <h1>Accessible Kanban Board</h1>');
    Add('  <div class="swimlane" role="region" aria-label="Swimlane 1" tabindex="0" draggable="true">');
    Add('    <h2>Swimlane 1</h2>');
    Add('    <div class="kanban-lists-container">');
    Add('      <div class="kanban-list" role="list" aria-label="List 1" tabindex="0">');
    Add('        <h3 class="kanban-list-title" draggable="true">List 1</h3>');
    Add('        <div class="kanban-card" role="listitem" tabindex="0">Card 1</div>');
    Add('        <div class="kanban-card" role="listitem" tabindex="0">Card 2</div>');
    Add('      </div>');
    Add('      <div class="kanban-list" role="list" aria-label="List 2" tabindex="0">');
    Add('        <h3 class="kanban-list-title" draggable="true">List 2</h3>');
    Add('        <div class="kanban-card" role="listitem" tabindex="0">Card A</div>');
    Add('        <div class="kanban-card" role="listitem" tabindex="0">Card B</div>');
    Add('      </div>');
    Add('    </div>');
    Add('  </div>');
    Add('  <div class="swimlane" role="region" aria-label="Swimlane 2" tabindex="0" draggable="true">');
    Add('    <h2>Swimlane 2</h2>');
    Add('    <div class="kanban-lists-container">');
    Add('      <div class="kanban-list" role="list" aria-label="List 3" tabindex="0">');
    Add('        <h3 class="kanban-list-title" draggable="true">List 3</h3>');
    Add('        <div class="kanban-card" role="listitem" tabindex="0">Card 3</div>');
    Add('        <div class="kanban-card" role="listitem" tabindex="0">Card 4</div>');
    Add('      </div>');
    Add('      <div class="kanban-list" role="list" aria-label="List 4" tabindex="0">');
    Add('        <h3 class="kanban-list-title" draggable="true">List 4</h3>');
    Add('        <div class="kanban-card" role="listitem" tabindex="0">Card C</div>');
    Add('        <div class="kanban-card" role="listitem" tabindex="0">Card D</div>');
    Add('      </div>');
    Add('    </div>');
    Add('  </div>');
    Add('  <script>');
    Add('    let selectedElement = null;');
    Add('    let isMoving = false;');
    Add('    let sourceList = null;');
    Add('    let placeholder = null;');
    Add('    let swimlanePlaceholder = null;');
    Add('');
    Add('    function handleSelection(e) {');
    Add('      const card = e.target.closest(".kanban-card");');
    Add('      if (!card) return;');
    Add('');
    Add('      if (e.key === " ") {');
    Add('        e.preventDefault();');
    Add('        if (!isMoving) {');
    Add('          if (selectedElement) {');
    Add('            selectedElement.classList.remove("selected");');
    Add('          }');
    Add('          selectedElement = card;');
    Add('          sourceList = card.closest(".kanban-list");');
    Add('          selectedElement.classList.add("selected", "moving");');
    Add('          selectedElement.setAttribute("aria-grabbed", "true");');
    Add('          isMoving = true;');
    Add('          placeholder = document.createElement("div");');
    Add('          placeholder.className = "card-placeholder";');
    Add('          card.parentNode.insertBefore(placeholder, card.nextSibling);');
    Add('        } else {');
    Add('          completeMove();');
    Add('        }');
    Add('      }');
    Add('    }');
    Add('');
    Add('    function completeMove() {');
    Add('      if (!selectedElement) return;');
    Add('');
    Add('      selectedElement.classList.remove("moving", "selected");');
    Add('      selectedElement.setAttribute("aria-grabbed", "false");');
    Add('      const targetList = document.activeElement.closest(".kanban-list");');
    Add('      if (targetList) {');
    Add('        targetList.insertBefore(selectedElement, placeholder);');
    Add('        announceMove(sourceList, targetList);');
    Add('      }');
    Add('      placeholder.remove();');
    Add('      isMoving = false;');
    Add('      sourceList = null;');
    Add('      placeholder = null;');
    Add('    }');
    Add('');
    Add('    function announceMove(from, to) {');
    Add('      const announcement = document.createElement("div");');
    Add('      announcement.setAttribute("role", "alert");');
    Add('      announcement.setAttribute("aria-live", "polite");');
    Add('      announcement.textContent = `Card moved from ${from.querySelector("h3").textContent} to ${to.querySelector("h3").textContent}`;');
    Add('      document.body.appendChild(announcement);');
    Add('      setTimeout(() => announcement.remove(), 1000);');
    Add('    }');
    Add('');
    Add('    document.addEventListener("keydown", function(e) {');
    Add('      if (e.key === " ") {');
    Add('        handleSelection(e);');
    Add('      }');
    Add('      ');
    Add('      if (isMoving) {');
    Add('        const lists = Array.from(document.querySelectorAll(".kanban-list"));');
    Add('        const currentList = document.activeElement.closest(".kanban-list");');
    Add('        const currentIndex = lists.indexOf(currentList);');
    Add('');
    Add('        switch(e.key) {');
    Add('          case "ArrowLeft":');
    Add('            e.preventDefault();');
    Add('            if (currentIndex > 0) {');
    Add('              lists[currentIndex - 1].focus();');
    Add('            }');
    Add('            break;');
    Add('          case "ArrowRight":');
    Add('            e.preventDefault();');
    Add('            if (currentIndex < lists.length - 1) {');
    Add('              lists[currentIndex + 1].focus();');
    Add('            }');
    Add('            break;');
    Add('          case "ArrowUp":');
    Add('            e.preventDefault();');
    Add('            if (selectedElement.previousElementSibling && selectedElement.previousElementSibling !== placeholder) {');
    Add('              selectedElement.parentNode.insertBefore(placeholder, selectedElement.previousElementSibling);');
    Add('            }');
    Add('            break;');
    Add('          case "ArrowDown":');
    Add('            e.preventDefault();');
    Add('            if (selectedElement.nextElementSibling) {');
    Add('              selectedElement.parentNode.insertBefore(placeholder, selectedElement.nextElementSibling.nextSibling);');
    Add('            }');
    Add('            break;');
    Add('          case "Escape":');
    Add('            e.preventDefault();');
    Add('            selectedElement.classList.remove("moving", "selected");');
    Add('            selectedElement.setAttribute("aria-grabbed", "false");');
    Add('            placeholder.remove();');
    Add('            isMoving = false;');
    Add('            break;');
    Add('        }');
    Add('      }');
    Add('    });');
    Add('');
    Add('    document.querySelectorAll(".kanban-card").forEach(card => {');
    Add('      card.setAttribute("draggable", "true");');
    Add('      card.addEventListener("dragstart", function(e) {');
    Add('        selectedElement = e.target;');
    Add('        sourceList = selectedElement.closest(".kanban-list");');
    Add('        e.dataTransfer.effectAllowed = "move";');
    Add('        setTimeout(() => selectedElement.classList.add("moving"), 0);');
    Add('      });');
    Add('      card.addEventListener("dragend", function() {');
    Add('        selectedElement.classList.remove("moving");');
    Add('        selectedElement = null;');
    Add('        sourceList = null;');
    Add('      });');
    Add('    });');
    Add('');
    Add('    document.querySelectorAll(".kanban-list").forEach(list => {');
    Add('      list.addEventListener("dragover", function(e) {');
    Add('        e.preventDefault();');
    Add('        e.dataTransfer.dropEffect = "move";');
    Add('        const afterElement = getDragAfterElement(list, e.clientY);');
    Add('        if (afterElement == null) {');
    Add('          list.appendChild(placeholder);');
    Add('        } else {');
    Add('          list.insertBefore(placeholder, afterElement);');
    Add('        }');
    Add('      });');
    Add('      list.addEventListener("drop", function(e) {');
    Add('        e.preventDefault();');
    Add('        if (selectedElement) {');
    Add('          list.insertBefore(selectedElement, placeholder);');
    Add('          announceMove(sourceList, list);');
    Add('        }');
    Add('        placeholder.remove();');
    Add('      });');
    Add('    });');
    Add('');
    Add('    document.querySelectorAll(".swimlane").forEach(swimlane => {');
    Add('      swimlane.addEventListener("dragstart", function(e) {');
    Add('        selectedElement = e.target;');
    Add('        e.dataTransfer.effectAllowed = "move";');
    Add('        setTimeout(() => selectedElement.classList.add("moving"), 0);');
    Add('      });');
    Add('      swimlane.addEventListener("dragend", function() {');
    Add('        selectedElement.classList.remove("moving");');
    Add('        selectedElement = null;');
    Add('      });');
    Add('      swimlane.addEventListener("dragover", function(e) {');
    Add('        e.preventDefault();');
    Add('        e.dataTransfer.dropEffect = "move";');
    Add('        const afterElement = getDragAfterElement(swimlane.parentNode, e.clientY);');
    Add('        if (afterElement == null) {');
    Add('          swimlane.parentNode.appendChild(swimlanePlaceholder);');
    Add('        } else {');
    Add('          swimlane.parentNode.insertBefore(swimlanePlaceholder, afterElement);');
    Add('        }');
    Add('      });');
    Add('      swimlane.addEventListener("drop", function(e) {');
    Add('        e.preventDefault();');
    Add('        if (selectedElement) {');
    Add('          swimlane.parentNode.insertBefore(selectedElement, swimlanePlaceholder);');
    Add('        }');
    Add('        swimlanePlaceholder.remove();');
    Add('      });');
    Add('    });');
    Add('');
    Add('    document.querySelectorAll(".kanban-list-title").forEach(title => {');
    Add('      title.addEventListener("dragstart", function(e) {');
    Add('        selectedElement = e.target.closest(".kanban-list");');
    Add('        e.dataTransfer.effectAllowed = "move";');
    Add('        setTimeout(() => selectedElement.classList.add("moving"), 0);');
    Add('      });');
    Add('      title.addEventListener("dragend", function() {');
    Add('        selectedElement.classList.remove("moving");');
    Add('        selectedElement = null;');
    Add('      });');
    Add('    });');
    Add('');
    Add('    function getDragAfterElement(container, y) {');
    Add('      const draggableElements = [...container.querySelectorAll(".swimlane:not(.moving)")];');
    Add('');
    Add('      return draggableElements.reduce((closest, child) => {');
    Add('        const box = child.getBoundingClientRect();');
    Add('        const offset = y - box.top - box.height / 2;');
    Add('        if (offset < 0 && offset > closest.offset) {');
    Add('          return { offset: offset, element: child };');
    Add('        } else {');
    Add('          return closest;');
    Add('        }');
    Add('      }, { offset: Number.NEGATIVE_INFINITY }).element;');
    Add('    }');
    Add('  </script>');
    Add('</section>');
  end;
  aResponse.Code:=200;
  aResponse.ContentType:='text/html';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure cardEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content:='Card';
  aResponse.Code:=200;
  aResponse.ContentType:='text/plain';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure shortcutsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content:='Shortcuts';
  aResponse.Code:=200;
  aResponse.ContentType:='text/plain';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure calendarEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '';
  with aResponse.Contents do
  begin
    // This calendar has been tested to be visible at: Amiga IBrowse, FreeDOS Dillo, Linux Dillo/Netsurf/Chrome
    //
    // For accessibility, use only one table. Not nested table. Avoid merged or split cells.
    // Consider Alternative Views: For calendars, offer a "list view" or other simplified
    // representations that are inherently more linear and accessible for assistive technology users.
    //
    Add('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">');
    Add('<html lang="en">');
    Add('<head>');
    Add('  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">');
    Add('  <title>July 2025 Calendar</title>');
    Add('  <style type="text/css">');
    Add('    table      { width: 100%; border-collapse: collapse; margin-bottom: 20px; }');
    Add('    th, td     { border: 1px solid #ccc; padding: 8px; text-align: center; }');
    Add('    th         { background-color: #f2f2f2; }');
    Add('    .empty-day { background-color: #eee; color: #999; }');
    // background-color: #d1e7dd; Light green to highlight today
    Add('    .today     { background-color: #d1e7dd; font-weight: bold; border: 2px solid #007bff; }');
    Add('    caption    { font-size: 1.5em; margin-bottom: 10px; font-weight: bold; text-align: left; }');
    Add('  </style>');
    Add('</head>');
    Add('<body bgcolor="#FFFFFF">');
    Add('<p><a href="../">All Pages</a></p>');
    Add('<h1><font face="arial">July 2025</font></h1>');
    Add('<table summary="This table displays the calendar. Days of the week are column headers, and dates are listed under them." width="100%" border-collapse="collapse" margin-bottom="20" border="1" cellpadding="5" cellspacing="0" id="calendartable">');
    Add('  <caption><font size="4" face="arial">July 2025</font></caption>');
    Add('  <thead>');
    Add('    <tr>');
    Add('      <th scope="col" border="1" padding="8" align="center" bgcolor="#f2f2f2"><font size="4" face="arial">Monday</font></th>');
    Add('      <th scope="col" border="1" padding="8" align="center" bgcolor="#f2f2f2"><font size="4" face="arial">Tuesday</font></th>');
    Add('      <th scope="col" border="1" padding="8" align="center" bgcolor="#f2f2f2"><font size="4" face="arial">Wednesday</font></th>');
    Add('      <th scope="col" border="1" padding="8" align="center" bgcolor="#f2f2f2"><font size="4" face="arial">Thursday</font></th>');
    Add('      <th scope="col" border="1" padding="8" align="center" bgcolor="#f2f2f2"><font size="4" face="arial">Friday</font></th>');
    Add('      <th scope="col" border="1" padding="8" align="center" bgcolor="#f2f2f2"><font size="4" face="arial">Saturday</font></th>');
    Add('      <th scope="col" border="1" padding="8" align="center" bgcolor="#f2f2f2"><font size="4" face="arial">Sunday</font></th>');
    Add('    </tr>');
    Add('  </thead>');
    Add('  <tbody>');
    Add('    <tr>');
    Add('      <td class="empty-day" border="1" padding="8" align="center" color="#999999" bgcolor="#eeeeee"><font size="4" face="arial"></font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">1</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">2</font></td>');
    Add('      <td class="today" border="3" padding="8" align="center" bgcolor="#d1e7dd" border="2"><b><font size="4" face="arial">3</font></b></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">4</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">5</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">6</font></td>');
    Add('    </tr>');
    Add('    <tr>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">7</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">8</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">9</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">10</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">11</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">12</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">13</font></td>');
    Add('    </tr>');
    Add('    <tr>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">14</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">15</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">16</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">17</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">18</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">19</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">20</font></td>');
    Add('    </tr>');
    Add('    <tr>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">21</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">22</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">23</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">24</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">25</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">26</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">27</font></td>');
    Add('    </tr>');
    Add('    <tr>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">28</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">29</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">30</font></td>');
    Add('      <td border="1" padding="8" align="center"><font size="4" face="arial">31</font></td>');
    Add('      <td class="empty-day" border="1" padding="8" align="center" color="#999999" bgcolor="#eeeeee"><font size="4" face="arial"></font></td>');
    Add('      <td class="empty-day" border="1" padding="8" align="center" color="#999999" bgcolor="#eeeeee"><font size="4" face="arial"></font></td>');
    Add('      <td class="empty-day" border="1" padding="8" align="center" color="#999999" bgcolor="#eeeeee"><font size="4" face="arial"></font></td>');
    Add('    </tr>');
    Add('  </tbody>');
    Add('</table>');
    Add('</body>');
    Add('</html>');
  end;
  aResponse.Code:=200;
  aResponse.ContentType:='text/html';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure templatesEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content:='Templates';
  aResponse.Code:=200;
  aResponse.ContentType:='text/plain';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure myCardsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content:='My Cards';
  aResponse.Code:=200;
  aResponse.ContentType:='text/plain';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure dueCardsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content:='Due Cards';
  aResponse.Code:=200;
  aResponse.ContentType:='text/plain';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure globalSearchEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content:='Global Search';
  aResponse.Code:=200;
  aResponse.ContentType:='text/plain';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure brokenCardsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content:='Broken Cards';
  aResponse.Code:=200;
  aResponse.ContentType:='text/plain';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure importEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content:='Import';
  aResponse.Code:=200;
  aResponse.ContentType:='text/plain';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure adminSettingEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content:='Admin Settings';
  aResponse.Code:=200;
  aResponse.ContentType:='text/plain';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure informationEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content:='Information';
  aResponse.Code:=200;
  aResponse.ContentType:='text/plain';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure peopleEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content:='People';
  aResponse.Code:=200;
  aResponse.ContentType:='text/plain';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure adminReportsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content:='Admin Reports';
  aResponse.Code:=200;
  aResponse.ContentType:='text/plain';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure translationEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content:='Translation';
  aResponse.Code:=200;
  aResponse.ContentType:='text/plain';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure jsonEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content:='{"everything":"great"}';
  aResponse.Code:=200;
  aResponse.ContentType:='application/json';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure serveStaticFileEndpoint(aRequest: TRequest; aResponse: TResponse);
var
  FilePath: String;
  FileStream: TFileStream;
  FileExt: String;
  MimeType: String;
  IndexPath: String;
  RequestPath: String;
  FullPublicPath: String;
  NormalizedFilePath: String;
begin
  // Security: Check for directory traversal attempts
  RequestPath := aRequest.PathInfo;
  if (Pos('..', RequestPath) > 0) or (Pos('/./', RequestPath) > 0) then
  begin
    aResponse.Code := 403;
    aResponse.Content := 'Access denied: Invalid path';
    aResponse.ContentType := 'text/plain';
    aResponse.SendContent;
    Exit;
  end;

  // Get absolute path to public directory for security checks
  FullPublicPath := ExpandFileName('public');
  
  // Construct path to requested file in public directory
  FilePath := 'public' + RequestPath;
  
  // If the path ends with a directory separator, append 'index.html'
  if (Length(FilePath) > 0) and (FilePath[Length(FilePath)] = DirectorySeparator) then
    FilePath := FilePath + 'index.html'
  // Check if the path is a directory, and if so, look for index.html
  else if DirectoryExists(FilePath) then
  begin
    IndexPath := IncludeTrailingPathDelimiter(FilePath) + 'index.html';
    if FileExists(IndexPath) then
      FilePath := IndexPath;
  end;
  
  // Security: Ensure the file is within the public directory
  NormalizedFilePath := ExpandFileName(FilePath);
  if (Copy(NormalizedFilePath, 1, Length(FullPublicPath)) <> FullPublicPath) then
  begin
    aResponse.Code := 403;
    aResponse.Content := 'Access denied: Path outside public directory';
    aResponse.ContentType := 'text/plain';
    aResponse.SendContent;
    Exit;
  end;
  
  // Check if file exists
  if not FileExists(NormalizedFilePath) then
  begin
    // If file doesn't exist, let the catchall handle it
    aResponse.Code := 404;
    aResponse.Content := 'File not found: ' + aRequest.PathInfo;
    aResponse.ContentType := 'text/plain';
    aResponse.SendContent;
    Exit;
  end;
  
  try
    // Determine content type based on file extension
    FileExt := LowerCase(ExtractFileExt(NormalizedFilePath));
    MimeType := 'application/octet-stream'; // Default MIME type
    
    if FileExt = '.html' then
      MimeType := 'text/html'
    else if FileExt = '.css' then
      MimeType := 'text/css'
    else if FileExt = '.js' then
      MimeType := 'application/javascript'
    else if FileExt = '.json' then
      MimeType := 'application/json'
    else if FileExt = '.png' then
      MimeType := 'image/png'
    else if FileExt = '.jpg' then
      MimeType := 'image/jpeg'
    else if FileExt = '.gif' then
      MimeType := 'image/gif'
    else if FileExt = '.svg' then
      MimeType := 'image/svg+xml'
    else if FileExt = '.ico' then
      MimeType := 'image/x-icon'
    else if FileExt = '.txt' then
      MimeType := 'text/plain';
      
    // Open file and send its contents
    FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
    try
      aResponse.ContentStream := FileStream;
      aResponse.ContentType := MimeType;
      aResponse.FreeContentStream := True; // Let TResponse free the stream
      aResponse.SendContent;
    except
      on E: Exception do
      begin
        FileStream.Free;
        aResponse.Code := 500;
        aResponse.Content := 'Error serving file: ' + E.Message;
        aResponse.ContentType := 'text/plain';
        aResponse.SendContent;
      end;
    end;
  except
    on E: Exception do
    begin
      aResponse.Code := 500;
      aResponse.Content := 'Error: ' + E.Message;
      aResponse.ContentType := 'text/plain';
      aResponse.SendContent;
    end;
  end;
end;

// We maintain a list of redirections to ensure that we don't break old URLs
// when we change our routing scheme.
//-- const redirections = {
//--   '/boards': '/',
//--   '/boards/:id/:slug': '/b/:id/:slug',
//--   '/boards/:id/:slug/:cardId': '/b/:id/:slug/:cardId',
//--  '/import': '/import/trello',
//--};
//--        redirect(FlowRouter.path(newPath, context.params));
//-- set static assets
//fm.setRoute("/*", "/assets/*")

begin
  Application.Port:=5500;
  HTTPRouter.RegisterRoute('/', rmGet, @allPagesEndpoint);
  HTTPRouter.RegisterRoute('/sign-in', rmGet, @loginEndpoint);
  HTTPRouter.RegisterRoute('/sign-up', rmGet, @registerEndpoint);
  HTTPRouter.RegisterRoute('/forgot-password', rmGet, @forgotPasswordEndpoint);
  HTTPRouter.RegisterRoute('/allboards', rmGet, @allBoardsEndpoint);
  HTTPRouter.RegisterRoute('/usersettings', rmGet, @userSettingsEndpoint);
  HTTPRouter.RegisterRoute('/notifications', rmGet, @notificationsEndpoint);
  HTTPRouter.RegisterRoute('/public', rmGet, @publicEndpoint);
  HTTPRouter.RegisterRoute('/board', rmGet, @boardEndpoint);
// fm.setRoute(fm.GET "/b/:boardId/:slug/:cardId", card)
  HTTPRouter.RegisterRoute('/card', rmGet, @cardEndpoint);
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
  HTTPRouter.RegisterRoute('/calendar', rmGet, @calendarEndpoint);
  HTTPRouter.RegisterRoute('/upload', rmGet, @uploadEndpoint);
  HTTPRouter.RegisterRoute('/upload', rmPost, @uploadEndpoint);
  HTTPRouter.RegisterRoute('/translation', rmGet, @translationEndpoint);
  HTTPRouter.RegisterRoute('/json', rmGet, @jsonEndpoint);
  HTTPRouter.RegisterRoute('/screeninfo', @screenInfoEndpoint);
  
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
  Application.Run;
end.
