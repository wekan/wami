unit wekan_web;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fphttpapp, HTTPDefs, httproute, Classes;

// Route registration
procedure RegisterWeKanRoutes;

// Endpoint handlers
procedure allPagesEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure loginEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure registerEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure forgotPasswordEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure logoutEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure allBoardsEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure publicEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure boardEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure cardEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure uploadEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure userSettingsEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure notificationsEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure shortcutsEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure calendarEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure templatesEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure myCardsEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure dueCardsEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure globalSearchEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure brokenCardsEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure accessibilityEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure importEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure adminSettingEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure informationEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure peopleEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure adminReportsEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure translationEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure attachmentsEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure jsonEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure screenInfoEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure serveStaticFileEndpoint(aRequest: TRequest; aResponse: TResponse);
procedure catchallEndpoint(aRequest: TRequest; aResponse: TResponse);

implementation

uses
  wekan_auth, wekan_utils;

// Global variables for current user
var
  CurrentUser: string;

// Route registration
procedure RegisterWeKanRoutes;
begin
  // Main routes matching router.js structure
  HTTPRouter.RegisterRoute('/', rmGet, @allPagesEndpoint);
  
  // Authentication routes
  HTTPRouter.RegisterRoute('/sign-in', rmGet, @loginEndpoint);
  HTTPRouter.RegisterRoute('/sign-in', rmPost, @loginEndpoint);
  HTTPRouter.RegisterRoute('/sign-up', rmGet, @registerEndpoint);
  HTTPRouter.RegisterRoute('/sign-up', rmPost, @registerEndpoint);
  HTTPRouter.RegisterRoute('/forgot-password', rmGet, @forgotPasswordEndpoint);
  HTTPRouter.RegisterRoute('/forgot-password', rmPost, @forgotPasswordEndpoint);
  HTTPRouter.RegisterRoute('/logout', rmGet, @logoutEndpoint);
  
  // Board routes
  HTTPRouter.RegisterRoute('/allboards', rmGet, @allBoardsEndpoint);
  HTTPRouter.RegisterRoute('/public', rmGet, @publicEndpoint);
  HTTPRouter.RegisterRoute('/board', rmGet, @boardEndpoint);
  HTTPRouter.RegisterRoute('/b/:id/:slug', rmGet, @boardEndpoint);
  HTTPRouter.RegisterRoute('/b/:boardId/:slug/:cardId', rmGet, @cardEndpoint);
  
  // Card routes
  HTTPRouter.RegisterRoute('/card', rmGet, @cardEndpoint);
  HTTPRouter.RegisterRoute('/my-cards', rmGet, @myCardsEndpoint);
  HTTPRouter.RegisterRoute('/due-cards', rmGet, @dueCardsEndpoint);
  
  // Utility routes
  HTTPRouter.RegisterRoute('/shortcuts', rmGet, @shortcutsEndpoint);
  HTTPRouter.RegisterRoute('/templates', rmGet, @templatesEndpoint);
  HTTPRouter.RegisterRoute('/b/templates', rmGet, @templatesEndpoint);
  HTTPRouter.RegisterRoute('/accessibility', rmGet, @accessibilityEndpoint);
  
  // Search and import
  HTTPRouter.RegisterRoute('/global-search', rmGet, @globalSearchEndpoint);
  HTTPRouter.RegisterRoute('/import', rmGet, @importEndpoint);
  HTTPRouter.RegisterRoute('/import/:source', rmGet, @importEndpoint);
  
  // Admin and settings routes
  HTTPRouter.RegisterRoute('/setting', rmGet, @adminSettingEndpoint);
  HTTPRouter.RegisterRoute('/information', rmGet, @informationEndpoint);
  HTTPRouter.RegisterRoute('/people', rmGet, @peopleEndpoint);
  HTTPRouter.RegisterRoute('/admin-reports', rmGet, @adminReportsEndpoint);
  HTTPRouter.RegisterRoute('/attachments', rmGet, @attachmentsEndpoint);
  HTTPRouter.RegisterRoute('/translation', rmGet, @translationEndpoint);
  HTTPRouter.RegisterRoute('/broken-cards', rmGet, @brokenCardsEndpoint);
  
  // User routes
  HTTPRouter.RegisterRoute('/usersettings', rmGet, @userSettingsEndpoint);
  HTTPRouter.RegisterRoute('/notifications', rmGet, @notificationsEndpoint);
  
  // Calendar and other features
  HTTPRouter.RegisterRoute('/calendar', rmGet, @calendarEndpoint);
  
  // Upload functionality
  HTTPRouter.RegisterRoute('/upload', rmGet, @uploadEndpoint);
  HTTPRouter.RegisterRoute('/upload', rmPost, @uploadEndpoint);
  
  // API routes
  HTTPRouter.RegisterRoute('/json', rmGet, @jsonEndpoint);
  HTTPRouter.RegisterRoute('/screeninfo', rmPost, @screenInfoEndpoint);
  
  // Legacy redirects
  HTTPRouter.RegisterRoute('/boards', rmGet, @allBoardsEndpoint);
  HTTPRouter.RegisterRoute('/boards/:id/:slug', rmGet, @boardEndpoint);
  HTTPRouter.RegisterRoute('/boards/:id/:slug/:cardId', rmGet, @cardEndpoint);
  
  // Static file server for files in the public directory
  HTTPRouter.RegisterRoute('/*', rmGet, @serveStaticFileEndpoint);
  
  // Fallback for any remaining requests
  HTTPRouter.RegisterRoute('/catchall', rmAll, @catchallEndpoint, true);
end;

// All pages endpoint
procedure allPagesEndpoint(aRequest: TRequest; aResponse: TResponse);
var
  MongoUrlValue: string;
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
    
    // JavaScript detection
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
    Add('</script>');
    
    // Navigation links
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
    Add('<p><b>Search</b>: <a href="global-search">Global Search</a></p>');
    Add('<p><b>Accessibility</b>: <a href="accessibility">Accessibility Features</a></p>');
    Add('<p><b>Admin</b>: <a href="broken-cards">Broken Cards</a>');
    Add(' - <a href="setting">Setting</a>');
    Add(' - <a href="information">Information</a>');
    Add(' - <a href="people">People</a>');
    Add(' - <a href="admin-reports">Admin Reports</a>');
    Add('- <a href="attachments">Attachments</a>');
    Add(' - <a href="translation">Translation</a></p>');
    Add('</body></html>');
  end;
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Login endpoint
procedure loginEndpoint(aRequest: TRequest; aResponse: TResponse);
var
  Username, Password: string;
  LoginMessage: string;
begin
  if aRequest.Method = 'POST' then
  begin
    // Handle form submission
    Username := aRequest.ContentFields.Values['username'];
    Password := aRequest.ContentFields.Values['password'];
    
    // Basic validation
    if (Username = '') or (Password = '') then
    begin
      LoginMessage := '<p style="color: red;">Username and password are required.</p>';
    end
    else
    begin
      // Authenticate user against database
      if AuthenticateUser(Username, Password) then
      begin
        // Login successful, redirect to All Boards page
        CurrentUser := Username;
        aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                            '<html><head><title>Login Successful - WeKan</title>' + LineEnding +
                            '<meta http-equiv="refresh" content="2;url=allboards">' + LineEnding +
                            '</head><body>' + LineEnding +
                            '<h1>Login Successful!</h1>' + LineEnding +
                            '<p>Welcome back, ' + Username + '!</p>' + LineEnding +
                            '<p>You will be redirected to the All Boards page in 2 seconds...</p>' + LineEnding +
                            '<p><a href="allboards">Click here if you are not redirected automatically</a></p>' + LineEnding +
                            '<p><a href=".">Back to Home</a></p>' + LineEnding +
                            '</body></html>';
        aResponse.Code := 200;
        aResponse.ContentType := 'text/html';
        aResponse.ContentLength := Length(aResponse.Content);
        aResponse.SendContent;
        Exit; // Exit early for successful login
      end
      else
      begin
        LoginMessage := '<p style="color: red;">Invalid username or password. Please try again.</p>';
      end;
    end;
    
    // If validation failed or authentication failed, show the form again with error message
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
      Add('        <form role="form"');
      Add('                 novalidate');
      Add('                 action="sign-in"');
      Add('                 method="POST">');
      Add('          <div class="form-group">');
      Add('            <label for="username">Username or Email</label>');
      Add('            <input type="text"');
      Add('                   class="form-control"');
      Add('                   id="username"');
      Add('                   name="username"');
      Add('                   value="' + Username + '"');
      Add('                   placeholder="Enter username or email"');
      Add('                   required>');
      Add('          </div>');
      Add('          <div class="form-group">');
      Add('            <label for="password">Password</label>');
      Add('            <input type="password"');
      Add('                   class="form-control"');
      Add('                   id="password"');
      Add('                   name="password"');
      Add('                   placeholder="Enter password"');
      Add('                   required>');
      Add('          </div>');
      Add('          <button type="submit"');
      Add('                  class="btn btn-primary btn-block">');
      Add('            Sign In');
      Add('          </button>');
      Add('        </form>');
      Add(LoginMessage);
      Add('        <p><a href="sign-up">Need an account? Sign up</a></p>');
      Add('        <p><a href="forgot-password">Forgot your password?</a></p>');
      Add('        <p><a href=".">Back to Home</a></p>');
      Add('      </td>');
      Add('    </tr>');
      Add('  </table>');
      Add('</center>');
    end;
  end
  else
  begin
    // Show login form (GET request)
    aResponse.Content := '';
    with aResponse.Contents do
    begin
      Add('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">');
      Add('<html>');
      Add('  <head>');
      Add('    <title>WeKan</title>');
      Add('    <style type="text/css">');
      Add('      body   { font-family: arial, sans-serif; font-size: 1.2em; }');
      Add('      h1     { font-size: 2.5em; }');
      Add('      input  { font-size: 1em; padding: 0.5em; }');
      Add('      button { font-size: 1.2em; }');
      Add('    </style>');
      Add('</head>');
      Add('<body bgcolor="#ffffff">');
      Add('  <center>');
      Add('    <h1>WeKan</h1>');
      Add('    <form role="form" novalidate action="sign-in" method="POST">');
      Add('      <label for="username">Username or Email</label><br>');
      Add('      <input type="text" id="username" name="username" ');
      Add('             placeholder="Enter username or email" required><br><br>');
      Add('      <label for="password">Password</label><br>');
      Add('      <input type="password" id="password" name="password" ');
      Add('             placeholder="Enter password" required><br><br>');
      Add('      <button type="submit" class="btn btn-primary btn-block">Sign In</button><br><br>');
      Add('    </form>');
      Add('    <p><a href="sign-up">Need an account? Sign up</a></p>');
      Add('    <p><a href="forgot-password">Forgot your password?</a></p>');
      Add('  </center>');
      Add('</body>');
      Add('</html>')
    end;
  end;
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Register endpoint
procedure registerEndpoint(aRequest: TRequest; aResponse: TResponse);
var
  Username, Email, Password, ConfirmPassword: string;
  SuccessMessage: string;
begin
  if aRequest.Method = 'POST' then
  begin
    // Handle form submission
    Username := aRequest.ContentFields.Values['username'];
    Email := aRequest.ContentFields.Values['email'];
    Password := aRequest.ContentFields.Values['password'];
    ConfirmPassword := aRequest.ContentFields.Values['confirm_password'];
    
    // Basic validation
    if (Username = '') or (Email = '') or (Password = '') then
    begin
      SuccessMessage := '<p style="color: red;">All fields are required.</p>';
    end
    else if Password <> ConfirmPassword then
    begin
      SuccessMessage := '<p style="color: red;">Passwords do not match.</p>';
    end
    else if Length(Password) < 6 then
    begin
      SuccessMessage := '<p style="color: red;">Password must be at least 6 characters long.</p>';
    end
    else if UserExists(Username) then
    begin
      SuccessMessage := '<p style="color: red;">Username already exists. Please choose a different username.</p>';
    end
    else
    begin
      // Register user in database
      if RegisterUser(Username, Email, Password) then
      begin
        // Set current user and redirect to All Boards page
        CurrentUser := Username;
        aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                            '<html><head><title>Registration Successful - WeKan</title>' + LineEnding +
                            '<meta http-equiv="refresh" content="2;url=allboards">' + LineEnding +
                            '</head><body>' + LineEnding +
                            '<h1>Registration Successful!</h1>' + LineEnding +
                            '<p>Welcome, ' + Username + '! Your account has been created successfully.</p>' + LineEnding +
                            '<p>You will be redirected to the All Boards page in 2 seconds...</p>' + LineEnding +
                            '<p><a href="allboards">Click here if you are not redirected automatically</a></p>' + LineEnding +
                            '<p><a href=".">Back to Home</a></p>' + LineEnding +
                            '</body></html>';
        aResponse.Code := 200;
        aResponse.ContentType := 'text/html';
        aResponse.ContentLength := Length(aResponse.Content);
        aResponse.SendContent;
        Exit; // Exit early for successful registration
      end
      else
      begin
        SuccessMessage := '<p style="color: red;">Failed to create account. Please try again.</p>';
      end;
    end;
    
    // If validation failed or registration failed, show the form again with error message
    aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                        '<html><head><title>Sign Up - WeKan</title></head><body>' + LineEnding +
                        '<h1>Sign Up</h1>' + LineEnding +
                        SuccessMessage + LineEnding +
                        '<form action="sign-up" method="POST">' + LineEnding +
                        '  <label for="username">Username:</label><br>' + LineEnding +
                        '  <input type="text" id="username" name="username" value="' + Username + '" required><br><br>' + LineEnding +
                        '  <label for="email">Email:</label><br>' + LineEnding +
                        '  <input type="email" id="email" name="email" value="' + Email + '" required><br><br>' + LineEnding +
                        '  <label for="password">Password:</label><br>' + LineEnding +
                        '  <input type="password" id="password" name="password" required><br><br>' + LineEnding +
                        '  <label for="confirm_password">Confirm Password:</label><br>' + LineEnding +
                        '  <input type="password" id="confirm_password" name="confirm_password" required><br><br>' + LineEnding +
                        '  <input type="submit" value="Sign Up">' + LineEnding +
                        '</form>' + LineEnding +
                        '<p><a href="sign-in">Already have an account? Sign in</a></p>' + LineEnding +
                        '<p><a href=".">Back to Home</a></p>' + LineEnding +
                        '</body></html>';
  end
  else
  begin
    // Show registration form (GET request)
    aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                        '<html>' + LineEnding +
                        '  <head>' + LineEnding +
                        '    <title>Sign Up - WeKan</title>' + LineEnding +
                        '    <style type="text/css">' + LineEnding +
                        '      body   { font-family: arial, sans-serif; font-size: 1.2em; }' + LineEnding +
                        '      h1     { font-size: 2.5em; }' + LineEnding +
                        '      input  { font-size: 1em; padding: 0.5em; }' + LineEnding +
                        '      button { font-size: 1.2em; }' + LineEnding +
                        '    </style>' + LineEnding +
                        '</head>' + LineEnding +
                        '<body bgcolor="#ffffff">' + LineEnding +
                        '  <center>' + LineEnding +
                        '    <h1>Sign Up</h1>' + LineEnding +
                        '    <form action="sign-up" method="POST">' + LineEnding +
                        '      <label for="username">Username:</label><br>' + LineEnding +
                        '      <input type="text" id="username" name="username" required><br><br>' + LineEnding +
                        '      <label for="email">Email:</label><br>' + LineEnding +
                        '      <input type="email" id="email" name="email" required><br><br>' + LineEnding +
                        '      <label for="password">Password:</label><br>' + LineEnding +
                        '      <input type="password" id="password" name="password" required><br><br>' + LineEnding +
                        '      <label for="confirm_password">Confirm Password:</label><br>' + LineEnding +
                        '      <input type="password" id="confirm_password" name="confirm_password" required><br><br>' + LineEnding +
                        '      <input type="submit" value="Sign Up">' + LineEnding +
                        '      </form>' + LineEnding +
                        '    <p><a href="sign-in">Already have an account? Sign in</a></p>' + LineEnding +
                        '    <p><a href="forgot-password">Forgot password?</a></p>' + LineEnding +
                        '  </center>' + LineEnding +
                        '</body>' + LineEnding +
                        '</html>';
  end;
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Forgot password endpoint
procedure forgotPasswordEndpoint(aRequest: TRequest; aResponse: TResponse);
var
  Email: string;
  ResetMessage: string;
begin
  if aRequest.Method = 'POST' then
  begin
    // Handle form submission
    Email := aRequest.ContentFields.Values['email'];
    
    // Basic validation
    if Email = '' then
    begin
      ResetMessage := '<p style="color: red;">Email address is required.</p>';
    end
    else if Pos('@', Email) = 0 then
    begin
      ResetMessage := '<p style="color: red;">Please enter a valid email address.</p>';
    end
    else
    begin
      // TODO: Add actual password reset functionality
      ResetMessage := '<p style="color: green;">Password reset instructions have been sent to ' + Email + '.</p>' +
                     '<p>Check your email for further instructions.</p>';
    end;
    
    aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                        '<html><head><title>Forgot Password - WeKan</title></head><body>' + LineEnding +
                        '<h1>Forgot Password</h1>' + LineEnding +
                        ResetMessage + LineEnding +
                        '<p>Enter your email address to reset your password:</p>' + LineEnding +
                        '<form action="forgot-password" method="POST">' + LineEnding +
                        '  <label for="email">Email:</label><br>' + LineEnding +
                        '  <input type="email" id="email" name="email" value="' + Email + '" required><br><br>' + LineEnding +
                        '  <input type="submit" value="Reset Password">' + LineEnding +
                        '</form>' + LineEnding +
                        '<p><a href="sign-in">Back to Sign In</a></p>' + LineEnding +
                        '<p><a href=".">Back to Home</a></p>' + LineEnding +
                        '</body></html>';
  end
  else
  begin
    // Show password reset form (GET request)
    aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                        '<html><head><title>Forgot Password - WeKan</title></head><body>' + LineEnding +
                        '<h1>Forgot Password</h1>' + LineEnding +
                        '<p>Enter your email address to reset your password:</p>' + LineEnding +
                        '<form action="forgot-password" method="POST">' + LineEnding +
                        '  <label for="email">Email:</label><br>' + LineEnding +
                        '  <input type="email" id="email" name="email" required><br><br>' + LineEnding +
                        '  <input type="submit" value="Reset Password">' + LineEnding +
                        '</form>' + LineEnding +
                        '<p><a href="sign-in">Back to Sign In</a></p>' + LineEnding +
                        '<p><a href=".">Back to Home</a></p>' + LineEnding +
                        '</body></html>';
  end;
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Logout endpoint
procedure logoutEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  // Clear current user session
  CurrentUser := '';
  aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                      '<html><head><title>Logged Out - WeKan</title>' + LineEnding +
                      '<meta http-equiv="refresh" content="2;url=.">' + LineEnding +
                      '</head><body>' + LineEnding +
                      '<h1>Logged Out</h1>' + LineEnding +
                      '<p>You have been successfully logged out.</p>' + LineEnding +
                      '<p>You will be redirected to the home page in 2 seconds...</p>' + LineEnding +
                      '<p><a href=".">Click here if you are not redirected automatically</a></p>' + LineEnding +
                      '</body></html>';
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// All boards endpoint
procedure allBoardsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                      '<html><head><title>All Boards - WeKan</title></head><body>' + LineEnding +
                      '<table border="0" cellspacing="0" cellpadding="10" width="100%" bgcolor="#2573a7">' + LineEnding +
                      '  <tr>' + LineEnding +
                      '    <td align="left" valign="middle">' + LineEnding +
                      '      <img src="images/logo-header.png" alt="WeKan" title="WeKan">&nbsp;' + LineEnding +
                      '      <a href="#"><img src="font/arrow/white/home.gif" width="20"><font size="3" color="#FFFFFF" face="arial">All Boards</font></a>&nbsp;' + LineEnding +
                      '    </td>' + LineEnding +
                      '    <td align="right" valign="middle">' + LineEnding;
  
  if CurrentUser <> '' then
  begin
    aResponse.Content := aResponse.Content + '<font size="3" color="#FFFFFF" face="arial">Welcome, ' + CurrentUser + '</font>&nbsp;' + LineEnding +
                        '<a href="usersettings" name="UserSettings"><img src="font/setting/white/cog.gif" width="20" height="20"></a>&nbsp;' + LineEnding +
                        '<a href="notifications" name="Notifications"><img src="font/notification/white/bell.gif" width="20" height="20"></a>&nbsp;' + LineEnding +
                        '<a href="logout"><font size="3" color="#FFFFFF" face="arial">Logout</font></a>';
  end
  else
  begin
    aResponse.Content := aResponse.Content + '<a href="sign-in"><font size="3" color="#FFFFFF" face="arial">Sign In</font></a>&nbsp;' + LineEnding +
                        '<a href="sign-up"><font size="3" color="#FFFFFF" face="arial">Sign Up</font></a>';
  end;
  
  aResponse.Content := aResponse.Content + '    </td>' + LineEnding +
                      '  </tr>' + LineEnding +
                      '  <tr>' + LineEnding +
                      '    <td colspan="2" bgcolor="#2980b9"><font size="5" color="#FFFFFF" face="arial"><b>My Boards</b></font></td>' + LineEnding +
                      '  </tr>' + LineEnding +
                      '</table>' + LineEnding +
                      '<br>' + LineEnding +
                      '<table border="0" padding="0" spacing="0" width="100%">' + LineEnding +
                      '  <tbody>' + LineEnding +
                      '    <tr>' + LineEnding +
                      '      <td width="20" height="20"></td>' + LineEnding +
                      '      <td width="200" height="80" valign="middle" align="left">' + LineEnding +
                      '        <a href="board">' + BoardIcon('Sample Board 1', 1, 'white', 'blue') + '</a>' + LineEnding +
                      '      </td>' + LineEnding +
                      '      <td width="200" height="80" valign="middle" align="left">' + LineEnding +
                      '        <a href="board">' + BoardIcon('Project Board', 2, 'white', 'green') + '</a>' + LineEnding +
                      '      </td>' + LineEnding +
                      '      <td width="200" height="80" valign="middle" align="left">' + LineEnding +
                      '        <a href="board">' + BoardIcon('Team Tasks', 3, 'white', 'orange') + '</a>' + LineEnding +
                      '      </td>' + LineEnding +
                      '      <td width="200" height="80" valign="middle" align="left">' + LineEnding +
                      '        <a href="board">' + BoardIcon('Personal', 4, 'white', 'purple') + '</a>' + LineEnding +
                      '      </td>' + LineEnding +
                      '      <td width="20" height="20"></td>' + LineEnding +
                      '    </tr>' + LineEnding +
                      '    <tr>' + LineEnding +
                      '      <td width="20" height="20"></td>' + LineEnding +
                      '      <td width="200" height="80" valign="middle" align="left">' + LineEnding +
                      '        <a href="board">' + BoardIcon('Marketing', 5, 'white', 'red') + '</a>' + LineEnding +
                      '      </td>' + LineEnding +
                      '      <td width="200" height="80" valign="middle" align="left">' + LineEnding +
                      '        <a href="board">' + BoardIcon('Development', 6, 'white', 'teal') + '</a>' + LineEnding +
                      '      </td>' + LineEnding +
                      '      <td width="200" height="80" valign="middle" align="left">' + LineEnding +
                      '        <a href="board">' + BoardIcon('Design', 7, 'white', 'pink') + '</a>' + LineEnding +
                      '      </td>' + LineEnding +
                      '      <td width="200" height="80" valign="middle" align="left">' + LineEnding +
                      '        <a href="board">' + BoardIcon('Testing', 8, 'white', 'brown') + '</a>' + LineEnding +
                      '      </td>' + LineEnding +
                      '      <td width="20" height="20"></td>' + LineEnding +
                      '    </tr>' + LineEnding +
                      '  </tbody>' + LineEnding +
                      '</table>' + LineEnding +
                      '<br>' + LineEnding +
                      '<p align="center"><a href="board"><font size="3" color="#007bff" face="arial">Create New Board</font></a></p>' + LineEnding +
                      '<p><a href=".">Back to Home</a></p>' + LineEnding +
                      '</body></html>';
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Public boards endpoint
procedure publicEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                      '<html><head><title>Public Boards - WeKan</title></head><body>' + LineEnding +
                      '<h1>Public Boards</h1>' + LineEnding +
                      '<p>Public boards will be displayed here.</p>' + LineEnding +
                      '<p><a href=".">Back to Home</a></p>' + LineEnding +
                      '</body></html>';
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Board endpoint
procedure boardEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                      '<html><head><title>Board - WeKan</title></head><body>' + LineEnding +
                      '<table border="0" cellspacing="0" cellpadding="10" width="100%" bgcolor="#2573a7">' + LineEnding +
                      '  <tr>' + LineEnding +
                      '    <td align="left" valign="middle">' + LineEnding +
                      '      <img src="images/logo-header.png" alt="WeKan" title="WeKan">&nbsp;' + LineEnding +
                      '      <a href="allboards"><img src="font/arrow/white/home.gif" width="20"><font size="3" color="#FFFFFF" face="arial">All Boards</font></a>&nbsp;' + LineEnding +
                      '    </td>' + LineEnding +
                      '    <td align="right" valign="middle">' + LineEnding;
  
  if CurrentUser <> '' then
  begin
    aResponse.Content := aResponse.Content + '<font size="3" color="#FFFFFF" face="arial">Welcome, ' + CurrentUser + '</font>&nbsp;' + LineEnding +
                        '<a href="usersettings" name="UserSettings"><img src="font/setting/white/cog.gif" width="20" height="20"></a>&nbsp;' + LineEnding +
                        '<a href="notifications" name="Notifications"><img src="font/notification/white/bell.gif" width="20" height="20"></a>&nbsp;' + LineEnding +
                        '<a href="logout"><font size="3" color="#FFFFFF" face="arial">Logout</font></a>';
  end
  else
  begin
    aResponse.Content := aResponse.Content + '<a href="sign-in"><font size="3" color="#FFFFFF" face="arial">Sign In</font></a>&nbsp;' + LineEnding +
                        '<a href="sign-up"><font size="3" color="#FFFFFF" face="arial">Sign Up</font></a>';
  end;
  
  aResponse.Content := aResponse.Content + '    </td>' + LineEnding +
                      '  </tr>' + LineEnding +
                      '  <tr>' + LineEnding +
                      '    <td colspan="2" bgcolor="#2980b9"><font size="5" color="#FFFFFF" face="arial"><b>Kanban Board</b></font></td>' + LineEnding +
                      '  </tr>' + LineEnding +
                      '</table>' + LineEnding +
                      '<br>' + LineEnding +

                      '<style>' + LineEnding +
                      '  .swimlane { margin-bottom: 20px; }' + LineEnding +
                      '  .kanban-lists-container {' + LineEnding +
                      '    display: flex;' + LineEnding +
                      '    flex-direction: row;' + LineEnding +
                      '    gap: 20px;' + LineEnding +
                      '    overflow-x: auto;' + LineEnding +
                      '  }' + LineEnding +
                      '  .kanban-list {' + LineEnding +
                      '    min-width: 250px;' + LineEnding +
                      '    background: #f4f4f4;' + LineEnding +
                      '    padding: 10px;' + LineEnding +
                      '    border-radius: 4px;' + LineEnding +
                      '  }' + LineEnding +
                      '  .selected {' + LineEnding +
                      '    outline: 2px solid #0066ff;' + LineEnding +
                      '    box-shadow: 0 0 5px rgba(0,102,255,0.5);' + LineEnding +
                      '  }' + LineEnding +
                      '  .moving {' + LineEnding +
                      '    opacity: 0.7;' + LineEnding +
                      '    background: #e0e0e0;' + LineEnding +
                      '  }' + LineEnding +
                      '  .kanban-card {' + LineEnding +
                      '    background: white;' + LineEnding +
                      '    padding: 8px;' + LineEnding +
                      '    margin: 8px 0;' + LineEnding +
                      '    border-radius: 3px;' + LineEnding +
                      '    box-shadow: 0 1px 3px rgba(0,0,0,0.12);' + LineEnding +
                      '    cursor: pointer;' + LineEnding +
                      '  }' + LineEnding +
                      '  .card-placeholder {' + LineEnding +
                      '    border: 2px dashed #ccc;' + LineEnding +
                      '    background: #f9f9f9;' + LineEnding +
                      '    height: 30px;' + LineEnding +
                      '    margin: 8px 0;' + LineEnding +
                      '  }' + LineEnding +
                      '  .swimlane-placeholder {' + LineEnding +
                      '    border: 2px dashed #ccc;' + LineEnding +
                      '    background: #f9f9f9;' + LineEnding +
                      '    height: 50px;' + LineEnding +
                      '    margin-bottom: 20px;' + LineEnding +
                      '  }' + LineEnding +
                      '  .kanban-list-title {' + LineEnding +
                      '    cursor: move;' + LineEnding +
                      '  }' + LineEnding +
                      '  .add-button {' + LineEnding +
                      '    background: #007bff;' + LineEnding +
                      '    color: white;' + LineEnding +
                      '    border: none;' + LineEnding +
                      '    padding: 8px 16px;' + LineEnding +
                      '    border-radius: 4px;' + LineEnding +
                      '    cursor: pointer;' + LineEnding +
                      '    margin: 10px 0;' + LineEnding +
                      '  }' + LineEnding +
                      '  .add-button:hover {' + LineEnding +
                      '    background: #0056b3;' + LineEnding +
                      '  }' + LineEnding +
                      '</style>' + LineEnding +
                      '<section id="kanban-board" aria-label="Kanban Board" role="region" tabindex="0">' + LineEnding +
                      '  <h1>Accessible Kanban Board</h1>' + LineEnding +
                      '  <div class="swimlane" role="region" aria-label="Swimlane 1" tabindex="0" draggable="true">' + LineEnding +
                      '    <h2>Swimlane 1</h2>' + LineEnding +
                      '    <div class="kanban-lists-container">' + LineEnding +
                      '      <div class="kanban-list" role="list" aria-label="To Do" tabindex="0">' + LineEnding +
                      '        <h3 class="kanban-list-title" draggable="true">To Do</h3>' + LineEnding +
                      '        <div class="kanban-card" role="listitem" tabindex="0">Card 1</div>' + LineEnding +
                      '        <div class="kanban-card" role="listitem" tabindex="0">Card 2</div>' + LineEnding +
                      '        <button class="add-button" onclick="addCard(this)">+ Add Card</button>' + LineEnding +
                      '      </div>' + LineEnding +
                      '      <div class="kanban-list" role="list" aria-label="In Progress" tabindex="0">' + LineEnding +
                      '        <h3 class="kanban-list-title" draggable="true">In Progress</h3>' + LineEnding +
                      '        <div class="kanban-card" role="listitem" tabindex="0">Card A</div>' + LineEnding +
                      '        <div class="kanban-card" role="listitem" tabindex="0">Card B</div>' + LineEnding +
                      '        <button class="add-button" onclick="addCard(this)">+ Add Card</button>' + LineEnding +
                      '      </div>' + LineEnding +
                      '      <div class="kanban-list" role="list" aria-label="Done" tabindex="0">' + LineEnding +
                      '        <h3 class="kanban-list-title" draggable="true">Done</h3>' + LineEnding +
                      '        <div class="kanban-card" role="listitem" tabindex="0">Card 3</div>' + LineEnding +
                      '        <div class="kanban-card" role="listitem" tabindex="0">Card 4</div>' + LineEnding +
                      '        <button class="add-button" onclick="addCard(this)">+ Add Card</button>' + LineEnding +
                      '      </div>' + LineEnding +
                      '    </div>' + LineEnding +
                      '    <button class="add-button" onclick="addList(this)">+ Add List</button>' + LineEnding +
                      '  </div>' + LineEnding +
                      '  <div class="swimlane" role="region" aria-label="Swimlane 2" tabindex="0" draggable="true">' + LineEnding +
                      '    <h2>Swimlane 2</h2>' + LineEnding +
                      '    <div class="kanban-lists-container">' + LineEnding +
                      '      <div class="kanban-list" role="list" aria-label="Backlog" tabindex="0">' + LineEnding +
                      '        <h3 class="kanban-list-title" draggable="true">Backlog</h3>' + LineEnding +
                      '        <div class="kanban-card" role="listitem" tabindex="0">Card 5</div>' + LineEnding +
                      '        <div class="kanban-card" role="listitem" tabindex="0">Card 6</div>' + LineEnding +
                      '        <button class="add-button" onclick="addCard(this)">+ Add Card</button>' + LineEnding +
                      '      </div>' + LineEnding +
                      '      <div class="kanban-list" role="list" aria-label="Review" tabindex="0">' + LineEnding +
                      '        <h3 class="kanban-list-title" draggable="true">Review</h3>' + LineEnding +
                      '        <div class="kanban-card" role="listitem" tabindex="0">Card C</div>' + LineEnding +
                      '        <div class="kanban-card" role="listitem" tabindex="0">Card D</div>' + LineEnding +
                      '        <button class="add-button" onclick="addCard(this)">+ Add Card</button>' + LineEnding +
                      '      </div>' + LineEnding +
                      '    </div>' + LineEnding +
                      '    <button class="add-button" onclick="addList(this)">+ Add List</button>' + LineEnding +
                      '  </div>' + LineEnding +
                      '  <button class="add-button" onclick="addSwimlane()">+ Add Swimlane</button>' + LineEnding +
                      '  <script>' + LineEnding +
                      '    let selectedElement = null;' + LineEnding +
                      '    let isMoving = false;' + LineEnding +
                      '    let sourceList = null;' + LineEnding +
                      '    let placeholder = null;' + LineEnding +
                      '    let swimlanePlaceholder = null;' + LineEnding +
                      '    let cardCounter = 1;' + LineEnding +
                      '    let listCounter = 1;' + LineEnding +
                      '    let swimlaneCounter = 1;' + LineEnding +

                      '    function addCard(button) {' + LineEnding +
                      '      const list = button.parentNode;' + LineEnding +
                      '      const cardTitle = prompt("Enter card title:");' + LineEnding +
                      '      if (cardTitle) {' + LineEnding +
                      '        const newCard = document.createElement("div");' + LineEnding +
                      '        newCard.className = "kanban-card";' + LineEnding +
                      '        newCard.setAttribute("role", "listitem");' + LineEnding +
                      '        newCard.setAttribute("tabindex", "0");' + LineEnding +
                      '        newCard.setAttribute("draggable", "true");' + LineEnding +
                      '        newCard.textContent = cardTitle;' + LineEnding +
                      '        list.insertBefore(newCard, button);' + LineEnding +
                      '        setupCardDrag(newCard);' + LineEnding +
                      '      }' + LineEnding +
                      '    }' + LineEnding +

                      '    function addList(button) {' + LineEnding +
                      '      const swimlane = button.parentNode;' + LineEnding +
                      '      const container = swimlane.querySelector(".kanban-lists-container");' + LineEnding +
                      '      const listTitle = prompt("Enter list title:");' + LineEnding +
                      '      if (listTitle) {' + LineEnding +
                      '        const newList = document.createElement("div");' + LineEnding +
                      '        newList.className = "kanban-list";' + LineEnding +
                      '        newList.setAttribute("role", "list");' + LineEnding +
                      '        newList.setAttribute("aria-label", listTitle);' + LineEnding +
                      '        newList.setAttribute("tabindex", "0");' + LineEnding +
                      '        newList.innerHTML = `' + LineEnding +
                      '          <h3 class="kanban-list-title" draggable="true">${listTitle}</h3>' + LineEnding +
                      '          <button class="add-button" onclick="addCard(this)">+ Add Card</button>' + LineEnding +
                      '        `;' + LineEnding +
                      '        container.appendChild(newList);' + LineEnding +
                      '        setupListDrag(newList);' + LineEnding +
                      '      }' + LineEnding +
                      '    }' + LineEnding +

                      '    function addSwimlane() {' + LineEnding +
                      '      const board = document.getElementById("kanban-board");' + LineEnding +
                      '      const swimlaneTitle = prompt("Enter swimlane title:");' + LineEnding +
                      '      if (swimlaneTitle) {' + LineEnding +
                      '        swimlaneCounter++;' + LineEnding +
                      '        const newSwimlane = document.createElement("div");' + LineEnding +
                      '        newSwimlane.className = "swimlane";' + LineEnding +
                      '        newSwimlane.setAttribute("role", "region");' + LineEnding +
                      '        newSwimlane.setAttribute("aria-label", swimlaneTitle);' + LineEnding +
                      '        newSwimlane.setAttribute("tabindex", "0");' + LineEnding +
                      '        newSwimlane.setAttribute("draggable", "true");' + LineEnding +
                      '        newSwimlane.innerHTML = `' + LineEnding +
                      '          <h2>${swimlaneTitle}</h2>' + LineEnding +
                      '          <div class="kanban-lists-container">' + LineEnding +
                      '            <div class="kanban-list" role="list" aria-label="New List" tabindex="0">' + LineEnding +
                      '              <h3 class="kanban-list-title" draggable="true">New List</h3>' + LineEnding +
                      '              <button class="add-button" onclick="addCard(this)">+ Add Card</button>' + LineEnding +
                      '            </div>' + LineEnding +
                      '          </div>' + LineEnding +
                      '          <button class="add-button" onclick="addList(this)">+ Add List</button>' + LineEnding +
                      '        `;' + LineEnding +
                      '        board.appendChild(newSwimlane);' + LineEnding +
                      '        setupSwimlaneDrag(newSwimlane);' + LineEnding +
                      '        setupListDrag(newSwimlane.querySelector(".kanban-list"));' + LineEnding +
                      '      }' + LineEnding +
                      '    }' + LineEnding +

                      '    function setupCardDrag(card) {' + LineEnding +
                      '      card.addEventListener("dragstart", function(e) {' + LineEnding +
                      '        selectedElement = e.target;' + LineEnding +
                      '        sourceList = selectedElement.closest(".kanban-list");' + LineEnding +
                      '        e.dataTransfer.effectAllowed = "move";' + LineEnding +
                      '        setTimeout(() => selectedElement.classList.add("moving"), 0);' + LineEnding +
                      '      });' + LineEnding +
                      '      card.addEventListener("dragend", function() {' + LineEnding +
                      '        selectedElement.classList.remove("moving");' + LineEnding +
                      '        selectedElement = null;' + LineEnding +
                      '        sourceList = null;' + LineEnding +
                      '      });' + LineEnding +
                      '    }' + LineEnding +

                      '    function setupListDrag(list) {' + LineEnding +
                      '      const title = list.querySelector(".kanban-list-title");' + LineEnding +
                      '      title.addEventListener("dragstart", function(e) {' + LineEnding +
                      '        selectedElement = e.target.closest(".kanban-list");' + LineEnding +
                      '        e.dataTransfer.effectAllowed = "move";' + LineEnding +
                      '        setTimeout(() => selectedElement.classList.add("moving"), 0);' + LineEnding +
                      '      });' + LineEnding +
                      '      title.addEventListener("dragend", function() {' + LineEnding +
                      '        selectedElement.classList.remove("moving");' + LineEnding +
                      '        selectedElement = null;' + LineEnding +
                      '      });' + LineEnding +
                      '    }' + LineEnding +

                      '    function setupSwimlaneDrag(swimlane) {' + LineEnding +
                      '      swimlane.addEventListener("dragstart", function(e) {' + LineEnding +
                      '        selectedElement = e.target;' + LineEnding +
                      '        e.dataTransfer.effectAllowed = "move";' + LineEnding +
                      '        setTimeout(() => selectedElement.classList.add("moving"), 0);' + LineEnding +
                      '      });' + LineEnding +
                      '      swimlane.addEventListener("dragend", function() {' + LineEnding +
                      '        selectedElement.classList.remove("moving");' + LineEnding +
                      '        selectedElement = null;' + LineEnding +
                      '      });' + LineEnding +
                      '    }' + LineEnding +

                      '    // Initialize drag and drop for existing elements' + LineEnding +
                      '    document.querySelectorAll(".kanban-card").forEach(card => {' + LineEnding +
                      '      card.setAttribute("draggable", "true");' + LineEnding +
                      '      setupCardDrag(card);' + LineEnding +
                      '    });' + LineEnding +

                      '    document.querySelectorAll(".kanban-list").forEach(list => {' + LineEnding +
                      '      setupListDrag(list);' + LineEnding +
                      '      list.addEventListener("dragover", function(e) {' + LineEnding +
                      '        e.preventDefault();' + LineEnding +
                      '        e.dataTransfer.dropEffect = "move";' + LineEnding +
                      '        const afterElement = getDragAfterElement(list, e.clientY);' + LineEnding +
                      '        if (afterElement == null) {' + LineEnding +
                      '          list.appendChild(placeholder);' + LineEnding +
                      '        } else {' + LineEnding +
                      '          list.insertBefore(placeholder, afterElement);' + LineEnding +
                      '        }' + LineEnding +
                      '      });' + LineEnding +
                      '      list.addEventListener("drop", function(e) {' + LineEnding +
                      '        e.preventDefault();' + LineEnding +
                      '        if (selectedElement) {' + LineEnding +
                      '          list.insertBefore(selectedElement, placeholder);' + LineEnding +
                      '          announceMove(sourceList, list);' + LineEnding +
                      '        }' + LineEnding +
                      '        placeholder.remove();' + LineEnding +
                      '      });' + LineEnding +
                      '    });' + LineEnding +

                      '    document.querySelectorAll(".swimlane").forEach(swimlane => {' + LineEnding +
                      '      setupSwimlaneDrag(swimlane);' + LineEnding +
                      '    });' + LineEnding +

                      '    function getDragAfterElement(container, y) {' + LineEnding +
                      '      const draggableElements = [...container.querySelectorAll(".kanban-card:not(.moving)")];' + LineEnding +
                      '      return draggableElements.reduce((closest, child) => {' + LineEnding +
                      '        const box = child.getBoundingClientRect();' + LineEnding +
                      '        const offset = y - box.top - box.height / 2;' + LineEnding +
                      '        if (offset < 0 && offset > closest.offset) {' + LineEnding +
                      '          return { offset: offset, element: child };' + LineEnding +
                      '        } else {' + LineEnding +
                      '          return closest;' + LineEnding +
                      '        }' + LineEnding +
                      '      }, { offset: Number.NEGATIVE_INFINITY }).element;' + LineEnding +
                      '    }' + LineEnding +

                      '    function announceMove(from, to) {' + LineEnding +
                      '      const announcement = document.createElement("div");' + LineEnding +
                      '      announcement.setAttribute("role", "alert");' + LineEnding +
                      '      announcement.setAttribute("aria-live", "polite");' + LineEnding +
                      '      announcement.textContent = `Card moved from ${from.querySelector("h3").textContent} to ${to.querySelector("h3").textContent}`;' + LineEnding +
                      '      document.body.appendChild(announcement);' + LineEnding +
                      '      setTimeout(() => announcement.remove(), 1000);' + LineEnding +
                      '    }' + LineEnding +
                      '  </script>' + LineEnding +
                      '</section>' + LineEnding +

                      '<p><a href=".">Back to Home</a></p>' + LineEnding +
                      '</body></html>';
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Card endpoint
procedure cardEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                      '<html><head><title>Card - WeKan</title></head><body>' + LineEnding +
                      '<h1>Card Details</h1>' + LineEnding +
                      '<p>Card details will be displayed here.</p>' + LineEnding +
                      '<p><a href=".">Back to Home</a></p>' + LineEnding +
                      '</body></html>';
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Upload endpoint
procedure uploadEndpoint(aRequest: TRequest; aResponse: TResponse);
const
  UPLOAD_FORM = 
    '<form action="/upload" method="post" enctype="multipart/form-data">' +
    '<input type="file" name="file" required>' +
    '<input type="submit" value="Upload">' +
    '</form>';
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
    // Handle file upload
    aResponse.Content := 'File upload functionality will be implemented here.';
    aResponse.Code := 200;
    aResponse.ContentType := 'text/plain';
    aResponse.ContentLength := Length(aResponse.Content);
    aResponse.SendContent;
  end;
end;

// User settings endpoint
procedure userSettingsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                      '<html><head><title>User Settings - WeKan</title></head><body>' + LineEnding +
                      '<h1>User Settings</h1>' + LineEnding +
                      '<p>User settings will be displayed here.</p>' + LineEnding +
                      '<p><a href=".">Back to Home</a></p>' + LineEnding +
                      '</body></html>';
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Notifications endpoint
procedure notificationsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                      '<html><head><title>Notifications - WeKan</title></head><body>' + LineEnding +
                      '<h1>Notifications</h1>' + LineEnding +
                      '<p>Notifications will be displayed here.</p>' + LineEnding +
                      '<p><a href=".">Back to Home</a></p>' + LineEnding +
                      '</body></html>';
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Shortcuts endpoint
procedure shortcutsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                      '<html><head><title>Shortcuts - WeKan</title></head><body>' + LineEnding +
                      '<h1>Shortcuts</h1>' + LineEnding +
                      '<p>Keyboard shortcuts and quick access will be implemented here.</p>' + LineEnding +
                      '<p><a href=".">Back to Home</a></p>' + LineEnding +
                      '</body></html>';
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Calendar endpoint
procedure calendarEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                      '<html><head><title>Calendar - WeKan</title></head><body>' + LineEnding +
                      '<h1>Calendar</h1>' + LineEnding +
                      '<p>Calendar view will be implemented here.</p>' + LineEnding +
                      '<p><a href=".">Back to Home</a></p>' + LineEnding +
                      '</body></html>';
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Templates endpoint
procedure templatesEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                      '<html><head><title>Templates - WeKan</title></head><body>' + LineEnding +
                      '<h1>Board Templates</h1>' + LineEnding +
                      '<p>Pre-defined board templates will be displayed here.</p>' + LineEnding +
                      '<p><a href=".">Back to Home</a></p>' + LineEnding +
                      '</body></html>';
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// My cards endpoint
procedure myCardsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                      '<html><head><title>My Cards - WeKan</title></head><body>' + LineEnding +
                      '<h1>My Cards</h1>' + LineEnding +
                      '<p>Cards assigned to you will be displayed here.</p>' + LineEnding +
                      '<p><a href=".">Back to Home</a></p>' + LineEnding +
                      '</body></html>';
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Due cards endpoint
procedure dueCardsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                      '<html><head><title>Due Cards - WeKan</title></head><body>' + LineEnding +
                      '<h1>Due Cards</h1>' + LineEnding +
                      '<p>Cards with upcoming due dates will be displayed here.</p>' + LineEnding +
                      '<p><a href=".">Back to Home</a></p>' + LineEnding +
                      '</body></html>';
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Global search endpoint
procedure globalSearchEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                      '<html><head><title>Global Search - WeKan</title></head><body>' + LineEnding +
                      '<h1>Global Search</h1>' + LineEnding +
                      '<form action="search" method="GET">' + LineEnding +
                      '  <input type="text" name="q" size="50" placeholder="Search for cards, boards, users...">' + LineEnding +
                      '  <input type="submit" value="Search">' + LineEnding +
                      '</form>' + LineEnding +
                      '<p><a href=".">Back to Home</a></p>' + LineEnding +
                      '</body></html>';
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Broken cards endpoint
procedure brokenCardsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                      '<html><head><title>Broken Cards - WeKan</title></head><body>' + LineEnding +
                      '<h1>Broken Cards</h1>' + LineEnding +
                      '<p>Cards with issues will be displayed here.</p>' + LineEnding +
                      '<p><a href=".">Back to Home</a></p>' + LineEnding +
                      '</body></html>';
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Accessibility endpoint
procedure accessibilityEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                      '<html><head><title>Accessibility - WeKan</title></head><body>' + LineEnding +
                      '<h1>Accessibility Features</h1>' + LineEnding +
                      '<p>WeKan is designed to be accessible to all users, including those using screen readers and assistive technologies.</p>' + LineEnding +
                      '<h2>Keyboard Navigation</h2>' + LineEnding +
                      '<ul>' + LineEnding +
                      '  <li>Use Tab key to navigate between elements</li>' + LineEnding +
                      '  <li>Use Enter or Space to activate buttons and links</li>' + LineEnding +
                      '  <li>Use Arrow keys to navigate within lists and cards</li>' + LineEnding +
                      '</ul>' + LineEnding +
                      '<h2>Screen Reader Support</h2>' + LineEnding +
                      '<ul>' + LineEnding +
                      '  <li>All images have descriptive alt text</li>' + LineEnding +
                      '  <li>Proper heading structure for navigation</li>' + LineEnding +
                      '  <li>Semantic HTML elements for better understanding</li>' + LineEnding +
                      '</ul>' + LineEnding +
                      '<h2>High Contrast</h2>' + LineEnding +
                      '<p>WeKan supports high contrast modes and can be used with browser accessibility features.</p>' + LineEnding +
                      '<p><a href=".">Back to Home</a></p>' + LineEnding +
                      '</body></html>';
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Import endpoint
procedure importEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                      '<html><head><title>Import - WeKan</title></head><body>' + LineEnding +
                      '<h1>Import Data</h1>' + LineEnding +
                      '<p>Import data from other sources will be implemented here.</p>' + LineEnding +
                      '<p><a href=".">Back to Home</a></p>' + LineEnding +
                      '</body></html>';
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Admin setting endpoint
procedure adminSettingEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                      '<html><head><title>Admin Settings - WeKan</title></head><body>' + LineEnding +
                      '<h1>Admin Settings</h1>' + LineEnding +
                      '<p>Administrative settings will be implemented here.</p>' + LineEnding +
                      '<p><a href=".">Back to Home</a></p>' + LineEnding +
                      '</body></html>';
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Information endpoint
procedure informationEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                      '<html><head><title>Information - WeKan</title></head><body>' + LineEnding +
                      '<h1>System Information</h1>' + LineEnding +
                      '<p>System information and status will be displayed here.</p>' + LineEnding +
                      '<p><a href=".">Back to Home</a></p>' + LineEnding +
                      '</body></html>';
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// People endpoint
procedure peopleEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                      '<html><head><title>People - WeKan</title></head><body>' + LineEnding +
                      '<h1>People Management</h1>' + LineEnding +
                      '<p>User management interface will be implemented here.</p>' + LineEnding +
                      '<p><a href=".">Back to Home</a></p>' + LineEnding +
                      '</body></html>';
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Admin reports endpoint
procedure adminReportsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                      '<html><head><title>Admin Reports - WeKan</title></head><body>' + LineEnding +
                      '<h1>Admin Reports</h1>' + LineEnding +
                      '<p>Administrative reports will be implemented here.</p>' + LineEnding +
                      '<p><a href=".">Back to Home</a></p>' + LineEnding +
                      '</body></html>';
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Translation endpoint
procedure translationEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                      '<html><head><title>Translation - WeKan</title></head><body>' + LineEnding +
                      '<h1>Translation</h1>' + LineEnding +
                      '<p>Translation management interface will be implemented here.</p>' + LineEnding +
                      '<p><a href=".">Back to Home</a></p>' + LineEnding +
                      '</body></html>';
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Attachments endpoint
procedure attachmentsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
                      '<html><head><title>Attachments - WeKan</title></head><body>' + LineEnding +
                      '<h1>Attachments Management</h1>' + LineEnding +
                      '<p>File attachments management interface will be implemented here.</p>' + LineEnding +
                      '<p><a href=".">Back to Home</a></p>' + LineEnding +
                      '</body></html>';
  aResponse.Code := 200;
  aResponse.ContentType := 'text/html';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// JSON endpoint
procedure jsonEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '{"status": "ok", "message": "JSON API endpoint"}';
  aResponse.Code := 200;
  aResponse.ContentType := 'application/json';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

// Screen info endpoint
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

// Serve static file endpoint
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

// Catchall endpoint
procedure catchallEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := 'This endpoint is not available';
  aResponse.Code := 404;
  aResponse.ContentType := 'text/plain';
  aResponse.ContentLength := Length(aResponse.Content);
  aResponse.SendContent;
end;

end.