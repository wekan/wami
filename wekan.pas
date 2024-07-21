program wekan;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, fphttpapp, HTTPDefs, httproute;

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
  aResponse.Content := '';
  with aResponse.Contents do
  begin
    Add('<h2>WeKan</h2>');
    Add('<p><a href="/">All Pages</a></p>');
    Add('<p><b>Login</b>: <a href="/sign-in">Sign In</a>');
    Add(' - <a href="/sign-up">Sign Up</a>');
    Add(' - <a href="/forgot-password">Forgot Password</a></p>');
    Add('<p><b>Boards</b>: <a href="/allboards">All Boards</a>');
    Add(' - <a href="/public">Public</a> - <a href="/board">Board</a>');
    Add(' - <a href="/shortcuts">Shortcuts</a>');
    Add(' - <a href="/templates">Templates</a></p>');
    Add('<p><b>Cards</b>: <a href="/my-cards">My Cards</a>');
    Add(' - <a href="/due-cards">Due Cards</a>');
    Add(' - <a href="/import">Import</a></p>');
    Add('<p><b>Search</b>: <a href="/global-search">Global Search</a>');
    Add('<p><b>Admin</b>: <a href="/broken-cards">Broken Cards</a>');
    Add(' - <a href="/setting">Setting</a>');
    Add(' - <a href="/information">Information</a>');
    Add(' - <a href="/people">People</a>');
    Add(' - <a href="/admin-reports">Admin Reports</a>');
    Add('- <a href="/attachments">Attachments</a>');
    Add(' - <a href="/translation">Translation</a></p>');
  end;
  aResponse.Code:=200;
  aResponse.ContentType:='text/html';
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.SendContent;
end;

procedure loginEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content := '';
  with aResponse.Contents do
  begin
    Add('<p>&nbsp;&nbsp;&nbsp;</p>');
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

procedure allBoardsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content:='All Boards';
  aResponse.Code:=200;
  aResponse.ContentType:='text/plain';
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
  aResponse.Content:='Board';
  aResponse.Code:=200;
  aResponse.ContentType:='text/plain';
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

procedure attachmentsEndpoint(aRequest: TRequest; aResponse: TResponse);
begin
  aResponse.Content:='Attachments';
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
  Application.Port:=5000;
  HTTPRouter.RegisterRoute('/', rmGet, @allPagesEndpoint);
  HTTPRouter.RegisterRoute('/sign-in', rmGet, @loginEndpoint);
  HTTPRouter.RegisterRoute('/sign-up', rmGet, @registerEndpoint);
  HTTPRouter.RegisterRoute('/forgot-password', rmGet, @forgotPasswordEndpoint);
  HTTPRouter.RegisterRoute('/allboards', rmGet, @allBoardsEndpoint);
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
  HTTPRouter.RegisterRoute('/attachments', rmGet, @attachmentsEndpoint);
  HTTPRouter.RegisterRoute('/translation', rmGet, @translationEndpoint);
  HTTPRouter.RegisterRoute('/json', rmGet, @jsonEndpoint);
  HTTPRouter.RegisterRoute('/catchall', rmAll, @catchallEndpoint, true);
  Application.Threaded:=false;
  Application.Initialize;
  Writeln('Server is ready at localhost:' + IntToStr(Application.Port));
  Application.Run;
end.
