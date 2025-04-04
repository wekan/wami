program wekan;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  SysUtils, fphttpapp, HTTPDefs, httproute, Classes; // Add Classes unit

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
    Add('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">');
    Add('<html><head><title>WeKan</title></head><body>');
    Add('<h2>WeKan</h2>');
    Add('<p>' + aRequest.UserAgent + '</p>');
    Add('<p>IPv4 Address: ' + aRequest.RemoteAddr + '</p>');
    Add('<p>IPv6 Address: ' + aRequest.RemoteAddr + '</p>');
    // New code to show screen width and height
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
    Add('  document.getElementById("screenInfo").innerHTML = "Browser window inner size: " + myWidth + " x " + myHeight;');
    Add('}');
    Add('window.onload = showBrowserWindowSize;');
    Add('</script>');

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
    Add('<p><b>Upload</b>: <a href="/upload">Upload</a></p>');
    Add('<p><b>Search</b>: <a href="/global-search">Global Search</a>');
    Add('<p><b>Admin</b>: <a href="/broken-cards">Broken Cards</a>');
    Add(' - <a href="/setting">Setting</a>');
    Add(' - <a href="/information">Information</a>');
    Add(' - <a href="/people">People</a>');
    Add(' - <a href="/admin-reports">Admin Reports</a>');
    Add('- <a href="/attachments">Attachments</a>');
    Add(' - <a href="/translation">Translation</a></p>');
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
