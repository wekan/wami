unit wekan_utils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

// Browser detection
function WebBrowserName(const UserAgent: string): String;

// HTML generation functions
function BoardIcon(BoardTitle: String; TabIndex: Integer; Color: String; BackgroundColor: String): String;
function DrawLine(X1: Integer; Y1: Integer; X2: Integer; Y2: Integer; Width: Integer; Height: Integer; Color: String; StrokeWidth: String): String;

implementation

// Browser detection function
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
    end;
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
    end;
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

// Board icon generation
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

// Draw line function
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

end.