program wekan;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  SysUtils, fphttpapp, HTTPDefs, httproute, Classes,
  wekan_web, wekan_auth, wekan_utils;

const
  VERSION = '1.0.0';
  PORT = 5500;

var
  MongoUrlValue: string;
  CurrentUser: string; // Store current logged in user

// Initialize application
procedure InitializeApplication;
begin
  WriteLn('Starting WeKan WAMI v' + VERSION);
  WriteLn('FreePascal Web Application');
  
  // Create necessary directories
  if not DirectoryExists('config') then
    CreateDir('config');
  if not DirectoryExists('logs') then
    CreateDir('logs');
  if not DirectoryExists('uploads') then
    CreateDir('uploads');
  if not DirectoryExists('backups') then
    CreateDir('backups');
  if not DirectoryExists('public') then
    CreateDir('public');
  if not DirectoryExists('wekandata') then
    CreateDir('wekandata');
  
  WriteLn('Directories created successfully');
  WriteLn('Configuration directory: config/');
  WriteLn('Logs directory: logs/');
  WriteLn('Uploads directory: uploads/');
  WriteLn('Backups directory: backups/');
  WriteLn('Public directory: public/');
  WriteLn('User data directory: wekandata/');
  
  // Initialize user storage
  if not InitializeUserStorage then
  begin
    WriteLn('Failed to initialize user storage. Exiting.');
    Exit;
  end;
end;

// Cleanup application
procedure CleanupApplication;
begin
  WriteLn('Shutting down WeKan WAMI...');
  WriteLn('WeKan WAMI stopped');
end;

// Register all routes
procedure RegisterRoutes;
begin
  WriteLn('Registering routes...');
  
  // Register all WeKan routes using the web module
  RegisterWeKanRoutes;
  
  WriteLn('Routes registered successfully');
end;

// Main application entry point
begin
  try
    InitializeApplication;
    RegisterRoutes;
    
    // Set up HTTP server
    Application.Port := PORT;
    Application.Threaded := False;
    Application.Initialize;
    
    WriteLn('WeKan WAMI server starting on port ' + IntToStr(PORT));
    WriteLn('Open your browser and go to: http://localhost:' + IntToStr(PORT));
    WriteLn('Static files are served from the public/ directory');
    WriteLn('User storage initialized successfully');
    WriteLn('Press Ctrl+C to stop the server');
    
    Application.Run;
    
  except
    on E: Exception do
    begin
      WriteLn('Error: ' + E.Message);
    end;
  end;
  
  CleanupApplication;
end.
