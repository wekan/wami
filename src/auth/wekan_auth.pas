unit wekan_auth;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

// User management functions
function InitializeUserStorage: Boolean;
function RegisterUser(const Username, Email, Password: string): Boolean;
function AuthenticateUser(const Username, Password: string): Boolean;
function UserExists(const Username: string): Boolean;

implementation

// Initialize user storage
function InitializeUserStorage: Boolean;
begin
  // Create users directory if it doesn't exist
  if not DirectoryExists('wekandata') then
  begin
    CreateDir('wekandata');
    Writeln('Created wekandata directory');
  end;

  // Create users.csv file if it doesn't exist
  if not FileExists('wekandata/users.csv') then
  begin
    with TStringList.Create do
    try
      Add('admin:admin:admin@example.com:false'); // Default admin user
      SaveToFile('wekandata/users.csv');
      Writeln('Created users file with default admin account');
    finally
      Free;
    end;
  end;

  Result := True;
  Writeln('File-based user storage initialized successfully');
end;

// Register a new user
function RegisterUser(const Username, Email, Password: string): Boolean;
var
  UsersFile: TStringList;
  UserLine: string;
  i: Integer;
begin
  Result := False;
  try
    UsersFile := TStringList.Create;
    try
      if FileExists('wekandata/users.csv') then
        UsersFile.LoadFromFile('wekandata/users.csv');
      
      // Check if user already exists
      for i := 0 to UsersFile.Count - 1 do
      begin
        if Pos(Username + ':', UsersFile[i]) = 1 then
        begin
          Writeln('User already exists: ' + Username);
          Exit;
        end;
      end;
      
      // Add new user (format: username:password:email:isAdmin)
      UserLine := Username + ':' + Password + ':' + Email + ':false';
      UsersFile.Add(UserLine);
      UsersFile.SaveToFile('wekandata/users.csv');
      Writeln('User registered successfully: ' + Username);
      Result := True;
    finally
      UsersFile.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln('Error registering user: ' + E.Message);
      Result := False;
    end;
  end;
end;

// Authenticate a user
function AuthenticateUser(const Username, Password: string): Boolean;
var
  UsersFile: TStringList;
  UserLine: string;
  i: Integer;
  Parts: TStringArray;
begin
  Result := False;
  try
    UsersFile := TStringList.Create;
    try
      if FileExists('wekandata/users.csv') then
        UsersFile.LoadFromFile('wekandata/users.csv');
      
      // Check each user line
      for i := 0 to UsersFile.Count - 1 do
      begin
        UserLine := UsersFile[i];
        if Pos(Username + ':', UserLine) = 1 then
        begin
          // Parse user line (format: username:password:email:isAdmin)
          Parts := UserLine.Split([':']);
          if (Length(Parts) >= 2) and (Parts[1] = Password) then
          begin
            Result := True;
            Writeln('User authenticated successfully: ' + Username);
            Exit;
          end;
        end;
      end;
      Writeln('Authentication failed for: ' + Username);
    finally
      UsersFile.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln('Error authenticating user: ' + E.Message);
      Result := False;
    end;
  end;
end;

// Check if user exists
function UserExists(const Username: string): Boolean;
var
  UsersFile: TStringList;
  i: Integer;
begin
  Result := False;
  try
    UsersFile := TStringList.Create;
    try
      if FileExists('wekandata/users.csv') then
        UsersFile.LoadFromFile('wekandata/users.csv');
      
      // Check if user exists
      for i := 0 to UsersFile.Count - 1 do
      begin
        if Pos(Username + ':', UsersFile[i]) = 1 then
        begin
          Result := True;
          Writeln('User already exists: ' + Username);
          Exit;
        end;
      end;
    finally
      UsersFile.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln('Error checking if user exists: ' + E.Message);
    end;
  end;
end;

end.