## Wami: Scale Up

- This is only after scale down version, if it can not scale up enough with minimal tricks
- Caddy 2 TLS as reverse proxy at front of FreePascal/SQLite
- Alternative web frameworks:
  - mORMot2: https://github.com/synopse/mORMot2
  - (or Brook framework)
- For 30k users could use 500 MB - 1.5 GB RAM
- Steady latency at high load
- High error resistance with strong typing
- Multi-threading: Readers and writes can be separated to their own pools
- Pascal TList and TStack structures are very memory efficient. 30k updates takes just couple MB of RAM.
- SQLite settings:
  - `PRAGMA journal_mode = WAL;` – Makes possible simultaneous reads and writes.
  - `PRAGMA synchronous = NORMAL;` – Speeds up writes noticeably (safe at WAL-mode).
  - `PRAGMA cache_size = -64000;` – Reserves about 64 MB RAM for SQLite cache.

#### Thread-safe Queue

This FreePascal Batch Queue could more scalable than Cosmopolitan Redbean
that has isolated CGI like separate writes that would require retry logic when SQLite Busy,
although I have not tested it. 

```
uses
  Classes, SysUtils, syncobjs, mORMot;

type
  TCardUpdate = record
    CardId: Integer;
    NewTitle: string;
  end;

  // Thread-safe Queue for card updates
  TUpdateQueue = class
  private
    FList: TList;
    FLock: TCriticalSection;
  public
    constructor Create;
    procedure Push(AUpdate: TCardUpdate);
    function PopAll: TList;
  end;

// Background thread, that writes changes to SQLite as one big transaction
procedure TDatabaseWriterThread.Execute;
var
  Updates: TList;
  i: Integer;
begin
  while not Terminated do
  begin
    Sleep(100); // Collect changes for 100ms (Batching)
    
    Updates := GUpdateQueue.PopAll;
    if Updates.Count > 0 then
    begin
      // Yksi transaktio voi sisältää tuhansia päivityksiä
      Database.BeginTransaction;
      try
        for i := 0 to Updates.Count - 1 do
          UpdateCardInDB(TCardUpdate(Updates[i]^));
        Database.Commit;
      except
        Database.Rollback;
      end;
    end;
  end;
end;
```

#### WebSocket + PubSub with Pascal

- Receive: WebSocket-server receives JSON-message and pushes it to write queue for SQLite
- Send Broadcast: WebSocket-server sends message info about change immediately to all other active users directly from RAM memory
- This user for example mORMot 2 or sgcWebSockets -library:

```
procedure TMyWebSocketHandler.OnMessage(Connection: TWebSocketConnection; const Msg: string);
var
  Update: TCardUpdate;
begin
  // 1. Parse JSON (for example: {"card_id": 123, "column": "done"})
  Update := ParseJSONToCardUpdate(Msg);

  // 2. Add ti SQLte-Queue (write to disk at background)
  GUpdateQueue.Push(Update);

  // 3. Realtime sending to others (Broadcast)
  // This happens at RAM-memory, so it is lightning fast (about 1-2 ms)
  GWebsocketServer.Broadcast(Msg, Connection); 
end;
```

#### Why this is faster than Cosmopolitan Redbean or Phoenix?

- Does not require SQL query: Pascal server does not ask anything from SQLite to send message to others. It only forwards message.
- Thread level control: You can limit amount of WebSockets, so that 30k messages does not make server stuck.
  - Compiled Pascal memory control is so tight, so that every WebSocket-connection spends only tiny fraction of what is would spend when using scripting languages. 
- Zero-copy JSON: Libraries like mORMot 2 can handle JSON data without making several new copies of data, that saves CPU at majority of message traffic.

#### RAM-optimization for 30k connections

30,000 WebSocket connections is a lot. Here are some tips for the Pascal environment:

- Buffer sizes: Reduce the WebSocket read and write buffers (e.g. 4 KB per connection). If the buffer is 64 KB by default, 30k users would already take up 1.9 GB just for buffers.
- Use epoll (Linux): Make sure your Pascal server uses the Linux epoll interface (mORMot does this automatically). It is the only way to efficiently manage 30k connections without CPU usage reaching 100% just from monitoring connections.

#### Summary of the final "Stack"

- Caddy 2: Acts as an SSL terminal and filter (Reverse Proxy).
- FreePascal (mORMot 2) Binary: Handles WebSockets and application logic.
- SQLite (WAL-mode): Stores data to disk in optimized batches (Batching).

This is probably the fastest and most stable way to run a 30k user real-time application on a single, affordable server.
