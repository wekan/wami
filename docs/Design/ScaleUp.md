## Wami: Scale Up

- This is only after scale down version, if it can not scale up enough with minimal tricks
  - Use fphttpapp and no-JS for retro platforms, keep minimal code, and add minimal scale up tricks
  - Only add mORMot2 when needing very high scale, because it is very many files
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

mORMot 2 combined with SQLite is one of the most performant combinations you can build. Its limits are usually not in the code, but in the operating system settings and physical memory.

## Analysis of the maximum capacity in different areas

#### 1. Concurrent WebSocket connections

mORMot 2 uses the `epoll` interface on Linux and `I/O Completion Ports (IOCP)` technology on Windows. These are industry standards for massive scalability.

* **Capacity:** It is perfectly possible to maintain **100,000 – 500,000** concurrent WebSocket connections on a single modern server.
* **Limiting factor:** RAM. Each TCP connection allocates a small amount of memory from the operating system for buffers. If the buffers are optimized (e.g. about 4–8 KB per connection), 100,000 connections will take about 800 MB of RAM.

#### 2. Requests Per Second (RPS)

Since FreePascal is compiled to native machine code and mORMot is extremely optimized (it even uses hand-written assembler code in critical places), the read speed is sky-high.

* **Read operations (RAM/Cache):** If the data is in SQLite's cache, mORMot can serve over **100,000 requests per second**.
* **Write operations:** As we calculated earlier, with the previously mentioned **Batching technique** (combining writes into transactions), SQLite can easily handle **50,000+ row updates per second**.

#### 3. SQLite's theoretical limits

While mORMot is fast, SQLite sets its own limits:

* **Database size:** The maximum size is **281 terabytes**. In practice, you will never reach this in a Kanban application.
* **Number of columns:** 32,767 per table.
* **SQL statement length:** 1 billion characters.


## What are the real "bottlenecks"?

When talking about more than 30,000 - 50,000 users, the problems move from software to infrastructure:

1. **Operating system file handles (File Descriptors):**
By default, Linux often only allows 1024 open files/connections. This needs to be increased (e.g. `ulimit -n 1000000`) so that mORMot can accept tens of thousands of WebSockets.
2. **TCP Stack Tuning:**
The TCP settings of the operating system (such as `tcp_fin_timeout` and `tcp_max_syn_backlog`) need to be adjusted so that the "ports" do not run out and new users can log in without delay.
3. **Network bandwidth:**
30,000 users constantly receiving small updates can consume a lot of bandwidth. Even if the messages are small, the number of messages (e.g. 5 messages/sec per user) means 150,000 messages per second.

## Comparison with others (Scalability unit: Single server)

| Technology | Max connections (RAM limited) | RPS (Number) |
| --- | --- | --- |
| **Meteor.js** | 5,000 – 10,000 | Low (Node.js overhead) |
| **Phoenix (Elixir)** | 100,000 – 1,000,000 | Very High |
| **mORMot 2 (FPC)** | **100,000 – 1,000,000** | **Highest (Native Code)** |

**Conclusion:**

mORMot 2 + SQLite can easily handle **30,000 – 100,000 active users** on a single powerful server (e.g. 16-32 GB RAM, 8+ cores). If the number of users goes above 100,000, the biggest challenge is no longer Pascal or SQLite, but network traffic management and possibly a distributed system (multiple servers).

## Can mORMot 2 run at Amiga?

The answer is both fascinating and a bit complicated: **The capability depends on which parts of mORMot 2 you want to use and which Amiga platform you are targeting.**

Free Pascal (FPC) supports Amiga (m68k processor) and new variants (AmigaOS 4, MorphOS, AROS), but mORMot 2 is a very modern and optimized library.

### 1. Traditional Amiga (m68k / AmigaOS 3.x)

**Functionality: Very limited / Not directly.**

* **Processor architecture:** mORMot 2 contains a lot of code optimized for x86 and ARM architectures (such as hand-written Assembler for JSON parsing and cryptography). m68k support is missing in these areas.
* **Operating System Limits:** mORMot 2 requires modern operating system features (such as advanced threading, memory management, and network stacks) that the original AmigaOS 3.x does not natively provide in the same way as Linux or Windows.
* **Memory:** As we calculated earlier, using mORMot 2 to manage 13k or 30k users requires gigabytes of RAM. The memory limits of a traditional Amiga are immediately encountered.

### 2. Modern "Amigas" (AROS, MorphOS, AmigaOS 4)

**Performance: Possible under certain conditions.**

If you are using a modern PPC-based Amiga or an x86-based AROS system, the situation is better:

* **Free Pascal Support:** FPC compiles code for these platforms well.
* **mORMot 2 Core:** The core of the library (like ORM, data structures and basic SQL) is pure Pascal and compiles to most platforms.
* **Network and SQLite:** The biggest challenge is finding compatible and high-performance C libraries (like SQLite and OpenSSL/LTS) that link to these operating systems.

### 3. mORMot 2 "Lightweight" for Amiga?

You could possibly use mORMot on Amiga for **Client** development, but not as a server for 30,000 users:

* You could build a GUI application on Amiga that talks to a mORMot server running on a Linux machine.
* Pascal is a great language for Amiga development, and mORMot's JSON tools could speed up data processing if compiled without x86 optimizations.

### Comparison: Amiga vs. Modern Server

| Feature | Amiga (m68k) | Modern Linux (mORMot 2 target) |
| --- | --- | --- |
| **Threading** | Cooperative multitasking (mainly) | Pre-emptive (kernel level) |
| **Network stack** | Add-ons (e.g. Roadshow) | Native epoll/IOCP |
| **Memory (RAM)** | 2MB - 128MB (typical) | 16GB - 128GB+ |
| **Scores** | Cult classic, great UI | Industrial powerhouse |

### Summary

If you want to run the previously discussed **30,000 user real-time Kanban server**, the Amiga is not the right platform. But if you want to write a nice Kanban client for Amiga in Pascal that retrieves data from a modern mORMot server, it's a fun and doable hobby project.

## What speeds can you get on a MorphOS PPC Mac Mini G4 running a FreePascal server like this under MorphOS?

The MorphOS Mac Mini G4 (1.25 or 1.5 GHz PowerPC G4) is an interesting environment for a FreePascal server. Since your code uses the standard `fphttpapp` unit (which is based on the `TFPHttpApplication` class in your code), it is lightweight and native, but has certain architectural limitations compared to the previously mentioned mORMot 2 library.

Here is an estimate of the speeds and performance on that hardware:

### 1. Raw performance (Requests per second)

The G4 processor is capable for its age, but it only has one core. Since your code serves pages by generating HTML dynamically (a lot of `Add()` calls and string concatenation), the biggest bottleneck is the CPU's string handling and network stack.

* **Estimated speed:** Around **500 – 1,500 requests per second** (RPS) for static or lightweight HTML pages.
* **Compared to modern hardware:** A modern i7 or similar would run the same code at 20,000 – 40,000 RPS. However, the advantage of MorphOS is its incredibly low operating system overhead, leaving almost all the power to the program.

### 2. Write speed (CSV vs SQLite)

Your code currently uses the **`users.csv`** file and the `TStringList.LoadFromFile` method. This is the biggest performance risk:

* **Bottleneck:** Every time someone logs in or registers, the entire CSV file is loaded into RAM, parsed, and saved.
* **With a 30k user load:** If `users.csv` contains 30,000 rows, the file size will be several megabytes. The slow IDE bus of the MorphOS machine (often a 2.5" HDD on the Mac Mini) makes this painfully slow.
* **Recommendation:** If you want speed, move the code to use **SQLite**. It is available for MorphOS and allows you to retrieve a single row without downloading the entire file.

### 3. User experience and browser compatibility

Your code has nicely taken into account old browsers (IBrowse, NetSurf, Dillo).

* **HTML 4.01:** Because you serve pure HTML without a heavy JavaScript framework, the page loads from the Mac Mini G4 to any browser in milliseconds.
* **MorphOS specifics:** If you run a server and use the machine for browsing at the same time (e.g. with Wayfarer), Wayfarer can easily eat up all the power of the G4. In this case, the server should be run in the background with high priority.

### How to optimize this code For MorphOS?

1. **Avoid `with aResponse.Contents do`:** Every `Add()` call causes memory reallocation. It is faster to build the entire HTML page in one big buffer or use the `TStringBuilder` class.
2. **Memory usage:** Mac Mini G4 usually has 512 MB or 1 GB of RAM. Your code is very frugal, so you can easily keep e.g. frequently accessed boards (Boards) directly in RAM in `TStringList` objects.
3. **File system:** Use SFS (SmartFileSystem) or PFS3 file system on MorphOS partition. It speeds up the processing of `wekandata/` folder significantly compared to the default settings.

### Summary

On MorphOS PPC Mac Mini G4 this FreePascal program is **surprisingly fast**. It easily beats a Python or PHP solution running on the same hardware 10x.

**Biggest speed advantage:** Your code produces straight machine code (AltiVec optimizations may not help here, but overall native speed will), so the server feels "lightning fast" as long as you stay away from heavy file operations (CSV) on every request.


## What if it runs on an OrangePi 5 with 16 GB RAM and boots from a 1 TB NVME disk?

Switching from a **Mac Mini G4 to an Orange Pi 5** is like switching from a bicycle to a jet fighter. The **Rockchip RK3588** in the Orange Pi 5 is one of the most powerful ARM chips on the market, and 16 GB RAM and an NVMe disk are a complete game changer.

Here is an analysis of the performance on this hardware:

### 1. Processing speed (CPU)

The RK3588 has 8 cores (4x Cortex-A76 and 4x Cortex-A55). FreePascal can take advantage of these perfectly if you are running a threaded server.

* **Mac Mini G4:** Single core, approx. 1.25–1.5 GHz (old architecture).
* **Orange Pi 5:** Eight cores, up to 2.4 GHz (modern architecture).
* **Speed ​​difference:** Raw computing power is estimated to be **30–50 times higher**. String processing and HTML generation are almost instantaneous.

### 2. I/O speed (NVMe vs. Mac Mini IDE/HDD)

This is the single biggest change.

* **Mac Mini G4:** The old IDE bus limits the speed to about 30–60 MB/s (if using an SSD adapter) or 10–20 MB/s (original HDD).
* **Orange Pi 5 + NVMe:** Read speeds can be up to **2,000 – 3,000 MB/s**.
* **Impact on your code:** Loading and saving `users.csv` is so fast on an NVMe disk that you won't notice any lag, even with thousands of users. Still, with 16 GB of RAM, you can load the entire database into memory at startup.

### 3. Estimated speeds (RPS)

Using your code on an Orange Pi 5:

* **Estimated speed:** Around **15,000 - 25,000 requests per second** (RPS) if you use a threaded HTTP server (`TFPHTTPServer` with `Threaded := True`).
* **30,000 user load:** The Orange Pi 5 can easily handle this number of users, if they don't do heavy searches at the same time. 16 GB of RAM is enough to keep all 30,000 users' data in memory ten times more than necessary.

### How should you change your code for Orange Pi 5?

Even though NVMe is fast, your code is "single-threaded" for writes (saving the CSV file). When you have 30,000 users, here are some optimization suggestions:

#### A. Switch to SQLite (Now it's really easy)

On Orange Pi 5, SQLite and NVMe disk allow you to do thousands of queries per second without even warming up the CPU.

```pascal
// Instead of CSV query:
"SELECT password FROM users WHERE username = 'admin' LIMIT 1;"

```

#### B. Utilize RAM (16 GB is a lot)

You can use `TMemDataset` or `TFPCanonicalJSON` type structures to keep the entire WeKan table structure in memory. Kanban boards for 13k–30k users will probably only take up about 500MB–1GB of RAM.

#### C. Use Linux `epoll` support

The Orange Pi 5 will probably run Debian or Ubuntu. FreePascal's `fphttpapp` is fine, but for massive scaling (over 10k concurrent connections) I recommend switching the server component to the **mORMot 2** or **Brook Freight** libraries, which are optimized to take advantage of the most powerful features of the Linux kernel.

### Comparison summary

| Feature | Mac Mini G4 (MorphOS) | Orange Pi 5 (Linux) |
| --- | --- | --- |
| **Request Handling** | Moderate (Hobby) | **Industrial** |
| **Memory (RAM)** | 512MB - 1GB | **16GB** |
| **Disk speed** | ~30 MB/s | **~2500 MB/s** |
| **Scalability** | <100 concurrent | **>10,000 concurrent** |

**End result:** On the Orange Pi 5, this Pascal application is fully production-ready. It easily outperforms many companies' "real" servers in performance because your code is so close to the hardware (native binary) and the Orange Pi 5's NVMe bus eliminates traditional storage bottlenecks.

## Scenario of SQLite and attachments size

Q: The SQLite file with text is about 500 MB, some fields are JSON. The files are about 2 GB, some of the files are 700 MB PDF. The files do not fit into memory at once.

A: This scenario changes the architecture significantly. When processing hundreds of megabytes of PDF files and a 500 MB SQLite database on the Orange Pi 5, **RAM management** and **I/O strategy** become the most important issues.

The 16 GB RAM of the Orange Pi 5 is large, but it is not worth trying to read 2 GB of large files (especially several 700 MB PDF files) into memory at once, especially if there are many users.

### 1. SQLite and 500 MB of text data

A 500 MB SQLite file is "small" for the Orange Pi 5 and the NVMe disk. It works very fast, as long as you make the following adjustments:

* **Do not load the entire database into memory:** Let SQLite handle the caching. Set `PRAGMA cache_size = -200000;` (this will reserve about 200 MB of RAM for SQLite's page cache).
* **JSON fields:** Since you are using FreePascal, use the `fpjson` unit or mORMot 2's SIMD-optimized JSON reader. If you only read JSON in SQL queries, SQLite 3.38+ includes very fast native JSON functions (e.g. `JSON_EXTRACT`).

### 2. Handling PDF files (700 MB each)

This is the biggest challenge. If only a few dozen out of 30,000 users are trying to download a 700 MB PDF file at the same time, you will run out of 16 GB of RAM if you read the files into `TStringList` or `TMemoryStream` objects.

**Solution: Streaming**
Never read the entire file into memory. Use FreePascal`s `TFileStream` class and serve the file to the browser in chunks.

```pascal
procedure ServeBigPDF(aResponse: TResponse; const FileName: string);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    aResponse.ContentStream := FS; // fphttpapp osaa striimata tämän
    aResponse.ContentType := 'application/pdf';
    aResponse.SendContent; // Lähettää tiedoston puskuroituna ilman että se vie 700MB RAMia
  finally
    // Huom: fphttpapp vapauttaa ContentStreamin automaattisesti
  end;
end;
```

### 3. Performance in an Orange Pi 5 + NVMe environment

Thanks to the NVMe disk, the search speed within the PDF file (e.g. if the browser only requests a specific page or byte range, *Byte Range Requests*) is very fast.

* **Database on NVMe:** Queries to a 500 MB database typically take 1–5 milliseconds, if the indexes are in order.
* **PDF serving:** NVMe can ingest data faster than the Orange Pi 5's gigabit network card (1 Gbps = approx. 110 MB/s) can transmit. So the network port is the bottleneck, not the disk or CPU.

### 4. Architecture recommendation for this amount of data

1. **SQLite:** Keep text data and metadata (like PDF file locations) here.
2. **File system:** Don't store 700 MB of PDF files inside SQLite (in BLOB fields). It will bloat the database and slow it down. Keep them as separate files on the NVMe disk and store only the path to the file in SQLite.
3. **JSON:** If the JSON fields are large, consider extracting them into SQL tables if you need to search on them. If they are just "attachments", keep them as is.

### Capacity summary

The Orange Pi 5 handles this data brilliantly:

* **500 MB SQLite:** Fits entirely in the OS file cache (Disk Cache) on a 16 GB RAM machine. Repeated queries come directly from RAM.
* **2 GB files:** NVMe handles these with ease. 16 GB of RAM leaves about 14 GB of space for the operating system cache, so a large portion of the PDF data may remain in RAM after the first read.

This is a very stable stack. Pascal's strength here is that it does not use "garbage collection" in the same way as Java or Python, so memory consumption remains predictable even with files of 700 MB.

## How do you retrieve the path to a PDF file from SQLite and serve it securely as a stream?

Here is a practical example of combining a file path retrieved from SQLite and a secure streaming of a large file (such as a 700 MB PDF).

We use the style appropriate for the fphttpapp application here. The key is that the file is not read entirely into memory, but is directly connected to the response content stream.

```pascal
procedure downloadPDFEndpoint(aRequest: TRequest; aResponse: TResponse);
var
  CardId: string;
  FilePath: string;
  FileStream: TFileStream;
begin

  // 1. Get the ID from the url, e.g. /download?id=123
  CardId := aRequest.QueryFields.Values['id'];
  
  if CardId = '' then
  begin
    aResponse.Code := 400;
    aResponse.Content := 'Missing ID';
    aResponse.SendContent;
    Exit;
  end;

  // 2. Get the file path from SQLite
  // NOTE: This assumes you have a SQL function ready
  FilePath := GetFilePathFromDatabase(CardId); 

  if (FilePath <> '') and FileExists(FilePath) then
  begin
    try
      // 3. Open the file as a stream
      // fmShareDenyWrite allows others to read, but not modify the file during transmission
      FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
      
      // 4. Define the response type and stream
      aResponse.ContentType := 'application/pdf';
      // Add a Header so that the browser suggests downloading or displays the name correctly
      aResponse.SetCustomHeader('Content-Disposition', 'inline; filename="' + ExtractFileName(FilePath) + '"');
      
      // IMPORTANT: Connect the file stream to the response
      aResponse.ContentStream := FileStream;
      
      // fphttpapp handles ContentLength and buffering automatically
      aResponse.SendContent;
      
      Writeln('Serving file: ' + FilePath);
    except
      on E: Exception do
      begin
        aResponse.Code := 500;
        aResponse.Content := 'Error reading file: ' + E.Message;
        aResponse.SendContent;
      end;
    end;
  end
  else
  begin
    aResponse.Code := 404;
    aResponse.Content := 'File not found in database or disk.';
    aResponse.SendContent;
  end;
end;
```

## 2. Why does this work on a 16GB Orange Pi 5?

When using `ContentStream`:

- RAM savings: Even if the PDF is 700MB, the server only allocates memory for a small transfer buffer (typically 16-64KB at a time). You can serve dozens of such downloads simultaneously without running out of 16GB of RAM.
- NVMe benefit: The Orange Pi 5's NVMe disk can read multiple files in parallel at a huge speed without the "seek time" caused by a mechanical hard drive.
- CPU load: Compared to the G4 processor, the Orange Pi 5's ARM cores don't even notice such a transfer, as TFileStream is very lightweight.

## 3. SQLite JSON tip for Orange Pi 5

Since you are using SQLite for text data (500MB), here is an optimized way to read JSON fields directly in a SQL query, which saves processing on the Pascal side:

Retrieve the card name and only one value from a JSON field (e.g. priority):
```
SELECT title, json_extract(metadata_json, '$.priority') as priority 
FROM cards 
WHERE id = 123;
```

This is significantly faster than fetching the entire 500 MB of text data into Pascal and parsing it there.

## 4. Practical tip: File placement with NVMe

Don't put all 2 GB of files in the same folder if there are thousands of them. Although NVMe is fast, file systems (like Ext4) will slow down if there are tens of thousands of files in one folder. Use subdirectories:

```
wekandata/attachments/a1/file1.pdf

wekandata/attachments/b2/file2.pdf
```
