## Design of Wami

- Binary Formats are Better Than JSON in Browsers!
  - https://adamfaulkner.github.io/binary_formats_are_better_than_json_in_browsers.html
  - https://news.ycombinator.com/item?id=43978476
  - https://github.com/howinfo/howinfo/wiki/Browser#development
- Git Branching style for data
  - https://github.com/howinfo/howinfo/wiki/Database#git-branching-style-for-data
  - Duplicate Board works immeditely, because it's git branch of same data. Not need to copy hundreds of cards slowly one card a time.
- Retro Modern Future
  - All browsers with same code. No feature detection. If browser adds new features, they work in that browser.
  - Retro
    - HTML4 that works at Amiga IBrowse, Netsurf, Lynx, etc
    - No drag drop, so showing checkboxes and buttons for moving etc
  - Modern features, when browser supports it
    - Rounded corners with CSS
    - Drag drop
    - Do not use fake rounded corners with transparent corner GIF, because it would require GIF image for every color
    - There will be custom color selection from color wheel for font and background color. Or trying to calculate colors that are visible enough.
  - Future, when browser and device supports it
    - Touch drag multiple cards at once with many fingers using https://interactjs.io . This is WeKan innovation, no other kanban has it yet.
- No migrations
  - Use directly any existing database structure, with general card convert proxy:
    - WeKan any old and new version structure MongoDB 3, 6, 8 etc
    - Kanboard
    - Trello
    - Jira
    - ClickUp
    - etc
  - Detects what database is used
  - Detects database structure
  - Detects OpenAPI structure
  - Detects JSON structure
  - Can add additional fields to any structure
  - Can save to any structure
  - Import/Export/Sync anything
  - Takes care of rate limits and errors of each API. Sensible default settings, and possibility change settings for rate limit etc.


