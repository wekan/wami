## Design of Wami

- This is Gold Standard for Cross-Platform GUI App Design by [xet7](https://github.com/xet7) https://forum.lunduke.com/t/gold-standard-of-gui-design/1909

#### Because

Possibility save real money and environment, when it will require less CPU, RAM and disk space:

- This is very important now that prices of hardware are rising rapidly.
- RAMpocalype 2026: https://www.youtube.com/watch?v=2wB7JNRU-BQ
- OpenAI bought 40% or RAM: https://www.youtube.com/watch?v=D0tZpmrDY0w
- Games, Electricity Crisis, Prices https://www.youtube.com/watch?v=MNdFW39NssQ

#### 1) Scale down: Offline/Retro/no-JS/minimal first

- Compile Offline with FreePascal
- Compile step does not download dependencies from Internet, like NPM, PyPi and Rust Crates.io
- Minimal code, included directly to WeKan repo
- Many additional OS/CPU platforms of FreePascal https://www.freepascal.org/download.html , much more than what Node.js/Python/Rust supports
- It's possible to develop Wami with FreePascal at AmigaOS/AROS/MorphOS, compiling and running WeKan server, using with IBrowse webbrowser, using SQLite database, using much less CPU and RAM.
- Platform Promise like GitLab mentioned at GitLab 18 event: "No separate tools required".
- [Scale Up](ScaleUp.md)

#### 2) Accessibility

- Better keyboard navigation
- Update to colors for better visibility
- Calendar nested table to one table, that will work better with screen readers

#### 3) Serverside rendering

- Serverside rendering, so it works with all browsers
  - No errors like `Uncaught RangeError: Maximum call stack size exceeded` that could be related to some Async Await code somewhere
    - https://github.com/wekan/wekan-meteor3/issues/3
    - https://forums.meteor.com/t/prd-for-migrating-to-meteor-3-using-ai-tools/63876/2?u=xet7
- Visible without Javascript. Checkboxes to select cards, submit buttons to move etc. This also means that features like Gantt Chart and Red Strings will 
- If browser supports Javascript, drag drop etc features that are possible with Javascript

#### 4) Make WeKan simpler

- Similar, like there is Basecamp being made simpler
  - https://www.youtube.com/watch?v=V_WkIbqhGQ4
  - https://www.youtube.com/watch?v=Cz6J9eU268Q

#### 5) Immediate Save

- Clicking Save saves immediately

#### 6) Only load visible small amount of data

- https://github.com/wekan/wekan/issues/4948

#### 7) Popup menus etc to full screen pages

- Because clicking outside of popup has bugs
  - https://github.com/wekan/wekan/issues/5686
  - https://github.com/wekan/wekan/issues/4449
- More is visible at once

#### 8) Minimap

- Add minimap, like at some games there is. Clicking some part of map moves to show that area of map.

This is original Wami feature, no other kanban has minimap. So visible part of WeKan kanban board is loaded immediately, with minimal amount of browserside code, and from minimap it's possible to move to other parts of board. If there is Javascript support, load only visible part of board, and when scrolling, load more cards when that part of board becomes visible.

#### 9) Some of current Meteor 2 WeKan Design

that is based to:

- by original WeKan creator [mquandalle](https://github.com/mquandalle)
  - [Original redesign](https://github.com/wekan/wekan/blob/main/docs/FAQ/FAQ.md#werent-you-called-libreboard-before) 
- by [xet7](https://github.com/xet7) current maintainer of WeKan
  - [Improvements to original design](https://github.com/wekan/wekan/blob/main/docs/DeveloperDocs/Design-Principles.md)
  - [Monkey Proof Software](https://github.com/wekan/wekan/blob/main/docs/DeveloperDocs/Monkey-Proof-Software.md)
- NOT using transparent or Liquid Glass Design of macOS/iOS 26 Beta, Windows Vista etc https://github.com/howinfo/howinfo/wiki/Design or shadow, those were tried at WeKan, they did not work, too hard to see https://github.com/wekan/wekan/pull/1726
- Color slide is OK, like at WeKan theme `clearblue`
- Mobile and desktop apps with fullscreen browsers like already is at with https://wekan.fi/app/ , where automatic Snap backend upgrade also updates backend app, not requiring publising new versions of frontend apps to stores

#### 9) Binary Formats are Better Than JSON in Browsers

- https://adamfaulkner.github.io/binary_formats_are_better_than_json_in_browsers.html
- https://news.ycombinator.com/item?id=43978476
- https://github.com/howinfo/howinfo/wiki/Browser#development

#### 10) Git Branching style for data

- https://github.com/howinfo/howinfo/wiki/Database#git-branching-style-for-data
- Duplicate Board works immeditely, because it's git branch of same data. Not need to copy hundreds of cards slowly one card a time.

#### 11) Design Style: Retro Modern Future

- This Design is [original feature of Wami 2025-06-29](https://github.com/wekan/wami/commit/6758c310) from [xet7](https://github.com/xet7). Later, [xet7](https://github.com/xet7) at 2025-07-12 found similar words used at https://www.commodore.net/faq , maybe they did see this Wami page, or they came up with similar idea.
- All browsers with same code, this can be seen working already with code at https://github.com/wekan/wami/blob/main/wekan.pas, where same code works and shows visible page at FreeDOS Dillo, Amiga IBrowse, Chrome etc, using some additional features where available. No feature detection. If browser adds new features, they work in that browser. Meteor 2 WeKan is not visible in all browsers, it requires modern browser with Javascript support.
- Designed for iffy Internet, with minimal amount of browserside code, does not require Javascript, but there is additional features when is Javascript support https://github.com/howinfo/howinfo/wiki/Design#should-we-design-for-iffy-internet
- Designed for Accessibility https://github.com/wekan/wekan/issues/459
- Retro
  - HTML4 that works at Amiga IBrowse, Netsurf, Lynx, etc
  - At AmigaOS/AROS/MorphOS at 680x0/PPC/x86 uses FreePascal compiled at Amiga, SQLite database, IBrowse webbrowser or other available webbrowser
  - No drag drop, so showing checkboxes and buttons for moving etc
  - No SVG or VML, so uses ASCII graphics for drawing lines between cards
  - At IE6, uses VML vector graphics for drawing lines between cards
  - No idea how to support Netscape or IE3, because they do not support submit form buttons
- Modern features, when browser supports it
  - Rounded corners with CSS, that is [original WeKan feature from 2022-02-06](https://github.com/wekan/wekan/issues/4326)
  - Not using original Wami feature rounded corners for retro browsers with transparent corner GIF image from [2025-06-19](https://github.com/wekan/wami/commit/60a6d583#diff-55eb6b0b766ec41c008ef615b2f1d3e24ba16b8c8ba549a84c5e73e2ab54344bR15-R17) and [2025-06-20](https://github.com/wekan/wami/commit/31ba33b37ab4b867fd2e344bf5ad004085745cb4) because it would require GIF image for every color
  - Drag drop
  - There will be custom color selection from color wheel for font and background color. Or trying to calculate colors that are visible enough.
- Future, when browser and device supports it
  - Touch drag multiple cards at once with many fingers using https://interactjs.io . This is [original Wami feature from 2025-03-27](https://github.com/wekan/wami/commit/5ef07efeac081c372c5e389eb9e6d80704a2614f), no other kanban has it yet.

#### 12) No migrations

- Use directly any existing database structure, with general card convert proxy:
  - WeKan any old and new version structure MongoDB 3, 6, 8 etc
  - SQLite. Uses database structure of MongoDB to SQLite migration script https://github.com/wekan/minio-metadata
  - Kanboard SQLite
  - Trello API
  - Jira API
  - ClickUp API
  - etc
- Detects what database is used
- Detects database structure
- Detects OpenAPI structure
- Detects JSON structure
- Can add additional fields to any structure
- Can save to any structure
- Import/Export/Sync anything
- Takes care of rate limits and errors of each API. Sensible default settings, and possibility change settings for rate limit etc.

#### 13) Have Wami repo issues mirrored at many places

- GitHub, original location
- Fossil SCM, so cloning repo also clones issues fast. There is no rate limits at downloading issues with Fossil SCM, like there is at GitHub. Fossil SCM stores repo to SQLite file, so it's possible to use it with SQLite command also at Amiga etc.
- Gitea
- Issues with https://github.com/git-bug/git-bug

#### 14) Additional Feature Requests

- Feature Request: System options to GDPR compliance https://github.com/wekan/wekan/issues/5820
- Question: Is there a way to bulk remove labels https://github.com/wekan/wekan/issues/5819
