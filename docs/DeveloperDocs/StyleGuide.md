## Style Guide

### Definitions

- WeKan is Open Source kanban software. There are many different versions of WeKan:
  - Meteor 2
    - Previous version of WeKan
    - https://github.com/wekan/wekan
    - Supports modern browsers
    - Features: https://github.com/wekan/wekan/blob/main/docs/DeveloperDocs/Deep-Dive-Into-WeKan.md
  - Wami
    - Current version of WeKan, in very active development
    - https://github.com/wekan/wami
    - Accessibility
    - Anybrowser: Works at Netsurf etc, required only low amount of RAM and CPU
    - Anyserver: Works at Amiga etc, requires only low amount of RAM and CPU
    - Against planned obsolescence and electronic waste, works also at old OS and old computer
        - https://en.wikipedia.org/wiki/Planned_obsolescence
        - https://en.wikipedia.org/wiki/Electronic_waste
        - Please do not send working computers to electronic waste. It has taken a lot of natural resources to create a computer.
    - Against expensive data centers and expensive servers
      - Please do not use AI servers that uses a lot of electricity, CPU, RAM and clean water
        - https://www.youtube.com/watch?v=8enXRDlWguU
        - Please let people have the life essential clean water and electricity, and not use that for datacenters, because we are running out of land, electricity and water
        - Please do not build more datacenters. Instead, make your datacenters and code more energy efficient.
      - Use only AI that is designed for low RAM and CPU usage
      - Please try to use computers that use smallest amount of electricity
  - Many more versions of WeKan for many platforms:
    - https://github.com/wekan/wekan/blob/main/docs/DeveloperDocs/WeKan-Multiverse-Roadmap.md

### Anybrowser-style, designed for WeKan development

- To be accessible to any user, pages need to be visible at any browser
  - This is working already with code at https://github.com/wekan/wami/blob/main/wekan.pas
- Designed for iffy Internet
  - with minimal amount of browserside code
  - https://github.com/howinfo/howinfo/wiki/Design#should-we-design-for-iffy-internet

### Progressive enhancement

  - https://www.w3.org/wiki/Graceful_degradation_versus_progressive_enhancement
  - No browser detection by user agent etc. All code for all browsers included.
  - Design first for all browsers, having all features available there.
    - This requires using http://aminet.net style HTML4
  - Images and icons with GIF images, that are visible at any browser
  - Works without Javascript
  - Additional features where browser supports it:
    - CSS Rounded Corners
      - not using fake transparent GIF edges, because it would require separate GIF image for each color
        - [2025-06-19](https://github.com/wekan/wami/commit/60a6d583#diff-55eb6b0b766ec41c008ef615b2f1d3e24ba16b8c8ba549a84c5e73e2ab54344bR15-R17) and [2025-06-20](https://github.com/wekan/wami/commit/31ba33b37ab4b867fd2e344bf5ad004085745cb4)
      - [Original WeKan feature from 2022-02-06](https://github.com/wekan/wekan/issues/4326)
    - Drag drop of Swimlanes/Lists/Cards/Checklists
    - If browser does not support Javascript, moving cards by checkbox selecting them, pressing Move Submit button
    - If device has touch screen, there is touch drag drop
    - If device supports multitouch drag and browser supports multidrag, drag many cards at once with https://interactjs.io . This is original WeKan feature.
  - Vector graphics for drawing between cards, drawing line draws with all of these all the time:
    - Amiga IBrowse: ASCII Art
    - IE6: VML
    - Netsurf/Chromium/etc: SVG

### Not supported browsers

- Netscape, IE3
- Because no form submit button feature

### Supported browsers

- Maintained browsers
  - IBrowse https://www.ibrowse-dev.net for AmigaOS® 3.0, 3.1, 3.1.4, 3.2, 3.5, 3.9, 4.1FE and MorphOS
  - Dillo for FreeDOS, Linux, BSD, MacOS, Windows (via Cygwin or WSL), Atari.
  - Text browsers: Links, ELinks, w3m, Chawan
  - Modern browsers based on: Ladybird, Safari, Chromium, Firefox, Webkit, Servo
- Legacy browsers based on browser engines from Microsoft
  - EdgeHTML like Legacy Edge (that was after IE, and before Credge/Chromium Edge)
  - MSHTML like IE6
- List of webbrowsers
  - https://github.com/wekan/wekan/blob/main/docs/Browsers/Browser-compatibility-matrix.md has what is supported by Meteor 2. Wami supports Anybrowser.
  - https://github.com/howinfo/howinfo/wiki/Browser

### Accessibility

- [PRs welcome](MakingPullRequest.md) for accessibility improvements at https://github.com/wekan/wami/pulls
  - Accessibility related comments are at code at https://github.com/wekan/wami/blob/main/wekan.pas
  - Before sending PR, required is to test with all above mentioned supported browsers, that pages are visible and all features work.
- To be accessible to any user, pages need to be visible at any browser. Wami has Anybrowser, Meteor 2 does not have.
- Accessibility links at https://github.com/wekan/wekan/issues/459
- Use only one table. No nested tables, those do not work with screen readers. Do not use merged of split cells.
- Semantic HTML https://developer.mozilla.org/en-US/docs/Glossary/Semantics
- There will be custom color selection from color wheel for font and background color. Or trying to calculate colors that are visible enough.
- Tab order that works for screenreaders and keyboard navigation
- Every webpage is tested with accessibility browser extensions
- Correctly visible colors

### Minimap

- Add minimap, like at some games there is.
- This is original Wami feature, no other kanban has minimap.
- So visible part of WeKan kanban board is loaded immediately, with minimal amount of browserside code.
- From minimap it's possible to move to other parts of board.
- If there is Javascript support, load only visible part of board, and when scrolling, load more cards when that part of board becomes visible.

### HTML4 tags to use first, to make visible at all browsers

- Use only one table. No nested tables, no merged cells, no split cells.
- <code>table tbody tr td</code>
- <code>&lt;font size="4" face="arial" color="#000000" bgcolor="#FFFFFF"&gt;Text&lt;/font&gt;</code>
- <code>border="0"</code>
- <code>tabindex="1"</code> with numbering tab order like 1, 2, 3, etc. For example from card to next card.
- <code>height="80" width="20"</code>
- <code>padding="0" spacing="0"</code>, changing these numbers where it looks better
- <code>id="SomeID"</code>, usually descriptive name of ID so that some code can be used with it, for example for styling, drag drop, etc
- <code>height="20"</code>
- HTML form elements, like checklists, submit buttons
- There is upload example at https://github.com/wekan/wami/blob/main/wekan.pas

### Rarely used tags

These only work at modern browsers, so use only if it looks similar with above HTML4 tags.

- <code>div</code>
- <code>style</code>

## Optional CSS and Javascript code for modern browsers

- Use with <b>relative URLs</b> at <code>wami/public/js/</code> and <code>wami/public/css</code>
  - <b>Do not use absolute URLs</b> like /css/something.css, that does not work with Sub-URL
- Here https://interactjs.io makes possible multidrag, dragging many cards at once with many fingers, at touchscreen with modern browser:
  - <code>&lt;link rel="stylesheet" type="text/css" href="css/interact.css"&gt;</code>
  - <code>&lt;script src="js/interact.js"&gt;&lt;/script&gt;</code>
- This makes possible with Wami:
    - Sub-URL like https://example.com/kanban/
    - [Multitenancy](https://en.wikipedia.org/wiki/Multitenancy)
      - Like having same kanban instance have many different domain URLs for many organizations
      - Makes possible to combine many separate MultiSnap WeKan for separate domains to one WeKan https://github.com/wekan/wekan/blob/main/docs/Platforms/FOSS/Snap/Many-Snaps-on-LXC.md
      - This is not possible with Meteor, that requires environment setting ROOT_URL. This is only possible with Wami.
