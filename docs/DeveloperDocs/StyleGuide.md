## Style Guide

Anybrowser-style, designed for WeKan Open Source kanban development.

- To be accessible to any user, pages need to be visible at any browser. 

Progressive enhancement

  - https://www.w3.org/wiki/Graceful_degradation_versus_progressive_enhancement
  - No browser detection by user agent etc. All code for all browsers included.
  - Design first for all browsers, having all features available there.
    - This requires using http://aminet.net style HTML4
  - Images and icons with GIF images, that are visible at any browser
  - Works without Javascript
  - Additional features where browser supports it:
    - CSS Rounded Corners (not using fake transparent GIF edges, because it would require separate GIF image for each color)
    - Drag drop of Swimlanes/Lists/Cards/Checklists
    - If browser does not support Javascript, moving cards by checkbox selecting them, pressing Move Submit button
    - If device has touch screen, there is touch drag drop
    - If device supports multitouch drag and browser supports multidrag, drag many cards at once with https://interactjs.io . This is original WeKan feature.
  - Vector graphics for drawing between cards, drawing line draws with all of these all the time:
    - Amiga IBrowse: ASCII Art
    - IE6: VML
    - Netsurf/Chromium/etc: SVG

Not supported browsers

- Netscape, IE3
- Because no form submit button feature

Supported browsers

- Maintained browsers
  - IBrowse https://www.ibrowse-dev.net for AmigaOS® 3.0, 3.1, 3.1.4, 3.2, 3.5, 3.9, 4.1FE and MorphOS
  - Dillo for FreeDOS, Linux, BSD, MacOS, Windows (via Cygwin or WSL), Atari.
  - Text browsers: Links, ELinks, w3m, Chawan
  - Modern browsers based on: Ladybird, Safari, Chromium, Firefox, Webkit, Servo
- Legacy browsers based on browser engines from Microsoft
  - EdgeHTML like Legacy Edge (that was after IE, and before Credge/Chromium Edge)
  - MSHTML like IE6

Accessibility

- To be accessible to any user, pages need to be visible at any browser.
- Use only one table. No nested tables, those do not work with screen readers. Do not use merged of split cells.
- Semantic HTML https://developer.mozilla.org/en-US/docs/Glossary/Semantics
