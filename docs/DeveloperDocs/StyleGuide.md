## Style Guide


Not possible to support these browsers, because there is no form submit button feature:

- Netscape, IE3


This is Anybrowser-style, designed for:

- WeKan Open Source kanban development
- Accessibility
  - To be accessible to any user, pages need to be visible at any browser. Currently tested at:
    - Amiga IBrowse, FreeDOS Dillo, IE6, Linux Dillo/Netsurf/ELinks/w3m/Chrome/Firefox
    - This requires using http://aminet.net style HTML4
    - Icons with GIF images, that are visible at any browser
    - No browser detection by user agent etc. Same code is for all browsers. For example:
      - Vector graphics for drawing between cards, drawing line draws with all of these all the time:
        - Amiga IBrowse: ASCII Art
        - IE6: VML
        - Netsurf/Chrome: SVG
      - If browser does not support Javascript, moving cards by checkbox selecting them, pressing Move Submit button
      - If browser supports Javascript, Javascript adds drag-drop etc features
      - If device has touch screen, there is touch drag drop features
      - If browser adds support for CSS rounded corners, it shows rounded corners at kanban
      - Vector graphics for drawing between cards, drawing line draws with all of these all the time:
        - Amiga IBrowse: ASCII Art
        - IE6: VML
        - Netsurf/Chrome: SVG
  - 
