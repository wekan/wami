# Wami: WeKan made with FreePascal for Amiga etc

## Cloning repo

```
git clone https://github.com/wekan/wami

cd wami
```

## Compiling with FreePascal

```
fpc wekan.pas
```

## Running at Amiga

```
wekan
```

## Running at Linux

Add executeable permission:
```
chmod +x wekan
```

Run:
```
./wekan
```

## Running at Windows

```
wekan.exe
```

## Stopping

Keyboard keys `Ctrl` and `c`:

```
Ctrl-c
```

## Web browsers

- Tested with all browsers, works also without Javascript:
  - Modern browsers based on: Chromium, Firefox, Safari
  - Upcoming browsers: Ladybird
  - Limited Javascript: Netsurf, Amiga IBrowse, ReactOS 32bit Wine Internet Explorer
  - Without Javascript: Lynx, ELinks, w3m w3m-img, FreeDOS Dillo
  - Legacy browsers: Netscape, IE
- If browser has Javascript support, Javascript code can use https://unpoly.com for additional effects.
- No cookies. No localstorage. Sessions stored to serverside database, based on browser properties. More info at https://github.com/wekan/wekanstudio/blob/main/docs/roadmap.md#sessions

## Backend: FreePascal/SQLite

- SSR (Server Side Rendering). Like Web 1.0, with HTML/CSS at frontend
  using HTML Forms with POST/GET. FreePascal at backend. Similar like LAMP.

## This is one of WeKan Multiverse prototypes

- WeKan Open Source kanban https://wekan.github.io https://github.com/wekan/wekan/wiki/Deep-Dive-Into-WeKan
  - Same features, with changes to use minimal amount of code
  - Same API:
    - https://wekan.github.io/api/ 
    - https://github.com/wekan/wekan/blob/main/api.py
- WeKan Multiverse prototypes https://github.com/wekan/wekan/wiki/WeKan-Multiverse-Roadmap
- Database structure is same as in WeKan Open Source kanban https://wekan.github.io https://github.com/wekan/wekan (also features will be same and more),
  when exported to SQLite with https://github.com/wekan/minio-metadata

Native apps will be added for many CPU/OS. They will use same WeKan APIs, and native hardware features.
