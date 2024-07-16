# Control a snake using JavaScript (demo of QuickJS-Pascal and Castle Game Engine)

## Overview

This demo shows [QuickJS-Pascal](https://github.com/Coldzer0/QuickJS-Pascal) (Pascal bindings to [QuickJS](https://bellard.org/quickjs/)) integrated with [Castle Game Engine](https://castle-engine.io/).

The demo is to show how to bind native classes and functions to the JS engine to build a game in fully in JS.

The core file to play with is the [data/main.js](https://github.com/castle-engine/quickjs-demo/blob/full_translation/data/main.js). There you have to import the needed Classes from `CastleEngine` to build a game from scratch:

- `Application` - has the same options in native Application ( Still in progress ).

- `Window` - This class is needed to build a new window from the JS engine and it contains:

- `Window.OnRender` that allows you to render whatever you want from JS engine side.

## Supported platforms

The demo is cross-platform and should support every platform supported by [Castle Game Engine](https://castle-engine.io/). Build it using [Castle Game Engine build tool](https://castle-engine.io/build_tool) or [Lazarus](https://www.lazarus-ide.org/).

- Tested
    - Mac OS
    - Linux x86_64
    - Windows

- <b>TODO</b>
    - Check [TODO.md](https://github.com/castle-engine/quickjs-demo/blob/full_translation/TODO.md)

You can download QuickJS library files from here: [Download](https://github.com/Coldzer0/QuickJS-Pascal/tree/master/LibQJS)

## QuickJS-Pascal

https://github.com/Coldzer0/QuickJS-Pascal
