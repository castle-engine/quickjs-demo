# Control a snake using JavaScript (demo of QuickJS-Pascal and Castle Game Engine)

This demo shows [QuickJS-Pascal](https://github.com/Coldzer0/QuickJS-Pascal) (Pascal bindings to [QuickJS](https://bellard.org/quickjs/)) integrated with [Castle Game Engine](https://castle-engine.io/).

The demo is based on a [snake game](https://github.com/eugeneloza/SnakeGame). In the original snake game, you control snake by the keyboard. In this version, you control snake by implementing an A.I. using JavaScript :)

The core file to play with is the [data/movement.js](https://github.com/castle-engine/quickjs-demo/blob/master/data/movement.js). There you have to define a global `update` function, which is called once per second, and can move the snake using these JS functions:

- `goal_position()` - returns a structure with `x` and `y` fields (like `{x:3,y:5}`) that contains the position of the "goal" on the map

- `player_position()` - returns a structure with `x` and `y` fields that contains the current player position of the map.

- `move(x, y)`. Can be called at most once per `update()`. The only allowed arguments' combinations are:
    - `move(+1, 0)` (move right)
    - `move(-1, 0)` (move left)
    - `move(0, +1)` (move up)
    - `move(0, -1)` (move down)

# Supports

The demo is cross-platform and should support every platform supported by [Castle Game Engine](https://castle-engine.io/). Build it using [Castle Game Engine build tool](https://github.com/castle-engine/castle-engine/wiki/Build-Tool) or [Lazarus](https://www.lazarus-ide.org/).

- Tested
    - Mac OS Catalina
    - IOS 13.3.1

- <b>TODO</b>
    - Test on Linux, Windows, Android
    - Add more JS examples
        - Add Start function to be called on game init
        - Implement full UIFont interface to JS


You can download QuickJS library files from here: TODO

### QuickJS-Pascal

https://github.com/Coldzer0/QuickJS-Pascal

### Game Credit

Demo by [Coldzer0](https://github.com/Coldzer0), who also created [QuickJS-Pascal](https://github.com/Coldzer0/QuickJS-Pascal).

[Original snake game](https://github.com/eugeneloza/SnakeGame) by [Eugene Loza](https://github.com/eugeneloza/).
