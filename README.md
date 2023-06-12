# Thirteen Letters
Thirteen Letters is a simple and unoriginal word game, written in Common Lisp for [Spring Lisp Game Jam (2023)](https://itch.io/jam/spring-lisp-game-jam-2023).

Play the game online here: https://jaredkrinke.itch.io/13l/

## Web-based multiplayer game
Version 2 of this game is a real-time, competitive browser-based game. Each round lasts 60 seconds and players attempt to find the longest word possible using the letters provided.

Note that the game is **100% Common Lisp**. The front end uses [Parenscript](https://parenscript.common-lisp.dev/), [Spinneret](https://github.com/ruricolist/spinneret), and [cl-css](https://github.com/Inaimathi/cl-css) to
translate CL and s-expressions to JavaScript, HTML, and CSS. The back end is built
using [Hunchentoot](https://edicl.github.io/hunchentoot/)/[Hunchensocket](https://github.com/joaotavora/hunchensocket), running on [SBCL](https://www.sbcl.org/).

### Setup/install
No setup or install is needed for the multiplayer game, just open the following URL in your browser:

https://jaredkrinke.itch.io/13l/

## Terminal-based single-player game
Version 1 of this game was minimal (to ensure I could actually finish *something*): single-player, played in the terminal.

The goal of the game is to unscramble a thirteen-letter long word in as few tries as possible. After each guess, one letter will be unscrambled.

There are 10 difficulty levels.

### Setup/install
Version 1 is provided in source form only, so you'll need a Common Lisp environment (e.g. SBCL or ECL).

Just clone this repository and then load "main.lisp", for example:

```sh
sbcl --load main.lisp
```

Follow the on-screen prompts to play.
