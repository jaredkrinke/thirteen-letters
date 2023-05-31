# Thirteen Letters
Thirteen Letters is a simple and unoriginal word game, written in Common Lisp for [Spring Lisp Game Jam (2023)](https://itch.io/jam/spring-lisp-game-jam-2023).

## Description
Version 1 of this game minimal (to ensure I could actually finish *something*): single-player, played in the terminal.

The goal of the game is to unscramble a thirteen-letter long word in as few tries as possible. After each guess, one letter will be unscrambled.

There are 10 difficulty levels.

## Setup/install
Version 1 is provided in source form only, so you'll need a Common Lisp environment (e.g. SBCL or ECL).

Just clone this repository and then load "main.lisp", for example:

```sh
sbcl --load main.lisp
```

Follow the on-screen prompts to play.
