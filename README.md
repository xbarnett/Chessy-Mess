# Chessy Mess!

A crazy chessy game on an infinite board.
Currently there are a lot of ~~bugs~~features.
The frontend is written in Flutter/Dart and the backend is written in Haskell.
Have fun!

Advice:
- Do not use your smartphone to connect to the game.
- Only use HTTP to connect to the game.
- Use ``w``/``a``/``s``/``d`` or the arrow keys to navigate.
- Use ``i``/``o`` to zoom in/out.
- Physically click the "enter" button, or your input will silently be ignored.
- Do not use the same username twice, or it will silently be ignored.
- It is dishonourable to move your opponent's pieces.
- You have lost The Game.

**Link**: [here](http://chessymess.barnett.au).


## Build Instructions:

To build the front end, navigate to the ``frontend`` directory and run ``flutter build web``.
Then, go to the ``build/web`` subdirectory and run ``python -m http.server 8000``.

To build the back end, navigate to the ``backend`` directory and run ``stack run``.
You'll need to install a bunch of Haskell software first (I recommend using ``ghcup``).

## References:

Piece assets from [here](https://commons.wikimedia.org/wiki/Category:SVG_chess_pieces).
