import "package:fast_immutable_collections/fast_immutable_collections.dart";
import "package:flutter_svg/flutter_svg.dart";
import "package:flutter/material.dart";
import "package:flutter/services.dart";
import "package:provider/provider.dart";
import "package:web_socket_channel/web_socket_channel.dart";

class State extends ChangeNotifier {
  final WebSocketChannel _channel;

  (int, int)? _hoverSquare;
  (int, int)? _selectSquare;
  (int, int)? get hoverSquare => _hoverSquare;
  (int, int)? get selectSquare => _selectSquare;

  final int _x1 = -4;
  final int _y1 = -4;
  final int _x2 = 4;
  final int _y2 = 4;
  int get x1 => _x1;
  int get y1 => _y1;
  int get x2 => _x2;
  int get y2 => _y2;

  GameState? _game;
  GameState? get game => _game;

  State() : _channel =
      WebSocketChannel.connect( Uri.parse("ws://localhost:8000")) {
    requestGame();
    processResponses();
  }

  void requestGame() async {
    Map<String, dynamic> request = {
      "request": "get state",
      "x1": x1,
      "y1": y1,
      "x2": x2,
      "y2": y2,
    };
    _channel.sink.add(request.toString());
  }

  void processResponses() async {
    await for (String s in _channel.stream) {
      // do stuff with s
      notifyListeners();
    }
  }

  void hoverEnter((int, int) p) {
    _hoverSquare = p;
    notifyListeners();
  }

  void hoverExit() {
    _hoverSquare = null;
    notifyListeners();
  }

  void click((int, int) p) {
    if (_selectSquare == null) {
      _selectSquare = p;
    } else {
      if (p != _selectSquare) {
        print("move from $_selectSquare to $p");
      }
      _selectSquare = null;
    }
    notifyListeners();
  }
}

class GameState {
}

abstract class Piece {
  final PieceColor color;
  String get name;

  const Piece({required this.color});
}

class Pawn extends Piece {
  const Pawn({required super.color});

  @override
  String get name => "pawn";
}

class Rook extends Piece {
  const Rook({required super.color});

  @override
  String get name => "rook";
}

class Knight extends Piece {
  const Knight({required super.color});

  @override
  String get name => "knight";
}

class Bishop extends Piece {
  const Bishop({required super.color});

  @override
  String get name => "bishop";
}

class Queen extends Piece {
  const Queen({required super.color});

  @override
  String get name => "queen";
}

class King extends Piece {
  const King({required super.color});

  @override
  String get name => "king";
}

void main() {
  runApp(
    ChangeNotifierProvider(
      create: (_) => State(),
      child: const App(),
    )
  );
}

class App extends StatelessWidget {
  const App({super.key});

  @override
  Widget build(BuildContext context) => MaterialApp(
    home: const Home(),
    theme: ThemeData.dark(useMaterial3: true),
    title: "Chessy Mess!",
  );
}

class Home extends StatelessWidget {
  const Home({super.key});

  @override
  Widget build(BuildContext context) {
    State state = context.watch<State>();

    return Focus(
      autofocus: true,
      onKeyEvent: (_, event) {
        if (event is KeyDownEvent) {
          switch (event.logicalKey) {
          }
        }
        return KeyEventResult.handled;
      },
      child: Scaffold(
        body: SafeArea(
          child: Center(
            child: Row(
              mainAxisAlignment: MainAxisAlignment.center,
              children: [
                if (state.game == null)
                Text(
                  "Waiting for connection...",
                  style: const TextStyle(
                    fontSize: 32.0,
                  )
                )
                else
                Board(size: 8),
              ],
            ),
          ),
        ),
      ),
    );
  }
}

class Board extends StatelessWidget {
  static const startRow = [
    Rook.new,
    Knight.new,
    Bishop.new,
    Queen.new,
    King.new,
    Bishop.new,
    Knight.new,
    Rook.new,
  ];

  final int size; // positive
  final IMap<(int, int), Piece?> pieces; // keys in [0, size)

  Board({super.key, required this.size}) :
    pieces = IMap.fromKeys(
      keys: Iterable.generate(size * size, (i) => (i ~/ size, i % size)),
      valueMapper: (p) {
        return switch (p) {
          (int i, 0) => startRow[i](color: PieceColor.red),
          (_, 1) => const Pawn(color: PieceColor.red),
          (_, 6) => const Pawn(color: PieceColor.blue),
          (int i, 7) => startRow[i](color: PieceColor.blue),
          _ => null,
        };
      }
    );

  @override
  Widget build(BuildContext context) {
    return Table(
      border: TableBorder.all(
        width: 1.0,
      ),
      defaultColumnWidth: const FixedColumnWidth(BoardSquare.size),
      children: List.generate(size, (i) =>
        TableRow(children: List.generate(size, (j) =>
          BoardSquare(
            piece: pieces[(j, size - 1 - i)],
            color: [SquareColor.white, SquareColor.black][(i + j) % 2],
            coordinate: (j, size - 1 - i),
          )
        )
      )),
    );
  }
}

class BoardSquare extends StatelessWidget {
  static const double size = 75.0;
  final Piece? piece;
  final SquareColor color;
  final (int, int) coordinate;

  const BoardSquare({
      super.key,
      required this.coordinate,
      required this.piece,
      required this.color
  });

  Color realColor((int, int)? hoverSquare, (int, int)? selectSquare) {
    if (coordinate == selectSquare) {
      return Colors.brown;
    } else if (coordinate == hoverSquare) {
      return Colors.yellow;
    } else {
      return switch (color) {
        SquareColor.white => const Color(0xFFFFCC9C),
        SquareColor.black => const Color(0xFFD88C44),
      };
    }
  }

  @override
  Widget build(BuildContext context) {
    State state = context.watch<State>();

    return GestureDetector(
      onTap: () => state.click(coordinate),
      child: MouseRegion(
        onEnter: (_) => state.hoverEnter(coordinate),
        onExit: (_) => state.hoverExit(),
        child: Stack(
          alignment: Alignment.center,
          children: [
            Container(
              width: size,
              height: size,
              color: realColor(state.hoverSquare, state.selectSquare)
            ),
            if (piece != null)
            SvgPicture.asset(
              "assets/${piece!.name}.svg",
              colorFilter: switch (piece!.color) {
                PieceColor.red => const ColorFilter.matrix([
                  1/3, 1/3, 1/3, 0, 0,
                    0,   0,   0, 0, 0,
                    0,   0,   0, 0, 0,
                    0,   0,   0, 1, 0,
                ]),
                PieceColor.blue => const ColorFilter.matrix([
                    0,   0,   0, 0, 0,
                    0,   0,   0, 0, 0,
                  1/3, 1/3, 1/3, 0, 0,
                    0,   0,   0, 1, 0,
                ]),
                PieceColor.grey => const ColorFilter.matrix([
                  1/2,   0,   0, 0, 0,
                  0,   1/2,   0, 0, 0,
                  0,     0, 1/2, 0, 0,
                  0,     0,   0, 1, 0,
                ]),
              },
              width: size,
              height: size,
            ),
          ],
        ),
      ),
    );
  }
}

enum SquareColor {
  white, black;
}

enum PieceColor {
  red, blue, grey;
}
