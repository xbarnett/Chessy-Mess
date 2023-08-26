import "dart:convert";
import "package:flutter_svg/flutter_svg.dart";
import "package:flutter/material.dart";
import "package:flutter/services.dart";
import "package:provider/provider.dart";
import "package:web_socket_channel/web_socket_channel.dart";

class State extends ChangeNotifier {
  final bool landing = true;
  List<String> _names = [];
  List<String> get names => _names;
  String? _name;
  String? get name => _name;

  final nameBuffer = TextEditingController();

  final WebSocketChannel _channel;

  (BigInt, BigInt)? _hoverSquare;
  (BigInt, BigInt)? _selectSquare;
  (BigInt, BigInt)? get hoverSquare => _hoverSquare;
  (BigInt, BigInt)? get selectSquare => _selectSquare;

  int _scale = 4;
  int get scale => _scale;

  BigInt _x1 = BigInt.from(0);
  BigInt _y1 = BigInt.from(0);
  BigInt _x2 = BigInt.from(0);
  BigInt _y2 = BigInt.from(0);
  BigInt get x1 => _x1;
  BigInt get y1 => _y1;
  BigInt get x2 => _x2;
  BigInt get y2 => _y2;

  GameState? _game;
  GameState? get game => _game;

  List<(BigInt, BigInt)> _underAttack = [];
  List<(BigInt, BigInt)> get underAttack => _underAttack;

  void requestName() {
    Map<String, dynamic> request = {
      "\"tag\"": "\"RequestName\"",
      "\"contents\"": "\"${nameBuffer.text}\"",
    };
    print("sending:");
    print(request.toString());
    _channel.sink.add(request.toString());
  }

  void zoomOut() {
    if (scale < 16) {
      _x1 -= BigInt.from(scale);
      _y1 -= BigInt.from(scale);
      _x2 += BigInt.from(scale);
      _y2 += BigInt.from(scale);
      _scale *= 2;
    }
    requestGame();
    requestAttacking();
  }

  void zoomIn() {
    if (scale > 2) {
      _x1 += BigInt.from(scale ~/ 2);
      _y1 += BigInt.from(scale ~/ 2);
      _x2 -= BigInt.from(scale ~/ 2);
      _y2 -= BigInt.from(scale ~/ 2);
      _scale ~/= 2;
    }
    requestGame();
    requestAttacking();
  }

  void scrollRight() {
    _x1 += BigInt.from(1);
    _x2 += BigInt.from(1);
    requestGame();
    requestAttacking();
  }
  void scrollLeft() {
    _x1 -= BigInt.from(1);
    _x2 -= BigInt.from(1);
    requestGame();
    requestAttacking();
  }

  void scrollUp() {
    _y1 += BigInt.from(1);
    _y2 += BigInt.from(1);
    requestGame();
    requestAttacking();
  }

  void scrollDown() {
    _y1 -= BigInt.from(1);
    _y2 -= BigInt.from(1);
    requestGame();
    requestAttacking();
  }

  State() : _channel =
      WebSocketChannel.connect(Uri.parse("ws://localhost:8000")) {
    _x1 = BigInt.from(-scale);
    _y1 = BigInt.from(-scale);
    _x2 = BigInt.from(scale);
    _y2 = BigInt.from(scale);
    processResponses();
  }

  void requestGame() async {
    Map<String, dynamic> request = {
      "\"tag\"": "\"RequestState\"",
      "\"x1\"": x1,
      "\"y1\"": y1,
      "\"x2\"": x2,
      "\"y2\"": y2,
    };
    _channel.sink.add(request.toString());
  }

  void requestAttacking() {
    if (selectSquare == null) {
      return;
    }

    Map<String, dynamic> request = {
      "\"tag\"": "\"RequestAttacking\"",
      "\"contents\"": [
          [_selectSquare!.$1, _selectSquare!.$2],
          [x1, y1],
          [x2, y2]
        ]
      };
    _channel.sink.add(request.toString());
  }

  void processResponses() async {
    await for (String s in _channel.stream) {
      print("received");
      print(s);
      Map<String, dynamic> json = jsonDecode(s);
      if (json["tag"] == "ResponseState") {
        _game = GameState.fromJson(json);
        notifyListeners();
      } else if (json["tag"] == "ResponseAttacking") {
        _underAttack = List.generate(json["contents"].length, (i) => (
            BigInt.from(json["contents"][i][0]),
            BigInt.from(json["contents"][i][1]),
          )
        );
        notifyListeners();
      } else if (json["tag"] == "ResponseHello") {
        _name = json["contents"][0];
        _names = List.generate(json["contents"][1].length, (i) =>
          json["contents"][1][i],
        );
        notifyListeners();
      }
    }
  }

  void hoverEnter((int, int) p) {
    _hoverSquare = (BigInt.from(p.$1) + x1, BigInt.from(p.$2) + y1);
    notifyListeners();
  }

  void hoverExit() {
    _hoverSquare = null;
    notifyListeners();
  }

  void click((int, int) p) {
    (BigInt, BigInt) q = (BigInt.from(p.$1) + x1, BigInt.from(p.$2) + y1);

    if (_selectSquare == null) {
      _selectSquare = (BigInt.from(p.$1) + x1, BigInt.from(p.$2) + y1);
      requestAttacking();
    } else {
      if (q != _selectSquare) {
        Map<String, dynamic> request = {
          "\"tag\"": "\"RequestMove\"",
          "\"contents\"": {
            "\"mfrom\"": [
              selectSquare!.$1,
              selectSquare!.$2,
            ],
            "\"mto\"": [
              q.$1,
              q.$2,
            ]
          }
        };
        _channel.sink.add(request.toString());
      }
      _selectSquare = null;
      _underAttack = [];
    }
    requestGame();
  }
}

class GameState {
  final int xdim;
  final int ydim;
  final List<List<Piece?>> pieces;
  final PieceColor player;
  final PieceColor? winner;

  const GameState({
    required this.xdim,
    required this.ydim,
    required this.pieces,
    required this.player,
    required this.winner,
  });

  static GameState fromJson(Map<String, dynamic> json) {
    int xdim = json["xdim"];
    int ydim = json["ydim"];
    List<List<Piece?>> pieces = List.generate(xdim, (i) =>
      List.generate(ydim, (j) {
          dynamic val = json["pieces"][i][j];
          if (val == null) {
            return null;
          }
          return Piece.fromJson(val);
        }
      )
    );
    return GameState(
      xdim: xdim,
      ydim: ydim,
      pieces: pieces,
      player: json["player"] == "Red" ? PieceColor.red : PieceColor.blue,
      winner: switch (json["hasWon"]) {
        "Red" => PieceColor.red,
        "Blue" => PieceColor.blue,
        _ => null,
      },
    );
  }
}

abstract class Piece {
  final PieceColor color;
  String get name;

  const Piece({required this.color});

  static Piece fromJson(Map<String, dynamic> json) {
    PieceColor color = switch (json["pieceAlignment"]) {
      "LE" => PieceColor.red,
      "NE" => PieceColor.red,
      "CE" => PieceColor.red,
      "LG" => PieceColor.blue,
      "NG" => PieceColor.blue,
      "CG" => PieceColor.blue,
      _ => PieceColor.grey,
    };

    return switch (json["pieceType"]) {
      "Pawn" => Pawn(color: color),
      "Rook" => Rook(color: color),
      "Knight" => Knight(color: color),
      "Bishop" => Bishop(color: color),
      "Queen" => Queen(color: color),
      "King" => King(color: color),
      _ => throw ErrorDescription("unknown piece"),
    };
  }
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
  Widget build(BuildContext context) {
    State state = context.watch<State>();

    return MaterialApp(
      home: const Landing(),
      theme: ThemeData.dark(useMaterial3: true),
      title: "Chessy Mess!",
    );
  }
}

class Landing extends StatelessWidget {
  const Landing({super.key});

  @override
  Widget build(BuildContext context) {
    State state = context.watch<State>();

    return Scaffold(
      body: SafeArea(
        child: Center(
          child: Column(
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              if (state.name == null)
              Row(
                mainAxisAlignment: MainAxisAlignment.center,
                children: [
                  Container(
                    constraints: const BoxConstraints(
                      maxWidth: 200.0,
                    ),
                    child: TextField(
                      controller: state.nameBuffer,
                      decoration: const InputDecoration(
                        border: OutlineInputBorder(),
                        hintText: "Enter your name"
                      ),
                    ),
                  ),
                  ElevatedButton(
                    onPressed: state.requestName,
                    child: const Text("enter")
                  )
                ],
              )
              else if (state.names.length >= 2)
              const Text(
                "Choose who to play:",
                style: TextStyle(
                  fontSize: 32.0,
                )
              )
              else
              const Text(
                "No players yet!",
                style: TextStyle(
                  fontSize: 32.0,
                )
              ),
              for (String name in state.names)
                if (name != state.name)
                Card(
                  clipBehavior: Clip.hardEdge,
                  child: InkWell(
                    splashColor: Colors.blue,
                    onTap: () {
                      print("tap");
                    },
                    child: SizedBox(
                      width: 200,
                      height: 50,
                      child: Center(
                        child: Text(
                          name,
                          style: const TextStyle(
                            fontSize: 16.0
                          )
                        ),
                      )
                    )
                  ),
                )
            ],
          ),
        ),
      ),
    );
  }
}

class Home extends StatelessWidget {
  const Home({super.key});

  @override
  Widget build(BuildContext context) {
    State state = context.watch<State>();

    Color? bg;
    if (state.game != null) {
      bg = switch (state.game!.player) {
        PieceColor.blue => Colors.blue,
        PieceColor.red => Colors.red,
        _ => throw ErrorDescription("grey move ?!"),
      };
    }

    return Focus(
      autofocus: true,
      onKeyEvent: (_, event) {
        if (event is KeyDownEvent || event is KeyRepeatEvent) {
          switch (event.logicalKey) {
            case LogicalKeyboardKey.arrowRight:
            case LogicalKeyboardKey.keyD:
              state.scrollRight();
            case LogicalKeyboardKey.arrowLeft:
            case LogicalKeyboardKey.keyA:
              state.scrollLeft();
            case LogicalKeyboardKey.arrowUp:
            case LogicalKeyboardKey.keyW:
              state.scrollUp();
            case LogicalKeyboardKey.arrowDown:
            case LogicalKeyboardKey.keyS:
              state.scrollDown();
            case LogicalKeyboardKey.keyO:
              state.zoomOut();
            case LogicalKeyboardKey.keyI:
              state.zoomIn();
          }
        }
        return KeyEventResult.handled;
      },
      child: Scaffold(
        backgroundColor: bg,
        body: SafeArea(
          child: Center(
            child: state.game == null ? const Text(
              "Waiting for connection...",
               style: TextStyle(
                 fontSize: 32.0,
               )
             ) : Container(
              decoration: BoxDecoration(
                border: Border.all(width: 4.0),
              ),
              child: Board(game: state.game!),
              ),
          ),
        ),
      ),
    );
  }
}

class Board extends StatelessWidget {
  final GameState game;

  const Board({super.key, required this.game});

  @override
  Widget build(BuildContext context) {
    State state = context.watch<State>();
    double size = 400.0 / state.scale;

    return Table(
      border: TableBorder.all(
        width: 1.0,
      ),
      defaultColumnWidth: FixedColumnWidth(size),
      children: List.generate(game.ydim, (i) =>
        TableRow(children: List.generate(game.xdim, (j) =>
          BoardSquare(
            piece: game.pieces[j][game.ydim - 1 - i],
            color: [SquareColor.white, SquareColor.black][(i + j) % 2],
            coordinate: (j, game.ydim - 1 - i),
            size: size,
          ),
        ),
      )),
    );
  }
}

class BoardSquare extends StatelessWidget {
  final double size;
  final Piece? piece;
  final SquareColor color;
  final (int, int) coordinate;

  const BoardSquare({
      super.key,
      required this.coordinate,
      required this.piece,
      required this.color,
      required this.size,
  });

  Color realColor((BigInt, BigInt)? hoverSquare,
    (BigInt, BigInt)? selectSquare, State state) {
    (BigInt, BigInt) bc = (
      BigInt.from(coordinate.$1) + state.x1,
      BigInt.from(coordinate.$2) + state.y1,
    );

    if (bc == selectSquare && state.game!.winner == null) {
      return Colors.pink.shade900;
    } else if (bc == hoverSquare) {
      return Colors.pink.shade100;
    } else if (state.underAttack.contains(bc)) {
      return Colors.pink.shade500;
    }
    else {
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
              color: realColor(state.hoverSquare, state.selectSquare, state)
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

  String toHumanString() => switch(this) {
    red => "Red",
    blue => "Blue",
    grey => "Grey",
  };
}
