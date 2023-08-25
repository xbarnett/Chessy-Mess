import "package:fast_immutable_collections/fast_immutable_collections.dart";
import "package:flutter_svg/flutter_svg.dart";
import "package:flutter/material.dart";
import "package:flutter/services.dart";
import "package:provider/provider.dart";

class State extends ChangeNotifier {
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
                Board(
                  size: 8,
                ),
              ]
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
      defaultColumnWidth: const FixedColumnWidth(BoardSquare.size),
      children: List.generate(size, (i) =>
        TableRow(children: List.generate(size, (j) =>
          BoardSquare(
            piece: pieces[(j, size - 1 - i)],
            color: [SquareColor.white, SquareColor.black][(i + j) % 2],
          )
        )
      )),
    );
  }
}

class BoardSquare extends StatelessWidget {
  static const double size = 100.0;
  final Piece? piece;
  final SquareColor color;

  const BoardSquare({super.key, required this.piece, required this.color});

  @override
  Widget build(BuildContext context) {
    return Stack(
      alignment: Alignment.center,
      children: [
        Container(
          width: size,
          height: size,
          color: color.toColor(),
        ),
        if (piece != null)
        SvgPicture.asset(
          "assets/${piece!.name}.svg",
          colorFilter: ColorFilter.mode(
            piece!.color.toColor(), BlendMode.modulate),
          width: size,
          height: size,
        ),
      ],
    );
  }
}

enum SquareColor {
  white, black;

  Color toColor() {
    return switch(this) {
      white => Colors.white,
      black => Colors.black,
    };
  }
}

enum PieceColor {
  red, blue, grey;

  Color toColor() {
    return switch(this) {
      red => Colors.red,
      blue => Colors.blue,
      grey => Colors.grey,
    };
  }
}
