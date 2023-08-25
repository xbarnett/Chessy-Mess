import "package:flutter/material.dart";
import "package:provider/provider.dart";

class State extends ChangeNotifier {
  int _count = 0;

  int get count => _count;

  void increment() {
    _count += 1;
    notifyListeners();
  }
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
    State state = context.read<State>();

    return Scaffold(
      body: SafeArea(
        child: Text("${state.count}"),
      ),
    );
  }
}
