namespace Surimi;

internal record class Builtin(string Name, int Arity,
  Func<List<object?>, object?> Function): Callable {
    public object? Call(List<object?> arguments) =>
      Function(arguments);
    public override string ToString() => $"<built-in function {Name}>";

    public static readonly SrcLoc BuiltinLocation = new SrcLoc(
      "<builtins>", 0, 0);

    public static Builtin[] BuiltinFunctions = new Builtin[] {
        new Builtin("clock", 0,
          (_) => (double)DateTime.Now.Ticks / TimeSpan.TicksPerMillisecond),
        new Builtin("read_line", 0,
          (_) => System.Console.ReadLine()),
        new Builtin("to_string", 1,
          (args) => Interpreter.ValueString(args[0])),
    };
}
