namespace Surimi;

using System;
using System.IO;

static class App {
    public static int Main(string[] args)
    {
        // PPTest();
        switch (args.Length) {
            case 0:
                return RunInteractive();
            case 1:
                return RunFile(args[0]);
            default:
                var prog = Environment.GetCommandLineArgs()[0];
                Console.Error.WriteLine($"Usage: {prog} [source-file]");
                return 2;
        }
    }

    public static int RunFile(string path)
    {
        var onError = new ErrorReporter();
        var interp = new Interpreter(onError);

        string src;
        try {
            src = File.ReadAllText(path);
        } catch (Exception e) {
            Console.Error.WriteLine($"Unable to read {path}: {e}");
            return 1;
        }

        interp.run(src);

        return onError.HadError ? 1 : 0;
    }

    public static int RunInteractive()
    {
        var onError = new ErrorReporter();
        var interp = new Interpreter(onError);

        while (true) {
            Console.Write("> ");
            Console.Out.Flush();
            string? line = Console.ReadLine();
            if (line == null)
                break;
            interp.run(line);
            // I really hate this error handling scheme
            onError.HadError = false;
        }

        return 0;
    }

    private static void PPTest()
    {
        var expr = new BinOpApp(
            BinOp.Plus,
            new BinOpApp(
                BinOp.GtEq,
                new UnOpApp(
                    UnOp.Not,
                    new Literal(true)
                ),
                new Literal(null)
            ),
            new BinOpApp(
                BinOp.EqEq,
                new Literal(36.5),
                new Literal("potato")
            )
        );
        Console.WriteLine(expr.Accept(new PrettyPrinter()));
    }
}
