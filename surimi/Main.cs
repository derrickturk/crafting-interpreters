namespace Surimi;

using System;
using System.IO;

static class App {
    public static int Main(string[] args)
    {
        switch (args.Length) {
            case 0:
                return RunInteractive();
            case 1:
                return RunFile(args[0]);
            default:
                var prog = System.Environment.GetCommandLineArgs()[0];
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

        interp.Run(src, path);

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
            interp.Run(line, "<stdin>");
            // I really hate this error handling scheme
            onError.HadError = false;
        }

        return 0;
    }
}
