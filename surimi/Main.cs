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
                var prog = Environment.GetCommandLineArgs()[0];
                Console.Error.WriteLine($"Usage: {prog} [source-file]");
                return 2;
        }
    }

    public static int RunFile(string path)
    {
        var interp = new Interpreter();

        string src;
        try {
            src = File.ReadAllText(path);
        } catch (Exception e) {
            Console.Error.WriteLine($"Unable to read {path}: {e}");
            return 1;
        }

        interp.run(src);
        return 0;
    }

    public static int RunInteractive()
    {
        var interp = new Interpreter();

        while (true) {
            Console.Write("> ");
            Console.Out.Flush();
            string? line = Console.ReadLine();
            if (line == null)
                break;
            interp.run(line);
        }

        return 0;
    }
}
