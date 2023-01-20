namespace Surimi;

using System;
using System.Linq;

public class Interpreter {
    public void run(string code)
    {
        var toks = Lexer.Lex(code).ToList();
        foreach (var t in toks)
            Console.WriteLine($"beep boop: {t}");
    }

    public bool HadError { get; protected set; }

    protected void Error(int line, string msg)
    {
        Report(line, "", msg);
        HadError = true;
    }

    protected void Report(int line, string whurr, string msg)
    {
        Console.Error.WriteLine($"[line {line}] Error{whurr}: {msg}");
    }
}
