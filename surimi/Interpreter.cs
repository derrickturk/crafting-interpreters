namespace Surimi;

using System;
using System.Linq;

public class Interpreter {
    public Interpreter(ErrorReporter onError)
    {
        _onError = onError;
    }

    public void run(string code)
    {
        var toks = Lexer.Lex(code, _onError).ToList();
        foreach (var t in toks)
            Console.WriteLine($"beep boop: {t}");
    }

    private ErrorReporter _onError;
}
