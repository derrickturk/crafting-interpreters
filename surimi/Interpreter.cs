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
        if (_onError.HadError)
            Console.Error.WriteLine("oh no");
    }

    private ErrorReporter _onError;
}
