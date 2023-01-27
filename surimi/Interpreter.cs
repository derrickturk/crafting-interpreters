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
        var expr = Parser.Parse(Lexer.Lex(code, _onError), _onError);
        if (_onError.HadError)
            Console.Error.WriteLine("oh no");
        else
            Console.WriteLine(
              $"Expression: {expr!.Accept(new PrettyPrinter())}");
    }

    private ErrorReporter _onError;
}
