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
        var expr = Parser.Parse(Lexer.Lex(code, _onError), _onError);
        if (!_onError.HadError) {
            Console.WriteLine(
              $"Expression: {expr!.Accept(new PrettyPrinter())}");
            return;
        }
    }

    private ErrorReporter _onError;

    private class TypeError: Exception {
        public TypeError(string message)
        {
            Payload = message;
        }

        public string Payload { get; init; }
    }

    private class DivZeroError: Exception { }

    private class ExprEvaluator: Visitor<object?> {
        public object? VisitLiteral(Literal e) => e.Value;

        public object VisitUnOpApp(UnOpApp e)
        {
            var operand = e.Operand.Accept(this);
            switch (e.Operator)
            {
                case UnOp.Not:
                    if (operand is bool b)
                        return !b;
                    throw new TypeError("operand to ! must be boolean");

                case UnOp.Negate:
                    if (operand is double d)
                        return -d;
                    throw new TypeError("operand to - must be number");
            }
        }

        public object VisitBinOpApp(BinOpApp e)
        {
            // TODO: lazy cases check here

            var lhs = e.Lhs.Accept(this);
            var rhs = e.Rhs.Accept(this);

            switch (e.Operator)
            {
                case BinOp.Plus:
                    if (lhs is double dlhs && rhs is double drhs)
                        return dlhs + drhs;
                    if (lhs is string slhs && rhs is string srhs)
                        return slhs + srhs;
                    throw new TypeError(
                      "operands to + must be both numbers or both strings");

                case BinOp.Minus:
                    if (lhs is double dlhs && rhs is double drhs)
                        return dlhs - drhs;
                    throw new TypeError("operands to - must be numbers");

                case BinOp.Times:
                    if (lhs is double dlhs && rhs is double drhs)
                        return dlhs * drhs;
                    throw new TypeError("operands to * must be numbers");

                case BinOp.DividedBy:
                    if (lhs is double dlhs && rhs is double drhs) {
                        if (drhs == 0.0)
                            throw new DivZeroError();
                        return lhs / rhs;
                    }
                    throw new TypeError("operands to / must be numbers");

                case BinOp.Lt:
                    if (lhs is double dlhs && rhs is double drhs)
                        return dlhs < drhs;
                    throw new TypeError("operands to < must be numbers");

                case BinOp.LtEq:
                    if (lhs is double dlhs && rhs is double drhs)
                        return dlhs <= drhs;
                    throw new TypeError("operands to <= must be numbers");

                case BinOp.Gt:
                    if (lhs is double dlhs && rhs is double drhs)
                        return dlhs > drhs;
                    throw new TypeError("operands to > must be numbers");

                case BinOp.GtEq:
                    if (lhs is double dlhs && rhs is double drhs)
                        return dlhs >= drhs;
                    throw new TypeError("operands to >= must be numbers");
            }
        }
    }
}
