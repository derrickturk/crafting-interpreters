namespace Surimi;

using System;
using System.Linq;

public class Interpreter {
    public Interpreter(ErrorReporter onError)
    {
        _onError = onError;
        _evaluator = new ExprEvaluator();
        _executor = new StmtExecutor(_evaluator);
    }

    public void run(string code, string filename)
    {
        var prog = Parser.Parse(Lexer.Lex(code, filename, _onError), _onError);
        if (_onError.HadError)
            return;

        // null iff HadError, so ! is safe
        ExecuteStatements(prog!);
    }

    private void ExecuteStatements(List<Stmt> prog)
    {
        try {
            foreach (var stmt in prog)
                stmt.Accept(_executor);
        } catch (TypeError e) {
            _onError.Error(e.Location, e.Payload);
        }
    }

    private object? EvaluateExpression(Expr expr)
    {
        try {
            return expr.Accept(_evaluator);
        } catch (TypeError e) {
            _onError.Error(e.Location, e.Payload);
            return null;
        }
    }

    private static bool ValueTruthy(object? val) => val switch
    {
        null => false,
        false => false,
        _ => true,
    };

    private static bool ValueEquals(object? lhs, object? rhs) =>
        lhs == null ? rhs == null : lhs.Equals(rhs);

    private static string ValueString(object? val) => val switch
    {
        null => "nil",
        true => "true",
        false => "false",
        double d => d.ToString(),
        string s => s,
        _ => throw new InvalidOperationException("invalid runtime value"),
    };

    private static string ValueLiteral(object? val) => val switch
    {
        null => "nil",
        true => "true",
        false => "false",
        double d => d.ToString(),
        string s => $"\"{s}\"",
        _ => throw new InvalidOperationException("invalid runtime value"),
    };

    private class TypeError: Exception {
        public TypeError(SrcLoc location, string message)
        {
            Location = location;
            Payload = message;
        }

        public SrcLoc Location { get; init; }
        public string Payload { get; init; }
    }

    private class ExprEvaluator: ExprVisitor<object?> {
        public object? VisitLiteral(Literal e) => e.Value;

        public object VisitUnOpApp(UnOpApp e) =>
            !ValueTruthy(e.Operand.Accept(this));

        public object VisitBinOpApp(BinOpApp e)
        {
            // TODO: lazy cases check here

            return (e.Operator, e.Lhs.Accept(this), e.Rhs.Accept(this)) switch
            {
                (BinOp.Plus, double lhs, double rhs) => lhs + rhs,
                (BinOp.Plus, string lhs, string rhs) => lhs + rhs,
                (BinOp.Plus, double _, string _) =>
                    throw new TypeError(
                      e.Location, "operands to + must have matching types"),
                (BinOp.Plus, string _, double _) =>
                    throw new TypeError(
                      e.Location, "operands to + must have matching types"),
                (BinOp.Plus, _, _) =>
                    throw new TypeError(
                      e.Location, "operands to + must be numbers or strings"),

                (BinOp.Minus, double lhs, double rhs) => lhs - rhs,
                (BinOp.Minus, _, _) =>
                    throw new TypeError(
                      e.Location, "operands to - must be numbers"),

                (BinOp.Times, double lhs, double rhs) => lhs * rhs,
                (BinOp.Times, _, _) =>
                    throw new TypeError(
                      e.Location, "operands to * must be numbers"),

                (BinOp.DividedBy, double lhs, double rhs) => lhs / rhs,
                (BinOp.DividedBy, _, _) =>
                    throw new TypeError(
                      e.Location, "operands to / must be numbers"),

                (BinOp.EqEq, var lhs, var rhs) => ValueEquals(lhs, rhs),
                (BinOp.NotEq, var lhs, var rhs) => !ValueEquals(lhs, rhs),

                (BinOp.Lt, double lhs, double rhs) => lhs < rhs,
                (BinOp.Lt, _, _) =>
                    throw new TypeError(
                      e.Location, "operands to < must be numbers"),

                (BinOp.LtEq, double lhs, double rhs) => lhs <= rhs,
                (BinOp.LtEq, _, _) =>
                    throw new TypeError(
                      e.Location, "operands to <= must be numbers"),

                (BinOp.Gt, double lhs, double rhs) => lhs > rhs,
                (BinOp.Gt, _, _) =>
                    throw new TypeError(
                      e.Location, "operands to > must be numbers"),

                (BinOp.GtEq, double lhs, double rhs) => lhs >= rhs,
                (BinOp.GtEq, _, _) =>
                    throw new TypeError(
                      e.Location, "operands to >= must be numbers"),

                _ => throw new InvalidOperationException(
                  "invalid binary operator"),
            };
        }
    }

    private class StmtExecutor: StmtVisitor<ValueTuple> {
        public StmtExecutor(ExprEvaluator evaluator)
        {
            _evaluator = evaluator;
        }

        public ValueTuple VisitPrint(Print s)
        {
            Console.WriteLine(ValueString(s.Expression.Accept(_evaluator)));
            return ValueTuple.Create();
        }

        public ValueTuple VisitExprStmt(ExprStmt s)
        {
            s.Expression.Accept(_evaluator);
            return ValueTuple.Create();
        }

        private ExprEvaluator _evaluator;
    }

    private ErrorReporter _onError;
    private ExprEvaluator _evaluator;
    private StmtExecutor _executor;
}
