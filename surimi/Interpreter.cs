namespace Surimi;

using System;
using System.Linq;

public class Interpreter {
    public Interpreter(ErrorReporter onError)
    {
        _onError = onError;
    }

    public void run(string code, string filename)
    {
        var expr = Parser.Parse(Lexer.Lex(code, filename, _onError), _onError);
        if (_onError.HadError)
            return;

        object? v = EvaluateExpression(expr!);
        if (_onError.HadError)
            return;

        Console.WriteLine(ValueString(v));
    }

    private object? EvaluateExpression(Expr expr)
    {
        try {
            return expr.Accept(new ExprEvaluator());
        } catch (TypeError e) {
            _onError.Error(e.Location, e.Payload);
            return null;
        } catch (DivZeroError) {
            _onError.Error(expr.Location, "division by zero");
            return null;
        }
    }

    private ErrorReporter _onError;

    private static bool ValueEquals(object? lhs, object? rhs) =>
        lhs == null ? rhs == null : lhs.Equals(rhs);

    private static string ValueString(object? val) => val switch
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

    private class DivZeroError: Exception { }

    private class ExprEvaluator: Visitor<object?> {
        public object? VisitLiteral(Literal e) => e.Value;

        public object VisitUnOpApp(UnOpApp e) =>
            (e.Operator, e.Operand.Accept(this)) switch
            {
                (UnOp.Not, bool b) => !b,
                (UnOp.Not, _) =>
                    throw new TypeError(
                      e.Location, "operand to ! must be boolean"),
                (UnOp.Negate, double d) => -d,
                (UnOp.Negate, _) =>
                    throw new TypeError(
                      e.Location, "operand to - must be number"),
                _ => throw new InvalidOperationException(
                  "invalid unary operator"),
            };

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

                (BinOp.DividedBy, double _, 0.0) => throw new DivZeroError(),
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
}
