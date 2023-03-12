namespace Surimi;

using System;
using System.Linq;

public class Interpreter {
    public Interpreter(ErrorReporter onError)
    {
        _onError = onError;
        _env = new Environment();
        _evaluator = new ExprEvaluator(_env);
        _executor = new StmtExecutor(_env, _evaluator);
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
        } catch (RuntimeError e) {
            _onError.Error(e.Location, e.Payload);
        }
    }

    private object? EvaluateExpression(Expr expr)
    {
        try {
            return expr.Accept(_evaluator);
        } catch (RuntimeError e) {
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

    private class ExprEvaluator: ExprVisitor<object?> {
        public ExprEvaluator(Environment env)
        {
            _env = env;
        }

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
                    throw new RuntimeError(
                      e.Location, "operands to + must have matching types"),
                (BinOp.Plus, string _, double _) =>
                    throw new RuntimeError(
                      e.Location, "operands to + must have matching types"),
                (BinOp.Plus, _, _) =>
                    throw new RuntimeError(
                      e.Location, "operands to + must be numbers or strings"),

                (BinOp.Minus, double lhs, double rhs) => lhs - rhs,
                (BinOp.Minus, _, _) =>
                    throw new RuntimeError(
                      e.Location, "operands to - must be numbers"),

                (BinOp.Times, double lhs, double rhs) => lhs * rhs,
                (BinOp.Times, _, _) =>
                    throw new RuntimeError(
                      e.Location, "operands to * must be numbers"),

                (BinOp.DividedBy, double lhs, double rhs) => lhs / rhs,
                (BinOp.DividedBy, _, _) =>
                    throw new RuntimeError(
                      e.Location, "operands to / must be numbers"),

                (BinOp.EqEq, var lhs, var rhs) => ValueEquals(lhs, rhs),
                (BinOp.NotEq, var lhs, var rhs) => !ValueEquals(lhs, rhs),

                (BinOp.Lt, double lhs, double rhs) => lhs < rhs,
                (BinOp.Lt, _, _) =>
                    throw new RuntimeError(
                      e.Location, "operands to < must be numbers"),

                (BinOp.LtEq, double lhs, double rhs) => lhs <= rhs,
                (BinOp.LtEq, _, _) =>
                    throw new RuntimeError(
                      e.Location, "operands to <= must be numbers"),

                (BinOp.Gt, double lhs, double rhs) => lhs > rhs,
                (BinOp.Gt, _, _) =>
                    throw new RuntimeError(
                      e.Location, "operands to > must be numbers"),

                (BinOp.GtEq, double lhs, double rhs) => lhs >= rhs,
                (BinOp.GtEq, _, _) =>
                    throw new RuntimeError(
                      e.Location, "operands to >= must be numbers"),

                _ => throw new InvalidOperationException(
                  "invalid binary operator"),
            };
        }

        public object? VisitVar(Var e)
        {
            return _env[e];
        }

        public object? VisitAssign(Assign e)
        {
            var val = e.Value.Accept(this);
            _env[e.Variable] = val;
            return val;
        }

        private Environment _env;
    }

    private class StmtExecutor: StmtVisitor<ValueTuple> {
        public StmtExecutor(Environment env, ExprEvaluator evaluator)
        {
            _env = env;
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

        public ValueTuple VisitBlock(Block s)
        {
            _env.PushScope();
            try {
                foreach (var stmt in s.Statements)
                    stmt.Accept(this);
            } finally {
                _env.PopScope();
            }
            return ValueTuple.Create();
        }

        public ValueTuple VisitVarDecl(VarDecl s)
        {
            _env.Declare(s.Variable, s.Initializer?.Accept(_evaluator));
            return ValueTuple.Create();
        }

        private Environment _env;
        private ExprEvaluator _evaluator;
    }

    private ErrorReporter _onError;
    private Environment _env;
    private ExprEvaluator _evaluator;
    private StmtExecutor _executor;
}

public class Environment {
    public Environment()
    {
        _locals = new Dictionary<string, object?>();
        _parent = null;
    }

    // clone, for use in push/pop
    private Environment(Environment other)
    {
        _locals = other._locals;
        _parent = other._parent;
    }

    // enter a new lexical scope
    public void PushScope()
    {
        var me = new Environment(this);
        _locals = new Dictionary<string, object?>();
        _parent = me;
    }

    // exit a scope
    public void PopScope()
    {
        if (_parent == null)
            throw new InvalidOperationException(
              "internal error: pop from global scope");
        _locals = _parent._locals;
        _parent = _parent._parent;
    }

    public object? this[Var variable]
    {
        get
        {
            if (_locals.ContainsKey(variable.Name))
                return _locals[variable.Name];
            if (_parent != null)
                return _parent[variable];
            throw new RuntimeError(variable.Location,
              $"undefined variable {variable.Name}");
        }

        set
        {
            if (_locals.ContainsKey(variable.Name))
                _locals[variable.Name] = value;
            else if (_parent != null)
                _parent[variable] = value;
            else
                throw new RuntimeError(variable.Location,
                  $"undefined variable {variable.Name}");
        }
    }

    public void Declare(Var variable, object? initializer)
    {
        _locals[variable.Name] = initializer;
    }

    public Environment? Parent => _parent;

    private Dictionary<string, object?> _locals;
    private Environment? _parent;
}

internal class RuntimeError: Exception {
    public RuntimeError(SrcLoc location, string message)
    {
        Location = location;
        Payload = message;
    }

    public SrcLoc Location { get; init; }
    public string Payload { get; init; }
}
