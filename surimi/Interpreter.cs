namespace Surimi;

using System;

public class Interpreter {
    public Interpreter(ErrorReporter onError)
    {
        _onError = onError;
        _globals = new Environment();
        RegisterBuiltins();
    }

    public void Run(string code, string filename)
    {
        var prog = Parser.Parse(Lexer.Lex(code, filename, _onError), _onError);
        if (_onError.HadError)
            return;

        // null iff HadError, so ! is safe
        var scopesOut = Resolver.Resolve(prog!,
          _globals.LocalBindings, _onError);
        if (_onError.HadError)
            return;

        var visitor = new EvalExecVisitor(_globals, scopesOut!);
        visitor.ExecuteStatements(prog!, _onError);
    }

    internal static bool ValueTruthy(object? val) => val switch
    {
        null => false,
        false => false,
        _ => true,
    };

    internal static bool ValueEquals(object? lhs, object? rhs) =>
        lhs == null ? rhs == null : lhs.Equals(rhs);

    internal static string ValueString(object? val) => val switch
    {
        null => "nil",
        true => "true",
        false => "false",
        double d => d.ToString(),
        string s => s,
        _ => throw new InvalidOperationException("invalid runtime value"),
    };

    internal static string ValueLiteral(object? val) => val switch
    {
        null => "nil",
        true => "true",
        false => "false",
        double d => d.ToString(),
        string s => $"\"{s}\"",
        _ => throw new InvalidOperationException("invalid runtime value"),
    };

    private void RegisterBuiltins()
    {
        foreach (var builtin in Builtin.BuiltinFunctions)
            _globals.Declare(new Var(builtin.Name, Builtin.BuiltinLocation),
              builtin);
    }

    private class EvalExecVisitor
      : ExprVisitor<object?>, StmtVisitor<ValueTuple> {
        public EvalExecVisitor(Environment env,
          Dictionary<Var, int> variableScopesOut)
        {
            _env = env;
            _scopesOut = variableScopesOut;
        }

        public void ExecuteStatements(List<Stmt> prog, ErrorReporter onError)
        {
            try {
                foreach (var stmt in prog)
                    stmt.Accept(this);
            } catch (RuntimeError e) {
                onError.Error(e.Location, e.Payload);
            }
        }

        public object? EvaluateExpression(Expr expr, ErrorReporter onError)
        {
            try {
                return expr.Accept(this);
            } catch (RuntimeError e) {
                onError.Error(e.Location, e.Payload);
                return null;
            }
        }

        public object? VisitLiteral(Literal e) => e.Value;

        public object VisitUnOpApp(UnOpApp e) =>
            !ValueTruthy(e.Operand.Accept(this));

        public object? VisitBinOpApp(BinOpApp e)
        {
            {
                object? lhs;
                // check lazy cases first
                switch (e.Operator) {
                    case BinOp.And:
                        lhs = e.Lhs.Accept(this);
                        if (!ValueTruthy(lhs))
                            return lhs;
                        return e.Rhs.Accept(this);
                    case BinOp.Or:
                        lhs = e.Lhs.Accept(this);
                        if (ValueTruthy(lhs))
                            return lhs;
                        return e.Rhs.Accept(this);
                    default:
                        break;
                }
            }

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
            return _env[e, _scopesOut[e]];
        }

        public object? VisitAssign(Assign e)
        {
            var val = e.Value.Accept(this);
            _env[e.Variable] = val;
            return val;
        }

        public object? VisitCall(Call e)
        {
            if (e.Callee.Accept(this) is Callable fn) {
                var args = e.Arguments.Select(a => a.Accept(this)).ToList();
                if (args.Count != fn.Arity)
                    throw new RuntimeError(e.Location,
                      $"expected {fn.Arity} arguments but got {args.Count}");
                return fn.Call(args);
            } else {
                throw new RuntimeError(
                  e.Callee.Location, "call on non-callable value");
            }
        }

        public ValueTuple VisitExprStmt(ExprStmt s)
        {
            s.Expression.Accept(this);
            return ValueTuple.Create();
        }

        public ValueTuple VisitIfElse(IfElse s)
        {
            if (ValueTruthy(s.Condition.Accept(this)))
                s.If.Accept(this);
            else if (s.Else != null)
                s.Else.Accept(this);
            return ValueTuple.Create();
        }

        public ValueTuple VisitWhile(While s)
        {
            while (ValueTruthy(s.Condition.Accept(this)))
                s.Body.Accept(this);
            return ValueTuple.Create();
        }

        public ValueTuple VisitPrint(Print s)
        {
            Console.WriteLine(ValueString(s.Expression.Accept(this)));
            return ValueTuple.Create();
        }

        public ValueTuple VisitReturn(Return s)
        {
            throw new ReturnOrError(s.Expression.Accept(this), s.Location);
        }

        public ValueTuple VisitBlock(Block s)
        {
            var innerVisitor = new EvalExecVisitor(
              new Environment(_env), _scopesOut);
            foreach (var stmt in s.Statements)
                stmt.Accept(innerVisitor);
            return ValueTuple.Create();
        }

        public ValueTuple VisitVarDecl(VarDecl s)
        {
            _env.Declare(s.Variable, s.Initializer?.Accept(this));
            return ValueTuple.Create();
        }

        public ValueTuple VisitFunDef(FunDef s)
        {
            _env.Declare(s.Name, new LoxFunction(s, _env, _scopesOut));
            return ValueTuple.Create();
        }

        private Environment _env;
        private Dictionary<Var, int> _scopesOut;
    }

    private record class LoxFunction(FunDef Definition, Environment Env,
      Dictionary<Var, int> VariableScopesOut): Callable {
        public int Arity => Definition.Parameters.Count;

        public object? Call(List<object?> arguments)
        {
            var frameEnv = new Environment(Env);
            foreach (var (param, arg) in Definition.Parameters.Zip(arguments))
                frameEnv.Declare(param, arg);
            var frameVisitor = new EvalExecVisitor(frameEnv, VariableScopesOut);
            try {
                foreach (var s in Definition.Body)
                    s.Accept(frameVisitor);
            } catch (ReturnOrError r) {
                return r.Result;
            }
            return null;
        }

        public override string ToString() => $"<function {Definition.Name.Name}>";
    }

    private ErrorReporter _onError;
    private Environment _globals;
}

public class Environment {
    public Environment(Environment? parent = null)
    {
        _locals = new Dictionary<string, object?>();
        _parent = parent;
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

    public object? this[Var variable, int depth]
    {
        get
        {
            if (depth <= 0)
                return this[variable];
            if (_parent == null)
                throw new InvalidOperationException(
                  "internal error: stack mismatch with resolved depth");
            return this._parent[variable, depth - 1];
        }

        set
        {
            if (depth <= 0)
                this[variable] = value;
            this[variable, depth - 1] = value;
        }
    }

    public void Declare(Var variable, object? initializer)
    {
        _locals[variable.Name] = initializer;
    }

    public Environment? Parent => _parent;

    public IEnumerable<string> LocalBindings => _locals.Keys;

    public IEnumerable<string> AllBindings
    {
        get
        {
            foreach (var name in LocalBindings)
                yield return name;
            if (Parent != null)
                foreach (var name in Parent.AllBindings)
                    yield return name;
        }
    }

    private Dictionary<string, object?> _locals;
    private Environment? _parent;
}

public interface Callable {
    public int Arity { get; }
    public object? Call(List<object?> arguments);
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

internal class ReturnOrError: RuntimeError {
    public ReturnOrError(object? result, SrcLoc location)
      : base(location, "\"return\" outside function body")
    {
        Result = result;
    }

    public object? Result { get; init; }
}
