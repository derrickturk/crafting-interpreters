namespace Surimi;

using System.Text;
using System.Linq;

public class ExprPrettyPrinter: ExprVisitor<string> {
    public string VisitLiteral(Literal e) => e.Value switch
    {
        null => "nil",
        true => "true",
        false => "false",
        double d => d.ToString(),
        string s => $"\"{s}\"",
        _ => throw new ArgumentException($"bad literal: {e}"),
    };

    public string VisitUnOpApp(UnOpApp e) => e.Operator switch
    {
        UnOp.Negate => $"-{e.Operand.Accept(this)}",
        UnOp.Not => $"!{e.Operand.Accept(this)}",
        var o => throw new ArgumentException($"invalid unary operator: {o}"),
    };

    public string VisitBinOpApp(BinOpApp e)
    {
        var lhs = e.Lhs.Accept(this);
        var rhs = e.Rhs.Accept(this);
        var op = e.Operator switch
        {
            BinOp.Minus => "-",
            BinOp.Plus => "+",
            BinOp.DividedBy => "/",
            BinOp.Times => "*",
            BinOp.NotEq => "!=",
            BinOp.EqEq => "==",
            BinOp.Gt => ">",
            BinOp.GtEq => ">=",
            BinOp.Lt => "<",
            BinOp.LtEq => "<=",
            BinOp.And => "and",
            BinOp.Or => "or",
            var o => throw new ArgumentException(
              $"invalid binary operator: {o}"),
        };
        return $"({lhs} {op} {rhs})";
    }

    public string VisitVar(Var e)
    {
        return e.Name;
    }

    public string VisitAssign(Assign e)
    {
        return $"{e.Variable.Name} = {e.Value.Accept(this)}";
    }

    public string VisitCall(Call e)
    {
        var args = String.Join(", ", e.Arguments.Select(a => a.Accept(this)));
        return $"{e.Callee.Accept(this)}({args})";
    }
}

public class StmtPrettyPrinter: StmtVisitor<string> {
    public string VisitExprStmt(ExprStmt s) =>
      $"{Indent}{s.Expression.Accept(_exprVisitor)};";

    public string VisitIfElse(IfElse s)
    {
        StringBuilder sb = new StringBuilder();
        sb.Append($"{Indent}if ({s.Condition.Accept(_exprVisitor)})\n");
        _indent += 2;
        sb.Append(s.If.Accept(this));
        _indent -= 2;
        if (s.Else != null) {
            sb.Append("\n{Indent}else\n");
            _indent += 2;
            sb.Append(s.Else.Accept(this));
            _indent -= 2;
        }
        sb.Append("\n");
        return sb.ToString();
    }

    public string VisitWhile(While s)
    {
        StringBuilder sb = new StringBuilder();
        sb.Append($"{Indent}if ({s.Condition.Accept(_exprVisitor)})\n");
        _indent += 2;
        sb.Append(s.Body.Accept(this));
        _indent -= 2;
        sb.Append("\n");
        return sb.ToString();
    }

    public string VisitPrint(Print s) =>
      $"{Indent}print {s.Expression.Accept(_exprVisitor)};";

    public string VisitReturn(Return s)
    {
        if (s.Expression == null)
            return $"{Indent}return;";
        return $"{Indent}return {s.Expression.Accept(_exprVisitor)};";
    }

    public string VisitBlock(Block s)
    {
        StringBuilder sb = new StringBuilder();
        sb.Append($"{Indent}{{\n");
        _indent += 2;
        foreach (var stmt in s.Statements)
            sb.Append($"{stmt.Accept(this)}\n");
        _indent -= 2;
        sb.Append($"{Indent}}}");
        return sb.ToString();
    }

    public string VisitVarDecl(VarDecl s)
    {
        string initializer = "";
        if (s.Initializer != null) {
            initializer = $" = {s.Initializer.Accept(_exprVisitor)}";
        }
        return $"{Indent}var {s.Variable.Name}{initializer};";
    }

    public string VisitFunDef(FunDef s) => VisitFunDef(s, true);

    public string VisitFunDef(FunDef s, bool freeFunction)
    {
        StringBuilder sb = new StringBuilder();
        string prefix = freeFunction ? "fun " : "";
        sb.Append($"{Indent}{prefix}{s.Name.Name} (");
        string sep = "";
        foreach (var p in s.Parameters) {
            sb.Append($"{sep}{p.Name}");
            sep = ", ";
        }
        sb.Append(") {\n");
        _indent += 2;
        foreach (var stmt in s.Body)
            sb.Append($"{stmt.Accept(this)}\n");
        _indent -= 2;
        sb.Append("}");
        return sb.ToString();
    }

    public string VisitClassDef(ClassDef s)
    {
        StringBuilder sb = new StringBuilder();
        sb.Append($"class {s.Name} {{\n");
        _indent += 2;
        foreach (var meth in s.Methods) {
            sb.Append(VisitFunDef(meth, false));
            sb.Append("\n");
        }
        _indent -= 2;
        sb.Append("}");
        return sb.ToString();
    }

    private string Indent => new string(' ', _indent);

    private ExprVisitor<string> _exprVisitor = new ExprPrettyPrinter();
    private int _indent = 0;
}
