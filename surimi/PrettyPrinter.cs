namespace Surimi;

using System.Text;

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
}

public class StmtPrettyPrinter: StmtVisitor<string> {
    public string VisitPrint(Print s) =>
      $"{Indent}print {s.Expression.Accept(_exprVisitor)};";

    public string VisitExprStmt(ExprStmt s) =>
      $"{Indent}{s.Expression.Accept(_exprVisitor)};";

    public string VisitBlock(Block s)
    {
        StringBuilder sb = new StringBuilder();
        sb.Append($"{Indent}{{\n");
        _indent += 2;
        foreach (var stmt in s.Statements) {
            sb.Append($"{stmt.Accept(this)}\n");
        }
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

    private string Indent => new string(' ', _indent);

    private ExprVisitor<string> _exprVisitor = new ExprPrettyPrinter();
    private int _indent = 0;
}
