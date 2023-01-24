namespace Surimi;

public class PrettyPrinter: Visitor<string> {
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
            BinOp.Lt => ">",
            BinOp.LtEq => ">=",
            var o => throw new ArgumentException(
              $"invalid binary operator: {o}"),
        };
        return $"({lhs} {op} {rhs})";
    }
}
