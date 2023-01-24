namespace Surimi;

public enum UnOp {
    Negate,
    Not,
}

public enum BinOp {
    // Comma, Member, // for later
    Minus, Plus, DividedBy, Times,
    NotEq, EqEq, Gt, GtEq, Lt, LtEq,
    // And, Or, // for later
}

// could be an interface, but I'm guess we're going to define some methods here
public abstract record class Expr {
    public abstract T Accept<T>(Visitor<T> visitor);
}

public record class Literal (object? Value): Expr () {
    public override T Accept<T>(Visitor<T> visitor)
    {
        return visitor.VisitLiteral(this);
    }
}

public record class UnOpApp (UnOp Operator, Expr Operand): Expr () {
    public override T Accept<T>(Visitor<T> visitor)
    {
        return visitor.VisitUnOpApp(this);
    }
}

public record class BinOpApp (BinOp Operator, Expr Lhs, Expr Rhs): Expr () {
    public override T Accept<T>(Visitor<T> visitor)
    {
        return visitor.VisitBinOpApp(this);
    }
}
