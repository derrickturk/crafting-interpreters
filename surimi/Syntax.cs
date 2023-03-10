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

// could be an interface, but I'd guess we're going to define some methods here
public abstract record class Expr (SrcLoc Location) {
    public abstract T Accept<T>(ExprVisitor<T> visitor);
}

public record class Literal (object? Value, SrcLoc Location): Expr (Location) {
    public override T Accept<T>(ExprVisitor<T> visitor)
    {
        return visitor.VisitLiteral(this);
    }
}

// location = location of the operator token (to match the book)
public record class UnOpApp (UnOp Operator, Expr Operand, SrcLoc Location)
  : Expr (Location) {
    public override T Accept<T>(ExprVisitor<T> visitor)
    {
        return visitor.VisitUnOpApp(this);
    }
}

// location = location of the operator token (to match the book)
public record class BinOpApp
  (BinOp Operator, Expr Lhs, Expr Rhs, SrcLoc Location): Expr (Location) {
    public override T Accept<T>(ExprVisitor<T> visitor)
    {
        return visitor.VisitBinOpApp(this);
    }
}

public abstract record class Stmt (SrcLoc Location) {
    public abstract T Accept<T>(StmtVisitor<T> visitor);
}

public record class Print (Expr Expression, SrcLoc Location) : Stmt (Location) {
    public override T Accept<T>(StmtVisitor<T> visitor)
    {
        return visitor.VisitPrint(this);
    }
}

public record class ExprStmt (Expr Expression, SrcLoc Location)
  : Stmt (Location) {
    public override T Accept<T>(StmtVisitor<T> visitor)
    {
        return visitor.VisitExprStmt(this);
    }
}
