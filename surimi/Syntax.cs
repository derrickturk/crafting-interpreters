namespace Surimi;

public enum UnOp {
    Negate,
    Not,
}

public enum BinOp {
    Minus, Plus, DividedBy, Times,
    NotEq, EqEq, Gt, GtEq, Lt, LtEq,
    And, Or,
}

public abstract record class Expr (SrcLoc Location) {
    public abstract T Accept<T>(ExprVisitor<T> visitor);
    public string PrettyPrint() => Accept(new ExprPrettyPrinter());
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

public record class Var (String Name, SrcLoc Location): Expr (Location) {
    public override T Accept<T>(ExprVisitor<T> visitor)
    {
        return visitor.VisitVar(this);
    }
}

public record class Assign (Var Variable, Expr Value, SrcLoc Location)
  : Expr (Location) {
    public override T Accept<T>(ExprVisitor<T> visitor)
    {
        return visitor.VisitAssign(this);
    }
}

public record class Call (Expr Callee, List<Expr> Arguments, SrcLoc Location)
  : Expr (Location) {
    public override T Accept<T>(ExprVisitor<T> visitor)
    {
        return visitor.VisitCall(this);
    }
}

public record class PropertyGet (Expr Object, Var Name, SrcLoc Location)
  : Expr (Location) {
    public override T Accept<T>(ExprVisitor<T> visitor)
    {
        return visitor.VisitPropertyGet(this);
    }
}

public record class PropertySet (Expr Object, Var Name, Expr Value,
  SrcLoc Location): Expr (Location) {
    public override T Accept<T>(ExprVisitor<T> visitor)
    {
        return visitor.VisitPropertySet(this);
    }
}

public record class This (SrcLoc Location): Expr (Location) {
    public override T Accept<T>(ExprVisitor<T> visitor)
    {
        return visitor.VisitThis(this);
    }
}

public abstract record class Stmt (SrcLoc Location) {
    public abstract T Accept<T>(StmtVisitor<T> visitor);
    public string PrettyPrint() => Accept(new StmtPrettyPrinter());
}

public record class ExprStmt (Expr Expression, SrcLoc Location)
  : Stmt (Location) {
    public override T Accept<T>(StmtVisitor<T> visitor)
    {
        return visitor.VisitExprStmt(this);
    }
}

public record class IfElse (Expr Condition, Stmt If, Stmt? Else,
  SrcLoc Location): Stmt (Location) {
    public override T Accept<T>(StmtVisitor<T> visitor)
    {
        return visitor.VisitIfElse(this);
    }
}

public record class While (Expr Condition, Stmt Body, SrcLoc Location)
  : Stmt (Location) {
    public override T Accept<T>(StmtVisitor<T> visitor)
    {
        return visitor.VisitWhile(this);
    }
}

public record class Print (Expr Expression, SrcLoc Location): Stmt (Location) {
    public override T Accept<T>(StmtVisitor<T> visitor)
    {
        return visitor.VisitPrint(this);
    }
}

public record class Return (Expr? Expression, SrcLoc Location)
  : Stmt (Location) {
    public override T Accept<T>(StmtVisitor<T> visitor)
    {
        return visitor.VisitReturn(this);
    }
}

public record class Block (List<Stmt> Statements, SrcLoc Location)
  : Stmt (Location) {
    public override T Accept<T>(StmtVisitor<T> visitor)
    {
        return visitor.VisitBlock(this);
    }
}

public record class VarDecl (Var Variable, Expr? Initializer, SrcLoc Location)
  : Stmt (Location) {
    public override T Accept<T>(StmtVisitor<T> visitor)
    {
        return visitor.VisitVarDecl(this);
    }
}

public record class FunDef (Var Name, List<Var> Parameters, List<Stmt> Body,
  SrcLoc Location): Stmt (Location) {
    public override T Accept<T>(StmtVisitor<T> visitor)
    {
        return visitor.VisitFunDef(this);
    }
}

public record class ClassDef (Var Name, Var? Super, List<FunDef> Methods,
  SrcLoc Location): Stmt (Location) {
    public override T Accept<T>(StmtVisitor<T> visitor)
    {
        return visitor.VisitClassDef(this);
    }
}
