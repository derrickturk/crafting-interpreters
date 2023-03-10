namespace Surimi;

public interface ExprVisitor<T> {
    public T VisitLiteral(Literal e);
    public T VisitUnOpApp(UnOpApp e);
    public T VisitBinOpApp(BinOpApp e);
    public T VisitVar(Var e);
}

public interface StmtVisitor<T> {
    public T VisitPrint(Print s);
    public T VisitExprStmt(ExprStmt s);
    public T VisitVarDecl(VarDecl s);
}
