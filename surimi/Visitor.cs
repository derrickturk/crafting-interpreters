namespace Surimi;

public interface Visitor<T> {
    public T VisitLiteral(Literal e);
    public T VisitUnOpApp(UnOpApp e);
    public T VisitBinOpApp(BinOpApp e);
    public T VisitPrint(Print s);
    public T VisitExprStmt(ExprStmt s);
}
