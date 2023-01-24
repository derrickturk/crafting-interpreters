namespace Surimi;

public interface Visitor<T> {
    public T VisitLiteral(Literal lit);
}
