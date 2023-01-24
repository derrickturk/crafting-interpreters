namespace Surimi;

// could be an interface, but I'm guess we're going to define some methods here
public abstract record class Expr {
    protected abstract T Accept<T>(Visitor<T> visitor);
}

public record class Literal (object Value): Expr () {
    protected override T Accept<T>(Visitor<T> visitor)
    {
        return visitor.VisitLiteral(this);
    }
}
