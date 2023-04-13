namespace Surimi;

public interface ExprVisitor<T> {
    public T VisitLiteral(Literal e);
    public T VisitUnOpApp(UnOpApp e);
    public T VisitBinOpApp(BinOpApp e);
    public T VisitVar(Var e);
    public T VisitAssign(Assign e);
    public T VisitCall(Call e);
    public T VisitPropertyGet(PropertyGet e);
    public T VisitPropertySet(PropertySet e);
}

public interface StmtVisitor<T> {
    public T VisitExprStmt(ExprStmt s);
    public T VisitIfElse(IfElse s);
    public T VisitWhile(While s);
    public T VisitPrint(Print s);
    public T VisitReturn(Return s);
    public T VisitBlock(Block s);
    public T VisitVarDecl(VarDecl s);
    public T VisitFunDef(FunDef s);
    public T VisitClassDef(ClassDef s);
}

// a "template" traversal which just visits things and does nothing
public class Traverser: ExprVisitor<ValueTuple>, StmtVisitor<ValueTuple> {
    public virtual ValueTuple VisitLiteral(Literal e) => ValueTuple.Create();

    public virtual ValueTuple VisitUnOpApp(UnOpApp e) => e.Operand.Accept(this);

    public virtual ValueTuple VisitBinOpApp(BinOpApp e)
    {
        e.Lhs.Accept(this);
        e.Rhs.Accept(this);
        return ValueTuple.Create();
    }

    public virtual ValueTuple VisitVar(Var e) => ValueTuple.Create();

    public virtual ValueTuple VisitAssign(Assign e)
    {
        e.Variable.Accept(this);
        e.Value.Accept(this);
        return ValueTuple.Create();
    }

    public virtual ValueTuple VisitPropertyGet(PropertyGet e)
    {
        e.Object.Accept(this);
        return ValueTuple.Create();
    }

    public virtual ValueTuple VisitPropertySet(PropertySet e)
    {
        e.Object.Accept(this);
        e.Value.Accept(this);
        return ValueTuple.Create();
    }

    public virtual ValueTuple VisitCall(Call e)
    {
        e.Callee.Accept(this);
        foreach (var expr in e.Arguments)
            expr.Accept(this);
        return ValueTuple.Create();
    }

    public virtual ValueTuple VisitExprStmt(ExprStmt s)
    {
        s.Expression.Accept(this);
        return ValueTuple.Create();
    }

    public virtual ValueTuple VisitIfElse(IfElse s)
    {
        s.Condition.Accept(this);
        s.If.Accept(this);
        if (s.Else != null)
            s.Else.Accept(this);
        return ValueTuple.Create();
    }

    public virtual ValueTuple VisitWhile(While s)
    {
        s.Condition.Accept(this);
        s.Body.Accept(this);
        return ValueTuple.Create();
    }

    public virtual ValueTuple VisitPrint(Print s)
    {
        s.Expression.Accept(this);
        return ValueTuple.Create();
    }

    public virtual ValueTuple VisitReturn(Return s)
    {
        if (s.Expression != null)
            s.Expression.Accept(this);
        return ValueTuple.Create();
    }

    public virtual ValueTuple VisitBlock(Block s)
    {
        foreach (var stmt in s.Statements)
            stmt.Accept(this);
        return ValueTuple.Create();
    }

    public virtual ValueTuple VisitVarDecl(VarDecl s)
    {
        s.Variable.Accept(this);
        if (s.Initializer != null)
            s.Initializer.Accept(this);
        return ValueTuple.Create();
    }

    public virtual ValueTuple VisitFunDef(FunDef s)
    {
        s.Name.Accept(this);
        foreach (var p in s.Parameters)
            p.Accept(this);
        foreach (var stmt in s.Body)
            stmt.Accept(this);
        return ValueTuple.Create();
    }

    public virtual ValueTuple VisitClassDef(ClassDef s)
    {
        s.Name.Accept(this);
        foreach (var meth in s.Methods)
            meth.Accept(this);
        return ValueTuple.Create();
    }
}
