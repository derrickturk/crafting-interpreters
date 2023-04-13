namespace Surimi;

internal enum VariableState {
    Declared,
    Defined,
    Deferred,
}

internal enum FunctionKind {
    None,
    Function,
    Method,
}

internal class Resolver: Traverser {
    public Resolver(IEnumerable<string> globalBindings, ErrorReporter onError)
    {
        _onError = onError;
        _stateStack = new Stack<Dictionary<string, VariableState>>();
        _stateStack.Push(new Dictionary<string, VariableState>());
        _scopesOut = new Dictionary<Var, int>();
        _functionKind = FunctionKind.None;
        foreach (var name in globalBindings)
            Define(name);
    }

    public override ValueTuple VisitVar(Var v)
    {
        if (!AtGlobalScope && ScopeStates.ContainsKey(v.Name)
          && ScopeStates[v.Name] == VariableState.Declared) {
            _onError.Error(v.Location,
              $"cannot use variable {v.Name} in its own initializer");
        }
        ResolveVar(v);
        return ValueTuple.Create();
    }

    public override ValueTuple VisitAssign(Assign e)
    {
        e.Value.Accept(this);
        e.Value.Accept(this);
        return ValueTuple.Create();
    }

    public override ValueTuple VisitReturn(Return s)
    {
        if (_functionKind == FunctionKind.None)
            _onError.Error(s.Location, "return outside function/method body");
        if (s.Expression != null)
            s.Expression.Accept(this);
        return ValueTuple.Create();
    }

    public override ValueTuple VisitBlock(Block s)
    {
        PushScope();
        foreach (var stmt in s.Statements)
            stmt.Accept(this);
        PopScope();
        return ValueTuple.Create();
    }

    public override ValueTuple VisitVarDecl(VarDecl s)
    {
        Declare(s.Variable);
        if (s.Initializer != null)
            s.Initializer.Accept(this);
        Define(s.Variable.Name);
        return ValueTuple.Create();
    }

    public override ValueTuple VisitFunDef(FunDef s) 
    {
        Declare(s.Name);
        Define(s.Name.Name);
        var currentKind = _functionKind;
        _functionKind = FunctionKind.Function;
        ResolveFunDefOrMethod(s);
        _functionKind = currentKind;
        return ValueTuple.Create();
    }

    public override ValueTuple VisitClassDef(ClassDef s)
    {
        Declare(s.Name);
        Define(s.Name.Name);
        var currentKind = _functionKind;
        _functionKind = FunctionKind.Method;
        foreach (var method in s.Methods)
            ResolveFunDefOrMethod(method);
        _functionKind = currentKind;
        return ValueTuple.Create();
    }

    public static Dictionary<Var, int>? Resolve(List<Stmt> program,
      IEnumerable<string> globalBindings, ErrorReporter onError)
    {
        var r = new Resolver(globalBindings, onError);
        foreach (var stmt in program)
            stmt.Accept(r);
        if (onError.HadError)
            return null;
        return r.VariableScopesOut;
    }

    private void ResolveFunDefOrMethod(FunDef s)
    {
        PushScope();
        foreach (var p in s.Parameters)
            Define(p.Name);
        foreach (var stmt in s.Body)
            stmt.Accept(this);
        PopScope();
    }

    private bool AtGlobalScope => _stateStack.Count == 1;

    private Dictionary<string, VariableState> ScopeStates => _stateStack.Peek();

    private Dictionary<Var, int> VariableScopesOut => _scopesOut;

    // this one has to take Var, because it needs a Location to error at...
    private void Declare(Var v)
    {
        if (!AtGlobalScope && ScopeStates.ContainsKey(v.Name))
            _onError.Error(v.Location,
              $"variable {v.Name} already defined in this scope");
        ScopeStates[v.Name] = VariableState.Declared;
    }

    /* ...but this one has to take string, because it gets called on
     *   globals from "nowhere".
     * sigh.
     */
    private void Define(string v)
    {
        ScopeStates[v] = VariableState.Defined;
    }

    private void ResolveVar(Var v)
    {
        int i = 0;
        foreach (var frame in _stateStack) {
            if (frame.ContainsKey(v.Name)) {
                _scopesOut[v] = i;
                return;
            }
            ++i;

            if (i == _stateStack.Count) {
                /* so gross: the language semantics *require* us to defer variable
                 *   resolution to runtime, in case they're globals that get defined
                 *   later in the text; basically all global definitions are mutually
                 *   recursive and incremental.
                 * so this fallback amounts to: assume global and "defer"; we don't
                 *   do anything with the Deferred state yet, but we might... 
                 */

                /* so grosser: .NET Stack doesn't have random access, List
                 *   doesn't have push and pop, and there ain't a Deque.
                 * I don't feel like writing extension methods or wrappers,
                 *   so this is my hack to operate on the "bottom" of the stack;
                 *   i.e. the global frame
                 */
                _scopesOut[v] = i - 1;
                frame[v.Name] = VariableState.Deferred;
            }
        }

    }

    private void PushScope()
    {
        _stateStack.Push(new Dictionary<string, VariableState>());
    }

    private void PopScope()
    {
        if (_stateStack.Count <= 1)
            throw new InvalidOperationException(
              "internal error: attempted pop from global scope in resolver");
        _stateStack.Pop();
    }

    private ErrorReporter _onError;
    private Stack<Dictionary<string, VariableState>> _stateStack;
    private Dictionary<Var, int> _scopesOut;
    private FunctionKind _functionKind;
}
