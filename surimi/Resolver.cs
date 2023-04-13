namespace Surimi;

internal enum VariableState {
    Declared,
    Defined,
    Deferred,
}

internal enum FunctionKind {
    None,
    Function,
    Initializer,
    Method,
}

internal enum ClassKind {
    None,
    Class,
    SubClass,
}

internal class Resolver: Traverser {
    public Resolver(IEnumerable<string> globalBindings, ErrorReporter onError)
    {
        _onError = onError;
        _stateStack = new Stack<Dictionary<string, VariableState>>();
        _stateStack.Push(new Dictionary<string, VariableState>());
        _scopesOut = new Dictionary<Expr, int>();
        _functionKind = FunctionKind.None;
        _classKind = ClassKind.None;
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
        ResolveVarLike(v, v.Name);
        return ValueTuple.Create();
    }

    public override ValueTuple VisitThis(This e)
    {
        if (_classKind == ClassKind.None)
            _onError.Error(e.Location, "this outside class definition");
        ResolveVarLike(e, "this");
        return ValueTuple.Create();
    }

    public override ValueTuple VisitSuperGet(SuperGet e)
    {
        switch (_classKind) {
            case ClassKind.None:
                _onError.Error(e.Location, "super outside class definition");
                break;
            case ClassKind.Class:
                _onError.Error(e.Location, "super in class with no superclass");
                break;
            case ClassKind.SubClass:
                // TODO: this seems dum
                ResolveVarLike(e, "super");
                break;
        }
        return ValueTuple.Create();
    }

    public override ValueTuple VisitReturn(Return s)
    {
        if (_functionKind == FunctionKind.None)
            _onError.Error(s.Location, "return outside function/method body");
        if (s.Expression != null) {
            if (_functionKind == FunctionKind.Initializer) {
                _onError.Error(s.Expression.Location,
                  "return value from initializer");
            }
            s.Expression.Accept(this);
        }
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

        if (s.Super != null) {
            // TODO: proper cycle detection, but those damn deferred globals...
            if (s.Super.Name == s.Name.Name)
                _onError.Error(s.Super.Location,
                  $"cannot use class {s.Name.Name} as its own superclass");
            ResolveVarLike(s.Super, s.Super.Name);
            PushScope();
            Define("super");
        }

        var currentClassKind = _classKind;
        _classKind = s.Super == null ? ClassKind.Class : ClassKind.SubClass;
        var currentFunctionKind = _functionKind;
        foreach (var method in s.Methods) {
            if (method.Name.Name == "init")
                _functionKind = FunctionKind.Initializer;
            else
                _functionKind = FunctionKind.Method;
            ResolveFunDefOrMethod(method);
        }
        _functionKind = currentFunctionKind;
        _classKind = currentClassKind;

        if (s.Super != null)
            PopScope();

        return ValueTuple.Create();
    }

    public static Dictionary<Expr, int>? Resolve(List<Stmt> program,
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
        /* ok, I'm deviating from the book here: we'll make 'this' the first
         * slot in the function environment */
        if (_functionKind == FunctionKind.Method
                || _functionKind == FunctionKind.Initializer)
            Define("this");
        foreach (var p in s.Parameters) {
            Declare(p);
            Define(p.Name);
        }
        foreach (var stmt in s.Body)
            stmt.Accept(this);
        PopScope();
    }

    private bool AtGlobalScope => _stateStack.Count == 1;

    private Dictionary<string, VariableState> ScopeStates => _stateStack.Peek();

    // so gross: this has to be Expr, because it has to work for This and Var
    private Dictionary<Expr, int> VariableScopesOut => _scopesOut;

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

    private void ResolveVarLike(Expr e, string name)
    {
        int i = 0;
        foreach (var frame in _stateStack) {
            if (frame.ContainsKey(name)) {
                _scopesOut[e] = i;
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
                _scopesOut[e] = i - 1;
                frame[name] = VariableState.Deferred;
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
    private Dictionary<Expr, int> _scopesOut;
    private FunctionKind _functionKind;
    private ClassKind _classKind;
}
