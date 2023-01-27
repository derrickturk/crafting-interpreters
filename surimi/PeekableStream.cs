namespace Surimi;

class PeekableStream: IDisposable {
    public PeekableStream(IEnumerable<Token> stream)
    {
        _stream = stream.GetEnumerator();
        _exhausted = false;
        _next = null;
        _highest_line = 1;
    }

    public Token? Next
    {
        get 
        {
            if (_next == null && !_exhausted)
                AdvanceImpl();
            return _next;
        }
    }

    public int Line => _highest_line;

    public void Advance()
    {
        if (!_exhausted)
            AdvanceImpl();
    }

    public void Dispose()
    {
        _stream.Dispose();
    }

    private void AdvanceImpl()
    {
        _exhausted = !_stream.MoveNext();
        if (_exhausted) {
            _next = null;
        } else {
            _next = _stream.Current;
            _highest_line = _stream.Current.Line;
        }
    }

    private IEnumerator<Token> _stream;
    private bool _exhausted;
    private Token? _next;
    private int _highest_line = 1;
}
