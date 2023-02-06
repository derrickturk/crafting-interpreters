namespace Surimi;

class PeekableStream: IDisposable {
    public PeekableStream(IEnumerable<Token> stream)
    {
        _stream = stream.GetEnumerator();
        _exhausted = false;
        _next = null;
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
        }
    }

    private IEnumerator<Token> _stream;
    private bool _exhausted;
    private Token? _next;
}
