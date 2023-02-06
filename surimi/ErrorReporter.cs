namespace Surimi;

public class ErrorReporter {
    public bool HadError { get; set; }

    public void Error(SrcLoc? loc, string msg)
    {
        Error(loc, "", msg);
    }

    public void Error(SrcLoc? loc, string whurr, string msg)
    {
        Report(loc, whurr, msg);
        HadError = true;
    }

    protected void Report(SrcLoc? loc, string whurr, string msg)
    {
        if (loc != null) {
            var l = loc.Value;
            Console.Error.WriteLine(
              $"[{l.File}:{l.Line}:{l.Char}] Error{whurr}: {msg}");
        } else {
            Console.Error.WriteLine($"Error{whurr}: {msg}");
        }
    }
}
