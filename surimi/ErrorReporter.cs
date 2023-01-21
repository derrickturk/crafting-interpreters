public class ErrorReporter {
    public bool HadError { get; set; }

    public void Error(int line, string msg)
    {
        Report(line, "", msg);
        HadError = true;
    }

    protected void Report(int line, string whurr, string msg)
    {
        Console.Error.WriteLine($"[line {line}] Error{whurr}: {msg}");
    }
}
