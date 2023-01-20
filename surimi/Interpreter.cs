namespace Surimi;

using System;

public class Interpreter {
    public void run(string code)
    {
        Console.WriteLine($"beep boop: {code}");
    }

    public bool HadError { get; protected set; }

    protected void Error(int line, string msg)
    {
        Report(line, "", msg);
        HadError = true;
    }

    protected void Report(int line, string whurr, string msg)
    {
        Console.Error.WriteLine($"[line {line}] Error{whurr}: {msg}");
    }
}
