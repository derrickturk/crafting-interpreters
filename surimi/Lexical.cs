namespace Surimi;

using System.Linq;

public enum TokenType {
    LParen, RParen, LBrace, RBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,
    Excl, ExclEqual, Eq, EqEq, Gt, GtEq, Lt, LtEq,
    Ident, StrLit, NumLit,
    And, Class, Else, False, Fun, For, If, Nil, Or, Print, Return, Super, This,
    True, Var, While,
    Eof, // I don't trust this guy one bit
}

// I don't like this representation for tokens
public readonly record struct Token(
  TokenType Type, string? Lexeme, object? Literal, int Line);

public class Lexer {
    public Lexer(string input)
    {
        _line = 1;
        _remaining = input.AsMemory();
    }

    public Token Next()
    {
        if (AtEnd)
            return HereToken(TokenType.Eof);

        char c = Consume();
        switch (c) {
            case '(':
                return HereToken(TokenType.LParen);
            case ')':
                return HereToken(TokenType.RParen);
            case '{':
                return HereToken(TokenType.LBrace);
            case '}':
                return HereToken(TokenType.RBrace);
            case ',':
                return HereToken(TokenType.Comma);
            case '.':
                return HereToken(TokenType.Dot);
            case '-':
                return HereToken(TokenType.Minus);
            case '+':
                return HereToken(TokenType.Plus);
            case ';':
                return HereToken(TokenType.Semicolon);
            case '*':
                return HereToken(TokenType.Star);
            default:
                // TODO: LexError or some such class
                throw new InvalidOperationException(
                  $"unexpected character: {c}");
        }
    }

    public static IEnumerable<Token> Lex(string input)
    {
        var l = new Lexer(input);
        while (true) {
            Token next = l.Next();
            yield return next;
            if (next.Type == TokenType.Eof)
                break;
        }
    }

    protected char Consume()
    {
        char ret = Peek;
        Advance();
        return ret;
    }

    protected bool Match(char what)
    {
        if (!AtEnd && Peek == what) {
            Advance();
            return true;
        }
        return false;
    }

    private bool AtEnd => _remaining.IsEmpty;

    private char Peek => _remaining.Span[0];

    private void Advance()
    {
        _remaining = _remaining.Slice(1);
    }

    protected Token HereToken(TokenType type,
      string? lexeme = null, object? literal = null)
    {
        return new Token(type, lexeme, literal, _line);
    }

    private int _line;
    private ReadOnlyMemory<char> _remaining;
}
