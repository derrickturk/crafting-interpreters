namespace Surimi;

using System.Linq;

public enum TokenType {
    LParen, RParen, LBrace, RBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,
    Not, NotEq, Eq, EqEq, Gt, GtEq, Lt, LtEq,
    Ident, StrLit, NumLit,
    And, Class, Else, False, Fun, For, If, Nil, Or, Print, Return, Super, This,
    True, Var, While,
    Eof, // I don't trust this guy one bit
}

// I don't like this representation for tokens
public readonly record struct Token(
  TokenType Type, string Lexeme, object? Literal, int Line);

public class Lexer {
    public Lexer(string input, ErrorReporter onError)
    {
        _line = 1;
        _startptr = 0;
        _nextptr = 0;
        _input = input.AsMemory();
        _onError = onError;
    }

    public Token Next()
    {
        while (true) {
            _startptr = _nextptr;

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

                case '!':
                    return HereToken(Match('=')
                      ? TokenType.NotEq : TokenType.Not);
                case '=':
                    return HereToken(Match('=')
                      ? TokenType.EqEq : TokenType.Eq);
                case '<':
                    return HereToken(Match('=')
                      ? TokenType.LtEq : TokenType.Lt);
                case '>':
                    return HereToken(Match('=')
                      ? TokenType.GtEq : TokenType.Gt);

                case '/':
                    if (Match('/')) {
                        while (!AtEnd && Peek != '\n')
                            Advance();
                    } else {
                        return HereToken(TokenType.Slash);
                    }
                    break;

                case ' ':
                case '\r':
                case '\t':
                    break;
                case '\n':
                    ++_line;
                    break;

                case '"':
                    while (!AtEnd) {
                        char next = Consume();
                        switch (next) {
                            case '"':
                                string token = CurrentLexeme;
                                return HereToken(TokenType.StrLit,
                                  token.Substring(1, token.Length - 2));
                            case '\n':
                                ++_line;
                                break;
                            default:
                                break;
                        }
                    }
                    _onError.Error(_line, "unterminated string literal");
                    break;

                case >= '0' and <= '9':
                    while (!AtEnd && Peek >= '0' && Peek <= '9')
                        Consume();
                    if (!AtEnd && Peek == '.') {
                        Consume();
                        if (AtEnd || Peek < '0' || Peek > '9')
                            Retreat();
                        else
                            while (!AtEnd && Peek >= '0' && Peek <= '9')
                                Consume();
                    }
                    return HereToken(TokenType.NumLit,
                      Double.Parse(CurrentLexeme));

                default:
                    _onError.Error(_line, $"unexpected character '{c}'");
                    break;
            }
        }
    }

    public static IEnumerable<Token> Lex(string input, ErrorReporter onError)
    {
        var l = new Lexer(input, onError);
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

    protected Token HereToken(TokenType type, object? literal = null)
    {
        return new Token(type, CurrentLexeme, literal, _line);
    }

    protected bool AtEnd => _nextptr >= _input.Length;

    protected char Peek => _input.Span[_nextptr];

    protected string CurrentLexeme =>
      _input.Slice(_startptr, _nextptr - _startptr).ToString();

    protected void Advance()
    {
        ++_nextptr;
    }

    // use with caution - just for 2-char lookahead on numbers at the moment
    protected void Retreat()
    {
        --_nextptr;
    }

    private int _line;
    private int _startptr;
    private int _nextptr;
    private ReadOnlyMemory<char> _input;
    private ErrorReporter _onError;
}
