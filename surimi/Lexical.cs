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

public static class Lexer {
    public static IEnumerable<Token> Lex(string input)
    {
        int line = 1;
        yield return new Token(TokenType.Eof, null, null, line);
    }
}
