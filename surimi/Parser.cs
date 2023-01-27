namespace Surimi;

public class Parser {
    public Parser(IEnumerable<Token> tokens, ErrorReporter onError)
    {
        _tokens = new PeekableStream(tokens);
        _onError = onError;
    }

    public static Expr? Parse(IEnumerable<Token> tokens, ErrorReporter onError)
    {
        try {
            return new Parser(tokens, onError).Expression();
        } catch (ParseError) {
            return null;
        }
    }

    private Expr Expression()
    {
        return Equality();
    }

    // TODO: de-duplicate all these!
    private Expr Equality()
    {
        var lhs = Comparison();
        Token? op_tok;
        while ((op_tok = Match(TokenType.NotEq, TokenType.EqEq)) != null) {
            var rhs = Comparison();
            lhs = new BinOpApp(TokenBinOp(op_tok.Value), lhs, rhs);
        }
        return lhs;
    }

    private Expr Comparison()
    {
        var lhs = Term();
        Token? op_tok;
        while ((op_tok = Match(
          TokenType.Gt, TokenType.GtEq, TokenType.Lt, TokenType.LtEq)) != null) {
            var rhs = Term();
            lhs = new BinOpApp(TokenBinOp(op_tok.Value), lhs, rhs);
        }
        return lhs;
    }

    private Expr Term()
    {
        var lhs = Factor();
        Token? op_tok;
        while ((op_tok = Match(TokenType.Plus, TokenType.Minus)) != null) {
            var rhs = Factor();
            lhs = new BinOpApp(TokenBinOp(op_tok.Value), lhs, rhs);
        }
        return lhs;
    }

    private Expr Factor()
    {
        var lhs = Unary();
        Token? op_tok;
        while ((op_tok = Match(TokenType.Star, TokenType.Slash)) != null) {
            var rhs = Unary();
            lhs = new BinOpApp(TokenBinOp(op_tok.Value), lhs, rhs);
        }
        return lhs;
    }

    private Expr Unary()
    {
        Token? op_tok = Match(TokenType.Not, TokenType.Minus);
        if (op_tok != null) {
            var rhs = Unary();
            return new UnOpApp(TokenUnOp(op_tok.Value), rhs);
        }
        return Primary();
    }

    private Expr Primary()
    {
        if (Match(TokenType.Nil) != null)
            return new Literal(null);

        if (Match(TokenType.True) != null)
            return new Literal(true);

        if (Match(TokenType.False) != null)
            return new Literal(false);

        Token? tok;
        if ((tok = Match(TokenType.StrLit, TokenType.NumLit)) != null)
            return new Literal(tok.Value.Literal);

        if (Match(TokenType.LParen) != null) {
            var e = Expression();
            Require("expected ')'", TokenType.RParen);
            return e;
        }

        // TODO: recover instead
        Require("expected an expression");
        // unreachable
        throw new InvalidOperationException(
          "internal error - should be unreachable");
    }

    private Token? Match(params TokenType[] types)
    {
        foreach (var type in types) {
            if (Check(type)) {
                var tok = _tokens.Next;
                _tokens.Advance();
                return tok;
            }
        }
        return null;
    }

    private Token Require(string message, params TokenType[] types)
    {
        if (_tokens.Next == null) {
            _onError.Error(_tokens.Line, " at end of input", message);
            throw new ParseError();
        }

        Token tok = _tokens.Next.Value;
        _tokens.Advance();
        foreach (var type in types) {
            if (tok.Type == type)
                return tok;
        }

        _onError.Error(tok.Line, $" at '{tok.Lexeme}'", message);
        throw new ParseError();
    }

    private bool Check(TokenType type)
    {
        return _tokens.Next?.Type == type;
    }

    // TODO: private void Recover() // seek to "sync point"

    private class ParseError: Exception { }

    private static UnOp TokenUnOp(Token t) => t.Type switch {
        TokenType.Minus => UnOp.Negate,
        TokenType.Not => UnOp.Not,
        _ => throw new ArgumentException($"Invalid unary operator {t}"),
    };

    private static BinOp TokenBinOp(Token t) => t.Type switch {
        TokenType.Plus => BinOp.Plus,
        TokenType.Minus => BinOp.Minus,
        TokenType.Star => BinOp.Times,
        TokenType.Slash => BinOp.DividedBy,
        TokenType.NotEq => BinOp.NotEq,
        TokenType.EqEq => BinOp.EqEq,
        TokenType.Gt => BinOp.Gt,
        TokenType.GtEq => BinOp.GtEq,
        TokenType.Lt => BinOp.Lt,
        TokenType.LtEq => BinOp.LtEq,
        _ => throw new ArgumentException($"Invalid binary operator {t}"),
    };

    private PeekableStream _tokens;
    private ErrorReporter _onError;
}
