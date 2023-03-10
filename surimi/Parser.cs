namespace Surimi;

using System.Collections.Generic;

public class Parser {
    public Parser(IEnumerable<Token> tokens, ErrorReporter onError)
    {
        _tokens = new PeekableStream(tokens);
        _onError = onError;
    }

    public static List<Stmt>? Parse(IEnumerable<Token> tokens,
      ErrorReporter onError)
    {
        var p = new Parser(tokens, onError);
        var prog = new List<Stmt>();
        bool failed = false;
        while (!p.AtEOF()) {
            try {
                prog.Add(p.Declaration());
            } catch (ParseError) {
                failed = true;
                p.Recover();
            }
        }

        try {
            p.RequireEOF();
        } catch (ParseError) {
            failed = true;
        }

        return failed ? null : prog;
    }

    private Stmt Declaration()
    {
        if (Match(TokenType.Var) != null) {
            var name = Require("expected identifier", TokenType.Ident);
            Expr? initializer = null;
            if (Match(TokenType.Eq) != null) {
                initializer = Expression();
            }
            Require("expected ';'", TokenType.Semicolon);
            return new VarDecl(name.Lexeme, initializer, name.Location);
        }

        return Statement();
    }

    private Stmt Statement()
    {
        Token? tok;
        Expr e;
        if ((tok = Match(TokenType.Print)) != null) {
            e = Expression();
            Require("expected ';'", TokenType.Semicolon);
            return new Print(e, tok.Value.Location);
        }

        e = Expression();
        Require("expected ';'", TokenType.Semicolon);
        return new ExprStmt(e, e.Location);
    }

    private Expr Expression()
    {
        return Equality();
    }

    private delegate Expr ExprParser();

    private Expr BinOpFold(ExprParser parser, params TokenType[] types)
    {
        var lhs = parser();
        Token? op_tok;
        while ((op_tok = Match(types)) != null) {
            var rhs = parser();
            lhs = new BinOpApp(TokenBinOp(op_tok.Value), lhs, rhs,
              op_tok.Value.Location);
        }
        return lhs;
    }

    private Expr Equality() => BinOpFold(Comparison,
      TokenType.NotEq, TokenType.EqEq);

    private Expr Comparison() => BinOpFold(Term,
      TokenType.Gt, TokenType.GtEq, TokenType.Lt, TokenType.LtEq);

    private Expr Term() => BinOpFold(Factor, TokenType.Plus, TokenType.Minus);

    private Expr Factor() => BinOpFold(Unary, TokenType.Star, TokenType.Slash);

    private Expr Unary()
    {
        Token? op_tok = Match(TokenType.Not, TokenType.Minus);
        if (op_tok != null) {
            var rhs = Unary();
            return new UnOpApp(TokenUnOp(op_tok.Value), rhs,
              op_tok.Value.Location);
        }
        return Primary();
    }

    private Expr Primary()
    {
        Token? tok;

        if ((tok = Match(TokenType.Nil)) != null)
            return new Literal(null, tok.Value.Location);

        if ((tok = Match(TokenType.True)) != null)
            return new Literal(true, tok.Value.Location);

        if ((tok = Match(TokenType.False)) != null)
            return new Literal(false, tok.Value.Location);

        if ((tok = Match(TokenType.StrLit, TokenType.NumLit)) != null)
            return new Literal(tok.Value.Literal, tok.Value.Location);

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
            _onError.Error(null, " at end of input", message);
            throw new ParseError();
        }

        Token tok = _tokens.Next!.Value;
        _tokens.Advance();
        foreach (var type in types) {
            if (tok.Type == type)
                return tok;
        }

        _onError.Error(tok.Location, $" at '{tok.Lexeme}'", message);
        throw new ParseError();
    }

    private bool AtEOF() => _tokens.Next == null;

    private void RequireEOF()
    {
        if (!AtEOF()) {
            _onError.Error(_tokens.Next!.Value.Location,
              $" at \"{_tokens.Next!.Value.Lexeme}\"",
              "expected end of input");
            throw new ParseError();
        }
    }

    private bool Check(TokenType type) => _tokens.Next?.Type == type;

    // seek to "sync point" (see ch6)
    private void Recover()
    {
        while (true) {
            if (Match(TokenType.Semicolon) != null)
                return;

            _tokens.Advance();

            if (_tokens.Next == null)
                return;

            switch (_tokens.Next.Value.Type) {
                case TokenType.Class:
                case TokenType.Fun:
                case TokenType.Var:
                case TokenType.For:
                case TokenType.If:
                case TokenType.While:
                case TokenType.Print:
                case TokenType.Return:
                    return;
                default:
                    break;
            }
        }
    }

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
