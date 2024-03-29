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
        while (!p.AtEOF) {
            try {
                prog.Add(p.Declaration());
            } catch (ParseError) {
                failed = true;
                p.Recover();
            }
        }
        return failed ? null : prog;
    }

    private Stmt Declaration()
    {
        Token? tok;
        if ((tok = Match(TokenType.Var)) != null)
            return VarDeclRest(tok.Value);
        if ((tok = Match(TokenType.Fun)) != null)
            return FunDefOrMethodRest(tok.Value);
        if ((tok = Match(TokenType.Class)) != null)
            return ClassDefRest(tok.Value);
        return Statement();
    }

    private VarDecl VarDeclRest(Token var_)
    {
        var name = Require("expected identifier", TokenType.Ident);
        Expr? initializer = null;
        if (Match(TokenType.Eq) != null) {
            initializer = Expression();
        }
        Require("expected ';'", TokenType.Semicolon);
        return new VarDecl(
          new Var(name.Lexeme, name.Location), initializer, var_.Location);
    }

    private FunDef FunDefOrMethodRest(Token tok)
    {
        string kind;
        Var name;
        switch (tok.Type) {
            case TokenType.Fun:
                kind = "function";
                var ident = Require("expected function name", TokenType.Ident);
                name = new Var(ident.Lexeme, ident.Location);
                break;
            case TokenType.Ident:
                kind = "method";
                name = new Var(tok.Lexeme, tok.Location);
                break;
            default:
                throw new InvalidOperationException(
                  "internal error: bad token type for fun/method rest");
        }

        Require($"expected '(' after {kind} name", TokenType.LParen);

        List<Var> parameters = new List<Var>();
        if (!Check(TokenType.RParen)) {
            do {
                var ident = Require("expected parameter name", TokenType.Ident);
                parameters.Add(new Var(ident.Lexeme, ident.Location));
            } while (Match(TokenType.Comma) != null);
        }

        var rparen = Require("expected ')'", TokenType.RParen);
        if (parameters.Count > 255)
            _onError.Error(rparen.Location, $" at {rparen.Lexeme}",
              "more than 255 arguments");

        var lbrace = Require($"expected '{{' before {kind} body",
          TokenType.LBrace);
        var body = BlockRest(lbrace);

        return new FunDef(name, parameters, body.Statements, tok.Location);
    }

    private ClassDef ClassDefRest(Token tok)
    {
        Token name = Require("expected class name", TokenType.Ident);
        Var? super = null;
        if (Match(TokenType.Lt) != null) {
            Token superName = Require("superclass name", TokenType.Ident);
            super = new Var(superName.Lexeme, superName.Location);
        }
        Require("expected '{'", TokenType.LBrace);
        List<FunDef> methods = new List<FunDef>();
        while (!Check(TokenType.RBrace) && !AtEOF) {
            var methodName = Require("expected method name", TokenType.Ident);
            methods.Add(FunDefOrMethodRest(methodName));
        }
        Require("expected '}'", TokenType.RBrace);
        return new ClassDef(new Var(name.Lexeme, name.Location),
          super, methods, tok.Location);
    }

    private Stmt Statement()
    {
        Token? tok;
        if ((tok = Match(TokenType.If)) != null)
            return IfElseRest(tok.Value);
        if ((tok = Match(TokenType.While)) != null)
            return WhileRest(tok.Value);
        if ((tok = Match(TokenType.For)) != null)
            return ForRest(tok.Value);
        if ((tok = Match(TokenType.Print)) != null)
            return PrintRest(tok.Value);
        if ((tok = Match(TokenType.Return)) != null)
            return ReturnRest(tok.Value);
        if ((tok = Match(TokenType.LBrace)) != null)
            return BlockRest(tok.Value);

        return ExpressionStatement();
    }

    private IfElse IfElseRest(Token if_)
    {
        Require("expected '('", TokenType.LParen);
        var cond = Expression();
        Require("expected ')'", TokenType.RParen);
        Stmt s_if = Statement();
        Stmt? s_else = null;
        if (Match(TokenType.Else) != null)
            s_else = Statement();
        return new IfElse(cond, s_if, s_else, if_.Location);
    }

    private While WhileRest(Token while_)
    {
        Require("expected '('", TokenType.LParen);
        var cond = Expression();
        Require("expected ')'", TokenType.RParen);
        var body = Statement();
        return new While(cond, body, while_.Location);
    }

    private Stmt ForRest(Token for_)
    {
        Require("expected '('", TokenType.LParen);

        Token? init_tok = null;
        Stmt? initializer;
        if (Match(TokenType.Semicolon) != null)
            initializer = null;
        else if ((init_tok = Match(TokenType.Var)) != null)
            initializer = VarDeclRest(init_tok.Value);
        else
            initializer = ExpressionStatement();

        Token? cond_tok = null;
        Expr? condition;
        if ((cond_tok = Match(TokenType.Semicolon)) != null) {
            condition = null;
        } else {
            condition = Expression();
            Require("expected ';'", TokenType.Semicolon);
        }

        Expr? increment;
        if (Match(TokenType.RParen) != null) {
            increment = null;
        } else {
            increment = Expression();
            Require("expected ')'", TokenType.RParen);
        }

        var body = Statement();

        if (increment != null)
            body = new Block(new List<Stmt> {
                body,
                new ExprStmt(increment, increment.Location),
            }, body.Location);

        if (condition == null)
            // point to the ;
            condition = new Literal(true, cond_tok!.Value.Location);

        body = new While(condition, body, for_.Location);

        if (initializer != null)
            return new Block(new List<Stmt> {
                initializer,
                body,
            }, initializer.Location);

        return body;
    }

    private Print PrintRest(Token print)
    {
        var e = Expression();
        Require("expected ';'", TokenType.Semicolon);
        return new Print(e, print.Location);
    }

    private Return ReturnRest(Token return_)
    {
        if (Match(TokenType.Semicolon) != null)
            return new Return(null, return_.Location);
        var e = Expression();
        Require("expected ';'", TokenType.Semicolon);
        return new Return(e, return_.Location);
    }

    private Block BlockRest(Token lbrace)
    {
        var stmts = new List<Stmt>();
        while (!Check(TokenType.RBrace) && !AtEOF)
            stmts.Add(Declaration());
        Require("expected '}'", TokenType.RBrace);
        return new Block(stmts, lbrace.Location);
    }

    private ExprStmt ExpressionStatement()
    {
        var e = Expression();
        Require("expected ';'", TokenType.Semicolon);
        return new ExprStmt(e, e.Location);
    }

    private Expr Expression()
    {
        return Assignment();
    }

    private Expr Assignment()
    {
        var e = LogicOr();

        Token? eq;
        if ((eq = Match(TokenType.Eq)) != null) {
            var val = Assignment();
            switch (e) {
                case Var v:
                    return new Assign(v, val, v.Location);
                case PropertyGet g:
                    return new PropertySet(g.Object, g.Name, val, g.Location);
                default:
                    _onError.Error(eq.Value.Location,
                      $" at '{eq.Value.Lexeme}'",
                      $"invalid assignment target: {e.PrettyPrint()}");
                    throw new ParseError();
            }
        }

        return e;
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

    private Expr LogicOr() => BinOpFold(LogicAnd, TokenType.Or);

    private Expr LogicAnd() => BinOpFold(Equality, TokenType.And);

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
        return Call();
    }

    private Expr Call()
    {
        var e = Primary();
        Token? tok;
        while ((tok = Match(TokenType.LParen, TokenType.Dot)) != null) {
            switch (tok.Value.Type) {
                case TokenType.LParen:
                    e = CallRest(e, tok.Value);
                    break;
                case TokenType.Dot:
                    Token name = Require("property name", TokenType.Ident);
                    e = new PropertyGet(e, new Var(name.Lexeme, name.Location),
                      tok.Value.Location);
                    break;
                default:
                    throw new InvalidOperationException(
                      "internal error: impossible result from Match");
            }
        }
        return e;
    }

    private Expr CallRest(Expr callee, Token lparen)
    {
        List<Expr> args = new List<Expr>();
        if (!Check(TokenType.RParen))
            do
                args.Add(Expression());
            while (Match(TokenType.Comma) != null);

        var rparen = Require("expected ')'", TokenType.RParen);

        /* TODO: revisit this whole idea - do we need it?
         * isn't the VM a stack machine?
         */
        if (args.Count > 255)
            _onError.Error(rparen.Location, $" at {rparen.Lexeme}",
              "more than 255 arguments");

        /* which Location?
         *   book sez rparen (low energy)
         *   callee.Location is sensible choice
         *   I choose lparen (that's where the call "begins" IMO
         */
        return new Call(callee, args, lparen.Location);
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

        if ((tok = Match(TokenType.This)) != null)
            return new This(tok.Value.Location);

        if ((tok = Match(TokenType.Ident)) != null)
            return new Var(tok.Value.Lexeme, tok.Value.Location);

        if ((tok = Match(TokenType.Super)) != null) {
            Require("expected '.'", TokenType.Dot);
            Token methodName = Require("expected superclass method name",
              TokenType.Ident);
            return new SuperGet(
              new Var(methodName.Lexeme, methodName.Location),
              tok.Value.Location);
        }

        if (Match(TokenType.LParen) != null) {
            var e = Expression();
            Require("expected ')'", TokenType.RParen);
            return e;
        }

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

    private bool AtEOF => _tokens.Next == null;

    private void RequireEOF()
    {
        if (!AtEOF) {
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
        TokenType.Or => BinOp.Or,
        TokenType.And => BinOp.And,
        _ => throw new ArgumentException($"Invalid binary operator {t}"),
    };

    private PeekableStream _tokens;
    private ErrorReporter _onError;
}
