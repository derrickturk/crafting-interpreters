use loxide::{
    syntax::*,
    Value,
};

fn main() {
    let ex = Box::new(Expr::BinOpApp(
        BinOp::Add,
        Box::new(Expr::BinOpApp(
            BinOp::GtEq,
            Box::new(Expr::UnOpApp(
                UnOp::Complement,
                Box::new(Expr::Literal(Value::Bool(true))),
            )),
            Box::new(Expr::Literal(Value::Nil)),
        )),
        Box::new(Expr::BinOpApp(
            BinOp::Eq,
            Box::new(Expr::Literal(Value::Number(36.5))),
            Box::new(Expr::Literal(Value::String("potato".to_string()))),
        )),
    ));
    println!("{}", ex);
}
