package org.zlox.zlox.Main

sealed trait Expr

object Expr {
  case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
  case class Call(callee: Expr, paren: Token, arguments: List[Expr]) extends Expr
  case class Grouping(expression: Expr) extends Expr
  case class Literal(value: Any) extends Expr
  case class Logical(left: Expr, operator: Token, right: Expr) extends Expr
  case class Unary(operator: Token, right: Expr) extends Expr
  case class Variable(name: Token) extends Expr
  case class Assign(name: Token, value: Expr) extends Expr
  case class Get(obj: Expr, name: Token) extends Expr
  case class Set(obj: Expr, name: Token, value: Expr) extends Expr
  case class Super(keyword: Token, method: Token) extends Expr
  case class This(keyword: Token) extends Expr
}