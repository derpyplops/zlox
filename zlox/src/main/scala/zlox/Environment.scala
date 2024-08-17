package org.zlox.zlox.Environment

import zio._
import scala.collection.mutable._
import org.zlox.zlox.Main.RuntimeError
import org.zlox.zlox.Token.Token

class Environment(private final val vals: Map[String, Option[Any]] = Map()) {
    def define(name: String, value: Option[Any]) = 
        vals(name) = value // todo handle the option here, breaks abstraction
    
    def get(name: Token): Any = 
        vals.getOrElse(name.lexeme, throw RuntimeError(name, "Undefined variable '" + name.lexeme + "'.")).getOrElse("nil")

    def assign(name: Token, value: Any): Unit = {
        if (!vals.contains(name.lexeme)) throw RuntimeError(name, "Undefined variable '" + name.lexeme + "'.")
        vals(name.lexeme) = Some(value)
    }
}

