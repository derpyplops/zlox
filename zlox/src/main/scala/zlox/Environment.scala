package org.zlox.zlox.Environment

import zio._
import scala.collection.mutable._
import org.zlox.zlox.Main.RuntimeError
import org.zlox.zlox.Token.Token



class Environment(private final val vals: Map[String, Any] = Map(), final val enclosing: Option[Environment] = None) {
    def define(name: String, value: Any) = 
        vals(name) = value
    
    def get(name: Token): Any = // returns a value if it's present, but can throw undefined var, should explode if not found
        vals
            .get(name.lexeme)
            .orElse(enclosing.map(_.get(name)))
            .getOrElse(throw RuntimeError(name, "Undefined variable '" + name.lexeme + "'."))

    def assign(name: Token, value: Any): Unit = {
        if (!vals.contains(name.lexeme)) {
            enclosing match
                case Some(env) => env.assign(name, value)
                case None => throw RuntimeError(name, "Undefined variable '" + name.lexeme + "'.")
        }
        else vals(name.lexeme) = value
    }
}

object Environment {
    def withEnclosing(env: Environment): Environment = {
        Environment(enclosing=Some(env))
    }
}

