package org.zlox.zlox.Main

object Resolver {
  import collection.mutable.Map

  var scopes = Array[Map[String, Boolean]]()

  enum FnType {
    case FUNCTION, NONE, METHOD, INITIALIZER
  }

  enum ClassType {
    case NONE, CLASS
  }

  var currentFunctionType: FnType = FnType.NONE
  var currentClassType: ClassType = ClassType.NONE

  def resolveFunction(func: Function, functionType: FnType): Unit = {
    val enclosingFunction = functionType
    currentFunctionType = functionType
    beginScope()
    func.params.foreach { param => 
      declare(param)
      define(param)
    }
    func.body.foreach(resolve)
    endScope()
    currentFunctionType = enclosingFunction
  }

  def resolve(thing: Stmt | Expr): Unit = {
    thing match {
      case Class(name: Token, methods: List[Function]) => {
        val enclosingClassType = currentClassType
        currentClassType = ClassType.CLASS
        declare(name)
        define(name)
        beginScope()
        scopes.last("this") = true
        methods.map { method =>
            val declaration = if (name.lexeme == "init") FnType.INITIALIZER else FnType.METHOD
            resolveFunction(method, declaration)
        }
        endScope()
        currentClassType = enclosingClassType
      }
      case Block(statements) => {
        beginScope()
        statements.foreach(resolve)
        endScope()
      }
      case Var(name, initializer) => {
        declare(name)
        initializer.foreach(resolve)
        define(name)
      }
      case expr @ Expr.Variable(name) => {
        if (!scopes.isEmpty && scopes.last.get(name.lexeme).contains(false)) {
          Lox.error(name.line, "Cannot read local variable in its own initializer.")
        }
        resolveLocal(expr, name)
      }
      case expr @ Expr.Assign(name, value) => {
        resolve(value)
        resolveLocal(expr, name)
      }
      case stmt @ Function(name, params, body) => {
        declare(name)
        define(name)
        resolveFunction(stmt, FnType.FUNCTION)
      }
      case Expression(expr) => {
        resolve(expr)
      }
      case If(condition, thenDo, elseDo) => {
        resolve(condition)
        resolve(thenDo)
        elseDo.foreach(resolve)
      }
      case Print(expr) => {
        resolve(expr)
      }
      case Return(keyword, value) => {
        checkInvalidReturn(keyword)
        value.foreach { v =>
          if (currentFunctionType == FnType.INITIALIZER) {
            Lox.error(keyword.line, "Can't return a value from an initializer.")
          }
          resolve(v)
        }
      }
      case While(condition, body) => {
        resolve(condition)
        resolve(body)
      }
      case Expr.Binary(left, operator, right) => {
        resolve(left)
        resolve(right)
      }
      case Expr.Call(callee, paren, arguments) => {
        resolve(callee)
        arguments.foreach(resolve)
      }
      case Expr.Grouping(expression) => {
        resolve(expression)
      }
      case Expr.Literal(value) => ()
      case Expr.Logical(left, operator, right) => {
        resolve(left)
        resolve(right)
      }
      case Expr.Unary(operator, right) => {
        resolve(right)
      }
      case Expr.Get(expr, name) => {
        resolve(expr)
      }
      case Expr.Set(obj, name, value) => {
        resolve(obj)
        resolve(value)
      }
      case expr @ Expr.This(keyword) => {
        Lox.error(keyword.line, "Can't use this outside a class")
        return resolveLocal(expr, keyword)
      }
    }
  }

  private def resolveLocal(expr: Expr, name: Token): Unit = {
    scopes.zipWithIndex.reverse
      .find { case (scope, _) => scope.contains(name.lexeme) }
      .foreach { case (_, index) =>
        Interpreter.resolve(expr, scopes.size - 1 - index)
      }
  }

  private def checkExistingVar(name: Token) = {
    val scope = scopes.lastOption
    if (scope.map(_.contains(name.lexeme)).getOrElse(false)) {
      Lox.error(name.line, "Variable with this name already declared in this scope.")
    }

    // if (scope.contains(name.lexeme)) {
    //   Lox.error(name.line, "Variable with this name already declared in this scope.")
    // }
  }

  private def checkInvalidReturn(name: Token) = {
    if (scopes.isEmpty) {
      Lox.error(name.line, "Cannot return from top-level code.")
    }
  }

  private def declare(name: Token) = 
    if scopes.nonEmpty then scopes.last += (name.lexeme -> false)
  

  private def define(name: Token) = 
    if scopes.nonEmpty then scopes.last += (name.lexeme -> true)
  

  private def beginScope() = {
    scopes = scopes.appended(Map())
  }

  private def endScope() = {
    scopes = scopes.dropRight(1)
  }


}