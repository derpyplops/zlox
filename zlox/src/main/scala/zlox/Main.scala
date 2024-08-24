package org.zlox.zlox.Main

import zio._
import zio.UIO
import scala.io.Source
import java.io.IOException
import scala.util.boundary, boundary.break
import TokenType.*
import org.zlox.zlox.Main.Parser.ParseError
import pprint.PPrinter

case object NotAssigned

object Lox extends ZIOAppDefault {

  var hadError: Boolean = false
  var hadRuntimeError: Boolean = false

  def run = program.provideSomeLayer[ZIOAppArgs](ZLayer.succeed(Console.ConsoleLive))

  val program: ZIO[Console & ZIOAppArgs, IOException, ExitCode] = for {
    args <- ZIOAppArgs.getArgs
    exitCode <- args.toList match {
      case Nil => runPrompt.as(ExitCode.success)
      case file :: Nil => runFile(file)
        .as {
          if (hadError) ExitCode(65)
          else if (hadRuntimeError) ExitCode(70) 
          else ExitCode.success
        }
      case _ => Console.printLine("Usage: zlox [script]").as(ExitCode.failure)
    }
  } yield exitCode

  def runFile(filepath: String): ZIO[Console, IOException, Unit] = for {
    source <- ZIO.attempt(Source.fromFile(filepath)).orDie
    lines <- ZIO.succeed(source.getLines.toList)
    _ <- run(lines.mkString("\n"))
  } yield ()

  val runPrompt: ZIO[Console, IOException, Unit] = {
    def prompt: ZIO[Console, IOException, Boolean] = for {
      _ <- Console.print("> ")
      line <- Console.readLine
      shouldExit = line.trim.toLowerCase == "exit"
      _ <- if (!shouldExit) run(line) else ZIO.unit
      hadError = false
    } yield shouldExit

    for {
      _ <- Console.printLine("Zlox 0.1.0")
      _ <- prompt.repeatUntil(identity)
    } yield ()
  }

  def run(source: String): ZIO[Console, IOException, Unit] = for {
    scanner <- ZIO.succeed(new Scanner(source))
    tokens <- scanner.scanTokens
    stmts <- {
      Parser(tokens).parseExpr.map(expr => List(Print(expr))) orElse Parser(tokens).parseProgram
    } catchAll {
      case ParseError(message) => {
        println(message)
        ZIO.succeed(List.empty)
      }
    }
    _ <- (ZIO.attempt(stmts.map(Resolver.resolve)) 
    *> ZIO.attempt(Interpreter.interpret(stmts))).catchAll {
      case e => Console.printError(f"Resolver error: ${e}")
    }
  } yield ()

  def error(line: Int, message: String) = {
    report(line, "", message)
  }

  def report(line: Int, where: String, message: String): IO[IOException, Unit] = {
    hadError = true
    Console.printLine(s"[line $line] Error$where: $message")
  }

  def handleParseError(error: ParseError) = {
    hadRuntimeError = true
    Console.printLine(error.getMessage())
  }

  def runtimeError(error: RuntimeError) = {
    hadRuntimeError = true
    Console.printLine(error.getMessage() +"\n[line " + error.token.line + "]")
  }
}

class Scanner(val source: String) {
  var tokens: Array[Token] = Array()
  var start: Int = 0
  var current: Int = 0
  var line: Int = 1

  def scanTokens: UIO[Array[Token]] = {
    while (!isAtEnd()) {
      start = current
      scanToken()
    }

    tokens :+= Token(EOF, "", None, line)
    return ZIO.succeed(tokens)
  }

  def isAtEnd(): Boolean = {
    current >= source.length
  }

  def scanToken(): Unit = {
    val c = advance()
    c match {
      case '(' => addToken(LEFT_PAREN)
      case ')' => addToken(RIGHT_PAREN)
      case '{' => addToken(LEFT_BRACE)
      case '}' => addToken(RIGHT_BRACE)
      case ',' => addToken(COMMA)
      case '.' => addToken(DOT)
      case '-' => addToken(MINUS)
      case '+' => addToken(PLUS)
      case ';' => addToken(SEMICOLON)
      case '*' => addToken(STAR)
      case '!' => addToken(if (matchChar('=')) BANG_EQUAL else BANG)
      case '=' => addToken(if (matchChar('=')) EQUAL_EQUAL else EQUAL)
      case '<' => addToken(if (matchChar('=')) LESS_EQUAL else LESS)
      case '>' => addToken(if (matchChar('=')) GREATER_EQUAL else GREATER)
      case '/' => if (matchChar('/')) {
        while (peek() != '\n' && !isAtEnd()) advance()
      } else {
        addToken(SLASH)
      }
      case ' ' => ()
      case '\r' => ()
      case '\t' => ()
      case '\n' => line += 1
      case '"' => string()
      case _ => if (isDigit(c)) {
        number()
      } else if (isAlpha(c)) {
        identifier()
      } else {
        Lox.error(line, "Unexpected character.")
      }
    }
  }

  def identifier(): Unit = {
    while (isAlphaNumeric(peek())) advance()

    val text = source.substring(start, current)
    val tokenType = keywords.getOrElse(text, IDENTIFIER)
    addToken(tokenType)
  }

  def advance(): Char = {
    current += 1
    source.charAt(current - 1)
  }

  def addToken(tokenType: TokenType): Unit = {
    addToken(tokenType, None)
  }

  def addToken(tokenType: TokenType, literal: Any): Unit = {
    val text = source.substring(start, current)
    tokens :+= Token(tokenType, text, literal, line)
  }

  def matchChar(expected: Char): Boolean = {
    if (isAtEnd()) return false
    if (source.charAt(current) != expected) return false

    current += 1
    true
  }

  def peek(): Char = {
    if (isAtEnd()) return '\u0000'
    source.charAt(current)
  }

  def peekNext(): Char = {
    if (current + 1 >= source.length) return '\u0000'
    source.charAt(current + 1)
  }

  def string(): Unit = {
    while (peek() != '"' && !isAtEnd()) {
      if (peek() == '\n') line += 1
      advance()
    }

    if (isAtEnd()) {
      Lox.error(line, "Unterminated string.")
      return
    }

    advance()

    val value = source.substring(start + 1, current - 1)
    addToken(STRING, value)
  }

  def number(): Unit = {
    while (isDigit(peek())) advance()

    if (peek() == '.' && isDigit(peekNext())) {
      advance()
      while (isDigit(peek())) advance()
    }

    addToken(NUMBER, source.substring(start, current).toDouble)
  }

  def isDigit(c: Char): Boolean = {
    c >= '0' && c <= '9'
  }

  def isAlpha(c: Char): Boolean = {
    (c >= 'a' && c <= 'z') ||
    (c >= 'A' && c <= 'Z') ||
    c == '_'
  }

  def isAlphaNumeric(c: Char): Boolean = isAlpha(c) || isDigit(c)

  val keywords: Map[String, TokenType] = Map(
    "and" -> AND,
    "class" -> CLASS,
    "else" -> ELSE,
    "false" -> FALSE,
    "for" -> FOR,
    "fun" -> FUN,
    "if" -> IF,
    "nil" -> NIL,
    "or" -> OR,
    "print" -> PRINT,
    "return" -> RETURN,
    "super" -> SUPER,
    "this" -> THIS,
    "true" -> TRUE,
    "var" -> VAR,
    "while" -> WHILE
  )
}

sealed trait Stmt
case class Expression(expr: Expr) extends Stmt
case class Function(name: Token, params: List[Token], body: List[Stmt]) extends Stmt
case class If(condition: Expr, thenDo: Stmt, elseDo: Option[Stmt]) extends Stmt
case class Print(expr: Expr) extends Stmt
case class Var(name: Token, initializer: Option[Expr]) extends Stmt
case class Block(statements: List[Stmt]) extends Stmt {
  def this(stmts: Stmt*) = this(stmts.toList)
}
case class While(condition: Expr, body: Stmt) extends Stmt
case class Return(keyword: Token, value: Option[Expr]) extends Stmt
case class Class(name: Token, superclass: Option[Expr.Variable], methods: List[Function]) extends Stmt

// def printAst(expr: Expr): String = expr match {
//   case Binary(left, op, right) => s"(${op.lexeme} ${printAst(left)} ${printAst(right)})"
//   case Grouping(expr) => s"(group ${printAst(expr)})"
//   case Literal(value) => if (value == None) "nil" else value.toString
//   case Unary(op, right) => s"(${op.lexeme} ${printAst(right)})"
//   case Variable(name) => s"(name:=${name})"
//   case Assign(name, value) => s"(assign (${name} -> ${value}))"
// }

class Parser(tokens: Array[Token]) {
  var current: Int = 0

  def parseProgram: IO[ParseError, List[Stmt]] = {
    var stmts: List[Either[ParseError, Stmt]] = List()
    while (!isAtEnd()) {
      val dec = declaration()
      stmts = stmts.appended(dec)
    }
    val (errors, successfulStmts) = stmts.partitionMap(identity)
    
    if (errors.nonEmpty) {
      // Combine error messages
      val combinedErrorMsg = errors.map(_.message).mkString("\n")
      ZIO.fail(ParseError(combinedErrorMsg))
    } else {
      ZIO.succeed(successfulStmts)
    }
  }

  def parseExpr: IO[ParseError, Expr] = {
    ZIO.attempt(expression()).mapError(_ => ParseError("failz"))
  }

  def declaration(): Either[ParseError, Stmt] = {
    val ttype = matchToken(VAR, FUN, CLASS).map(_.tokenType)
    try
      ttype match {
        case Some(CLASS) => Right(classDeclaration())
        case Some(FUN) => Right(function("function"))
        case Some(VAR) => Right(varDeclaration())
        case None => Right(statement())
        case Some(_) => throw ParseError("Unreachable")
      }
    catch
      case e: ParseError => {
        synchronize()
        Left(e)
      }
  }

  def classDeclaration(): Stmt = {
    val name = consume(IDENTIFIER, "Expect class name.")

//    val superclass = matchToken(LESS).map(Expr.Variable.apply)

    val superclass = matchToken(LESS).map { _ =>
      Expr.Variable(consume(IDENTIFIER))
    }

    consume(LEFT_BRACE, "Expect it")

    val methods: List[Function] = {
      @annotation.tailrec
      def loop(acc: List[Function]): List[Function] = {
        if (isAtEnd() || check(RIGHT_BRACE)) acc.reverse
        else loop(function("method") :: acc)
      }
      
      loop(List())
    }

    consume(RIGHT_BRACE)

    Class(name, superclass, methods)
  }



  def varDeclaration(): Stmt = {
    val name = consume(IDENTIFIER, "Expect identifier")
    val initializer = matchToken(EQUAL).map(_ => expression())
    consume(SEMICOLON, "Expect ;")

    Var(name, initializer)
  }

  def statement(): Stmt = {
    if (matchToken(FOR).isDefined) forStmt()
    else if (matchToken(IF).isDefined) ifStmt()
    else if (peek().tokenType == PRINT) printStmt()
    else if (matchToken(RETURN).isDefined) returnStmt()
    else if (matchToken(WHILE).isDefined) whileStmt()
    else if (matchToken(LEFT_BRACE).isDefined) Block(block())
    else exprStmt()
  }

  def exprStmt(): Stmt = {
    val exprStmt = Expression(expression())
    consume(SEMICOLON, "Expected semicolon")
    exprStmt
  }

  def function(kind: String): Function = {
    val name = consume(IDENTIFIER, f"Expect ${kind} name.")
    consume(LEFT_PAREN, f"Expect '(' after ${kind} name.")
    val parameters = if (!check(RIGHT_PAREN)) params() else List()
    consume(RIGHT_PAREN, "Expect ')' after parameters.")
    consume(LEFT_BRACE, "Expect '{' before " + kind + " body.")
    val body = block()
    return Function(name, parameters, body)
  }

  def params(): List[Token] = {
    var params: List[Token] = List()
    boundary:
      while (!check(RIGHT_PAREN)) {
        if (params.length >= 255) {
          error(peek(), "Cannot have more than 255 parameters.")
        }
        params = params :+ consume(IDENTIFIER, "Expect parameter name.")
        if (!check(COMMA)) break()
        advance()
      }
    params
  }

  def forStmt(): Stmt = {
    consume(LEFT_PAREN, "Expect '(' after 'for'.")
    val initializer = matchToken(SEMICOLON, VAR).map(_.tokenType) match {
      case Some(SEMICOLON) => None
      case Some(VAR) => Some(varDeclaration())
      case _ => Some(exprStmt())
    }
    val condition = Option.when(!check(SEMICOLON))(expression())
    consume(SEMICOLON, "Expect ';' after loop condition")
    val condStmt = condition.getOrElse(Expr.Literal(true))

    val increment = Option.when(!check(RIGHT_PAREN))(expression())
    consume(RIGHT_PAREN, "Expect ')' after for clauses.")

    val body = statement()

    // val afterInc = Block(List(body) ++ increment.map(Expression.apply).toSeq)
    // val afterCond = While(condition.getOrElse(Literal(true)), afterInc)
    // val afterInit = Block(initializer.toList ++ List(afterCond))
    Block(
      initializer.toList ++ 
      List(
          While(
              condStmt,
              Block(
                  List(body) ++ increment.map(Expression.apply).toList
              )
          )
      )
    )
  }

  def ifStmt(): Stmt = {
    consume(LEFT_PAREN, "Expect ( after 'if'.")
    val condition = expression()
    consume(RIGHT_PAREN, "Expect ')' after 'if' condition")

    val thenDo = statement()
    val elseDo = matchToken(ELSE).map(_ => statement())
    If(condition, thenDo, elseDo)
  }

  def printStmt(): Stmt = {
    consume(PRINT, "Expected print")
    val expr = expression()
    consume(SEMICOLON, "Expected semicolon")
    Print(expr)
  }

  def returnStmt(): Stmt = {
    val keyword = previous()
    val value = Option.when(!check(SEMICOLON))(expression())
    consume(SEMICOLON, "Expect ';' after return value.")
    Return(keyword, value)
  }

  def whileStmt(): Stmt = {
    consume(LEFT_PAREN, "Expect '(' after 'while'.")
    val condition = expression()
    consume(RIGHT_PAREN, "Expect ') after condition.")
    val body = statement()
    return While(condition, body)
  }

  def block(): List[Stmt] = {
    var statements: Array[Stmt] = Array.empty
    while (!check(RIGHT_BRACE) && !isAtEnd()) {
      val decEither = declaration() // WARNING: THIS SILENTLY IGNORES PARSE ERRORS
      statements = statements :++ decEither.toSeq
    }
    consume(RIGHT_BRACE, "Expect }")
    statements.toList
  }

  def expression(): Expr = assignment()

  def assignment(): Expr = {
    val left = or() // every valid assignment target happens to also be valid syntax as a normal expression
    // eg. newPoint(x + 2, 0).y = 3;

    matchToken(EQUAL) match {
      case Some(equals) => {
        val right = assignment()
        left match {
          case Expr.Variable(name) => Expr.Assign(name, right)
          case Expr.Get(obj, name) => Expr.Set(obj, name, right)
          case _ => {
            error(equals, "Invalid assignment target")
            left
          }
        }
      }
      case _ => left
    }
  }

  def or(): Expr = {
    var expr = and()

    while (matchToken(OR).isDefined) {
      val operator = previous()
      val right = and()
      expr = Expr.Logical(expr, operator, right)
    }

    return expr
  }

  def and(): Expr = {
    var expr = equality()

    while (matchToken(AND).isDefined) {
      val operator = previous()
      val right = equality()
      expr = Expr.Logical(expr, operator, right)
    }

    return expr
  }


  def equality(): Expr = leftAssoc(comparison, BANG_EQUAL, EQUAL_EQUAL)

  def comparison(): Expr = leftAssoc(term, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)

  def leftAssoc(lower: () => Expr, tokenTypes: TokenType*): Expr = {
    var expr = lower()

    matchToken(tokenTypes*) match {
      case Some(operator) => {
        val right = leftAssoc(lower, tokenTypes*)
        Expr.Binary(expr, operator, right)
      }
      case None => expr
    }
  }

  def term(): Expr = leftAssoc(factor, MINUS, PLUS)

  def factor(): Expr = leftAssoc(unary, SLASH, STAR)

  def unary(): Expr = {
    matchToken(BANG, MINUS) match {
      case Some(operator) => Expr.Unary(operator, unary())
      case None =>  call()
    }
  }

  def call(): Expr = {
    var expr = primary()

    boundary:
      while (true)
        if (matchToken(LEFT_PAREN).isDefined) then expr = finishCall(expr)
        else if (matchToken(DOT).isDefined) then
          val name = consume(IDENTIFIER, "Expect property name after '.'.")
          expr = Expr.Get(expr, name)
        else break()

    return expr
  }

  def finishCall(callee: Expr) = {
    val arguments = if (!check(RIGHT_PAREN)) args() else List()

    val paren = consume(RIGHT_PAREN, "Expect ')' after arguments.")
    Expr.Call(callee, paren, arguments)
  }

  def args(): List[Expr] = {
    var arguments: List[Expr] = List()
    while
        if arguments.length >= 255 then error(peek(), "Cannot have more than 255 arguments.")
        arguments = arguments :+ expression()
        matchToken(COMMA).isDefined
      do ()
    return arguments
  }

  def primary(): Expr = {
    val token = matchToken(FALSE, TRUE, NIL, NUMBER, STRING, LEFT_PAREN, IDENTIFIER, THIS, SUPER)
      .getOrElse(throw error(peek(), "Expect expression."))
    val ttype = token.tokenType
    ttype match {
      case FALSE => Expr.Literal(false)
      case TRUE => Expr.Literal(true)
      case NIL => Expr.Literal(None)
      case NUMBER => Expr.Literal(token.literal)
      case STRING => Expr.Literal(token.literal)
      case LEFT_PAREN => {
        val expr = expression()
        consume(RIGHT_PAREN, "Expect ')' after expression.")
        Expr.Grouping(expr)
      }
      case IDENTIFIER => Expr.Variable(token)
      case THIS => Expr.This(token)
      case SUPER => {
        consume(DOT)
        val method = consume(IDENTIFIER)
        Expr.Super(token, method)
      }
      case o => {
        pprint.pprintln(o)
        throw error(peek(), "Expect expression.")
      }
    }
  }

  def consume(tokenType: TokenType): Token = consume(tokenType, s"Expected ${tokenType}")

  def consume(tokenType: TokenType, message: String): Token = {
    if (check(tokenType)) return advance()
    throw error(peek(), message)
  }

  def error(token: Token, message: String): ParseError = {
    Lox.error(token.line, message)
    ParseError(f"${message} at ${token}")
  }

  def matchToken(types: TokenType*): Option[Token] = {
    types.find(check).map(_ => advance())
  }

  def check(tokenType: TokenType): Boolean = 
    !isAtEnd() && peek().tokenType == tokenType

  def peek(): Token = tokens(current)

  def advance(): Token = {
    if (!isAtEnd()) current += 1
    previous()
  }

  def isAtEnd(): Boolean = peek().tokenType == EOF

  def previous(): Token = tokens(current - 1)

  def synchronize(): Unit = {
    advance()

    while (!isAtEnd()) {
      if (previous().tokenType == SEMICOLON) return

      peek().tokenType match {
        case CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN => return
        case _ => advance()
      }
    }
  }
}

class Interpreter

object Interpreter {  // singleton

  final val globals = Environment()
  private var env = globals
  private var locals = collection.mutable.Map[Expr, Int]()

  globals.define("clock", new LoxCallable {
    def arity = 0
    def call(args: List[Any]): Any = java.time.LocalDateTime.now().toEpochSecond(java.time.ZoneOffset.UTC)
  })

  def interpret(program: List[Stmt]): Unit = {
    try 
      for {
        stmt <- program
      } exec(stmt) 
    catch
      case (error: RuntimeError) => {
        println(error)
      }
  }

  private def checkNumberOperand(operator: Token, operand: Any): Unit =
    if (!operand.isInstanceOf[Double]) throw new RuntimeError(operator, "Operand must be a number.")

  private def checkNumberOperands(operator: Token, left: Any, right: Any): Unit =
    (left, right) match {
      case (a: Double, b: Double) => return
      case _ => throw new RuntimeError(operator, "Operands must be numbers.")
    }

  private def exec(stmt: Stmt): Unit = {
    stmt match {
      case Expression(expr) => { 
        eval(expr)
        ()
      }
      case Print(expr) => println(eval(expr))
      case Var(name, Some(expr)) => { // isn't there a way to simplify this
        env.define(name.lexeme, eval(expr))
      }
      case Var(name, None) => env.define(name.lexeme, NotAssigned)
      case Block(statements) => execBlock(statements, env)
      case If(condition, thenDo, elseDo) => {
        if isTruthy(eval(condition)) then exec(thenDo)
        else elseDo.foreach(exec)
      }
      case While(condition, body) => {
        while (isTruthy(eval(condition))) {
          exec(body)
        }
      }
      case func @ Function(name, _, _) => env.define(name.lexeme, LoxFn(func, env, isInitializer = false))
      case Return(keyword, value) => {
        val returnValue = value.map(eval).getOrElse(None)
        throw ReturnException(returnValue)
      }
      case Class(name, superclass, methods) => {

        val superVal = superclass.map(eval)

        superVal.foreach { v =>
          if (!v.isInstanceOf[LoxClass]) {
            throw RuntimeError(name, "Superclass must be a class")
          }
        }

        env.define(name.lexeme, NotAssigned)

        superclass.foreach { sup =>
          env = Environment.withEnclosing(env)
          env.define("super", sup)
        }

        val mmap: Map[String, LoxFn] = Map.from {
          methods.map { method =>
            val isInitializer = method.name.lexeme == "init"
            (method.name.lexeme, LoxFn(method, env, isInitializer))
          }
        }

        val cls = LoxClass(name.lexeme, superVal.map(_.asInstanceOf[LoxClass]), mmap)

        if superclass.isDefined then env = env.enclosing.get

        env.assign(name, cls)
      }
    }
  }

  def execBlock (statements: List[Stmt], next: Environment): Unit = {
    val prev = env
    try
      env = next
      statements.foreach(exec)
    finally
      env = prev
  }

  private def eval(expr: Expr): Any = {
    expr match {
      case Expr.Binary(left, op, right) => {
        val leftVal = eval(left)
        val rightVal = eval(right)
        op.tokenType match {
          case MINUS => {
            checkNumberOperands(op, leftVal, rightVal)
            leftVal.asInstanceOf[Double] - rightVal.asInstanceOf[Double]
          }
          case PLUS => {
            (leftVal, rightVal) match {
              case (a: Double, b: Double) => a + b
              case (a: String, b: String) => a + b
              case _ => throw new RuntimeError(op, "Operands must be two numbers or two strings.")
            }
          }
          case SLASH => {
            checkNumberOperands(op, leftVal, rightVal)
            leftVal.asInstanceOf[Double] / rightVal.asInstanceOf[Double]
          }
          case STAR => {
            checkNumberOperands(op, leftVal, rightVal)
            leftVal.asInstanceOf[Double] * rightVal.asInstanceOf[Double]
          }
          case GREATER => {
            checkNumberOperands(op, leftVal, rightVal)
            leftVal.asInstanceOf[Double] > rightVal.asInstanceOf[Double]
          }
          case GREATER_EQUAL => {
            checkNumberOperands(op, leftVal, rightVal)
            leftVal.asInstanceOf[Double] >= rightVal.asInstanceOf[Double]
          }
          case LESS => {
            checkNumberOperands(op, leftVal, rightVal)
            leftVal.asInstanceOf[Double] < rightVal.asInstanceOf[Double]
          }
          case LESS_EQUAL => {
            checkNumberOperands(op, leftVal, rightVal)
            leftVal.asInstanceOf[Double] <= rightVal.asInstanceOf[Double]
          }
          case BANG_EQUAL => !isEqual(leftVal, rightVal)
          case EQUAL_EQUAL => isEqual(leftVal, rightVal)
          case _ => throw Error("Unreachable eval error")
        }
      }
      case Expr.Grouping(expression) => eval(expression)
      case Expr.Literal(value) => value
      case Expr.Unary(op, right) => {
        val rightVal = eval(right)
        checkNumberOperand(op, right)
        op.tokenType match {
          case MINUS => -rightVal.asInstanceOf[Double]
          case BANG => !isTruthy(rightVal)
          case _ => throw Error("Unreachable eval error") // unreachable todo fix
        }
      }
      case Expr.Variable(name) => lookupVariable(name, expr)
      case assignExpr @ Expr.Assign(name, valueExpr) => {
        val value = eval(valueExpr)        
        locals.get(assignExpr).fold(globals.assign(name, value)) { depth =>
          env.assignAt(depth, name, value)
        }
      }
      case Expr.Logical(left, op, right) => {
        val leftVal = eval(left)
        op.tokenType match {
          case OR => if (isTruthy(leftVal)) leftVal else eval(right)
          case AND => if (!isTruthy(leftVal)) leftVal else eval(right)
          case _ => throw Error("Unreachable eval error") // unreachable
        }
      }
      case Expr.Call(callee, paren, arguments) => {
        val calleeVal = eval(callee)
        val argvals = arguments.map(eval)

        if (!calleeVal.isInstanceOf[LoxCallable]) {
          throw new RuntimeError(paren, "Can only call functions and classes.")
        }

        val func = calleeVal.asInstanceOf[LoxCallable]

        if (argvals.length != func.arity) {
          throw new RuntimeError(paren, f"Expected ${calleeVal.asInstanceOf[LoxCallable].arity} arguments but got ${argvals.length}.")
        }

        return func.call(argvals)
      }
      case Expr.Get(expr, name) => {
        val obj = eval(expr)
        obj match
          case instance: LoxInstance => instance.get(name)
          case _ =>
            throw RuntimeError(name, "Only instances have properties.")
      }
      case Expr.Set(obj, name, value) => {
        val objVal = eval(obj)
        if (!objVal.isInstanceOf[LoxInstance]) {
          throw RuntimeError(name, "Only instances have fields.")
        } else {
          objVal.asInstanceOf[LoxInstance].set(name, eval(value))
          return value
        }
      }
      case Expr.This(keyword) => lookupVariable(keyword, expr)
      case Expr.Super(keyword, methodToken) =>
        val dist = locals(expr)
        val superclass = env.getAt(dist, "super").asInstanceOf[LoxClass]
        val obj = env.getAt(dist - 1, "this").asInstanceOf[LoxInstance]
        val method = superclass.findMethod(methodToken.lexeme)
        method.map(_.bind(obj)).getOrElse(throw RuntimeError(methodToken, s"Undefined property ${methodToken.lexeme}"))
    }
  }

  private def lookupVariable(name: Token, expr: Expr) =
    locals.get(expr)
    .fold { globals.get(name) } { depth => env.getAt(depth, name.lexeme) }

  private def isTruthy(value: Any): Boolean = {
    value match {
      case None => false
      case false => false
      case _ => true
    }
  }

  private def isEqual(a: Any, b: Any): Boolean = {
    if (a == None && b == None) return true
    if (a == None) return false
    a == b
  }

  def resolve(expr: Expr, depth: Int): Unit = {
    locals += (expr -> depth)
  }
}

object Parser {
  case class ParseError(message: String) extends RuntimeException(message)
}

case class RuntimeError(token: Token, message: String) extends RuntimeException(message)
case class ReturnException(value: Any) extends RuntimeException(null, null, false, false)

abstract trait LoxCallable {
  def arity: Int
  def call(args: List[Any]): Any
}

class LoxFn(val declaration: Function, val closure: Environment, val isInitializer: Boolean) extends LoxCallable {
  override def arity = declaration.params.length

  override def call(args: List[Any]): Any = {
    val env = Environment.withEnclosing(closure)
    for {
      (param, arg) <- declaration.params.zip(args)
    } env.define(param.lexeme, arg)

    try
      Interpreter.execBlock(declaration.body, env)
    catch
      case ReturnException(value) => {
        if (isInitializer) return closure.getAt(0, "this")
        else return value 
      }
    return None // todo
  }

  def bind(instance: LoxInstance): LoxFn = {
    val env = Environment.withEnclosing(closure)
    env.define("this", instance)
    return LoxFn(declaration, env, isInitializer)
  }

  override def toString: String = "<fn " + declaration.name.lexeme + ">"
}
