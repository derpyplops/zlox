package org.zlox.zlox.Main

import zio._
import zio.UIO
import scala.io.Source
import java.io.IOException
import scala.util.boundary, boundary.break
import org.zlox.zlox.Token._
import TokenType.*
import org.zlox.zlox.Environment.Environment
import org.zlox.zlox.Main.Parser.ParseError

object Lox extends ZIOAppDefault {

  var hadError: Boolean = false
  var hadRuntimeError: Boolean = false

  override def run =
    program.provide(
      ZLayer.succeed(ZIOAppArgs(Chunk.empty)),
      ZLayer.succeed(Console.ConsoleLive)
    )

  val program: ZIO[Console & ZIOAppArgs, IOException, Unit] = for {
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
    _ <- exit(exitCode)
  } yield ()

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
    _=println(tokens.toList)
    stmts <- Parser(tokens).parse()
  } yield Interpreter.interpret(stmts)

  def error(line: Int, message: String) = {
    report(line, "", message)
  }

  def report(line: Int, where: String, message: String) = {
    hadError = true
    Console.printLine(s"[line $line] Error$where: $message")
  }

  def handleParseError(error: ParseError) = {
    println("adf")
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

sealed trait Expr
case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
case class Grouping(expression: Expr) extends Expr
case class Literal(value: Any) extends Expr
case class Unary(operator: Token, right: Expr) extends Expr
case class Variable(name: Token) extends Expr
case class Assign(name: Token, value: Expr) extends Expr

sealed trait Stmt
case class Expression(expr: Expr) extends Stmt
case class Print(expr: Expr) extends Stmt
case class Var(name: Token, initializer: Option[Expr]) extends Stmt

def printAst(expr: Expr): String = expr match {
  case Binary(left, op, right) => s"(${op.lexeme} ${printAst(left)} ${printAst(right)})"
  case Grouping(expr) => s"(group ${printAst(expr)})"
  case Literal(value) => if (value == None) "nil" else value.toString
  case Unary(op, right) => s"(${op.lexeme} ${printAst(right)})"
  case Variable(name) => s"name=${name}"
}

class Parser(tokens: Array[Token]) {
  var current: Int = 0

  def parse(): UIO[List[Stmt]] = {
    try
      var stmts: List[Stmt] = List()
      while (!isAtEnd()) {
        val maybeDeclaration = declaration()
        stmts = stmts ++ maybeDeclaration
      }
      ZIO.succeed(stmts)
    catch
      case (error: ParseError) => {
        println("adf")
        throw error
      }
      case e: Exception => {
        println("other error")
        throw e
      }
  }

  def declaration(): Option[Stmt] = {
    try
      if (matchToken(VAR)) Some(varDeclaration())
      else return Some(statement())
    catch
      case e: ParseError => {
        synchronize()
        None
      }
  }

  def varDeclaration(): Stmt = {
    val name = consume(IDENTIFIER, "Expect identifier")
    val initializer = Option.when(matchToken(EQUAL))(expression())
    consume(SEMICOLON, "Expect ;")
    Var(name, initializer)
  }

  def statement(): Stmt = {
    print("statemnt")
    if (peek().tokenType == PRINT) printStmt()
    else exprStmt()
  }

  def exprStmt(): Stmt = {
    val exprStmt = Expression(expression())
    consume(SEMICOLON, "Expected semicolon")
    exprStmt
  }

  def printStmt(): Stmt = {
    println("print stmt")
    consume(PRINT, "Expected print")
    val expr = expression()
    consume(SEMICOLON, "Expected semicolon")
    Print(expr)
  }

  def expression(): Expr = assignment()

  def assignment(): Expr = {
    val left = equality() // every valid assignment target happens to also be valid syntax as a normal expression
    // eg. newPoint(x + 2, 0).y = 3;

    if (matchToken(EQUAL)) {
      val equals = previous()
      val right = assignment()
      left match
        case Variable(name) => Assign(name, right)
        case _ => {
          error(equals, "Invalid assignment target")
          left
        }
    } else {
      left
    }
  }

  def equality(): Expr = {
    var expr = comparison()

    while (matchToken(BANG_EQUAL, EQUAL_EQUAL)) {
      val operator = previous()
      val right = comparison()
      expr = Binary(expr, operator, right)
    }

    expr
  }

  def comparison(): Expr = {
    var expr = term()

    while (matchToken(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
      val operator = previous()
      val right = term()
      expr = Binary(expr, operator, right)
    }

    expr
  }

  def term(): Expr = {
    var expr = factor()

    while (matchToken(MINUS, PLUS)) {
      val operator = previous()
      val right = factor()
      expr = Binary(expr, operator, right)
    }

    expr
  }

  def factor(): Expr = {
    var expr = unary()

    while (matchToken(SLASH, STAR)) {
      val operator = previous()
      val right = unary()
      expr = Binary(expr, operator, right)
    }

    expr
  }

  def unary(): Expr = {
    if (matchToken(BANG, MINUS)) {
      val operator = previous()
      val right = unary()
      Unary(operator, right)
    } else {
      primary()
    }
  }

  def primary(): Expr = {
    if (matchToken(FALSE)) return Literal(false)
    if (matchToken(TRUE)) return Literal(true)
    if (matchToken(NIL)) return Literal(None)
    if (matchToken(NUMBER, STRING)) {
      return Literal(previous().literal)
    }
    if (matchToken(LEFT_PAREN)) {
      val expr = expression()
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      return Grouping(expr)
    }
    if (matchToken(IDENTIFIER)) return Variable(previous())
    throw error(peek(), "Expect expression.")
  }

  def consume(tokenType: TokenType, message: String): Token = {
    if (check(tokenType)) return advance()
    throw error(peek(), message)
  }

  def error(token: Token, message: String): ParseError = {
    Lox.error(token.line, message)
    ParseError(message)
  } 

  // def left_assoc(lower: () => Expr, tokenTypes: TokenType*): Expr = {
  //   var expr = lower()

  //   while (matchToken(tokenTypes*)) {
  //     val operator = previous()
  //     val right = lower()
  //     expr = Binary(expr, operator, right)
  //   }

  //   expr
  // }


  def matchToken(types: TokenType*): Boolean = {
    boundary:
      for (tokenType <- types) {
        if (check(tokenType)) {
          advance()
          break(true)
        }
      }

      false
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

object Interpreter {  // singleton
  private val env = Environment()
  def interpret(program: List[Stmt]): Unit = {
    try 
      for {
        stmt <- program
      } exec(stmt)
    catch
      case (error: RuntimeError) => Lox.runtimeError(error)
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
      case Var(name, initializer) => {
        println(name)
        println(initializer)
        env.define(name.lexeme, initializer.map(eval))
      }
    }
  }

  private def eval(expr: Expr): Any = {
    expr match {
      case Binary(left, op, right) => {
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
          case _ => null
        }
      }
      case Grouping(expression) => eval(expression)
      case Literal(value) => value
      case Unary(op, right) => {
        val rightVal = eval(right)
        checkNumberOperand(op, right)
        op.tokenType match {
          case MINUS => -rightVal.asInstanceOf[Double]
          case BANG => !isTruthy(rightVal)
          case _ => null // unreachable todo fix
        }
      }
      case Variable(name) => env.get(name)
      case Assign(name, value) => env.assign(name, eval(value))
    }
  }

  private def isTruthy(value: Any): Boolean = {
    value match {
      case null => false
      case false => false
      case _ => true
    }
  }

  private def isEqual(a: Any, b: Any): Boolean = {
    if (a == null && b == null) return true
    if (a == null) return false
    a == b
  }
}

object Parser {
  case class ParseError(message: String) extends RuntimeException(message)
}

case class RuntimeError(token: Token, message: String) extends RuntimeException(message)