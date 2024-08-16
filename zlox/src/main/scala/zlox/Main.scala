import zio._
import zio.UIO
import scala.io.Source
import java.io.IOException
import scala.util.boundary, boundary.break

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
    expr <- Parser(tokens).parse().orDie // TODO handle error
  } yield Interpreter.interpret(expr)

  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  def report(line: Int, where: String, message: String): Unit = {
    Console.printError(s"[line $line] Error$where: $message")
    hadError = true
  }

  def runtimeError(error: RuntimeError) = {
    println(error.getMessage() +"\n[line " + error.token.line + "]")
    hadRuntimeError = true
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

    tokens :+= Token(TokenType.EOF, "", null, line)
    return ZIO.succeed(tokens)
  }

  def isAtEnd(): Boolean = {
    current >= source.length
  }

  def scanToken(): Unit = {
    val c = advance()
    println("c: " + c)
    c match {
      case '(' => addToken(TokenType.LEFT_PAREN)
      case ')' => addToken(TokenType.RIGHT_PAREN)
      case '{' => addToken(TokenType.LEFT_BRACE)
      case '}' => addToken(TokenType.RIGHT_BRACE)
      case ',' => addToken(TokenType.COMMA)
      case '.' => addToken(TokenType.DOT)
      case '-' => addToken(TokenType.MINUS)
      case '+' => addToken(TokenType.PLUS)
      case ';' => addToken(TokenType.SEMICOLON)
      case '*' => addToken(TokenType.STAR)
      case '!' => addToken(if (matchChar('=')) TokenType.BANG_EQUAL else TokenType.BANG)
      case '=' => addToken(if (matchChar('=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL)
      case '<' => addToken(if (matchChar('=')) TokenType.LESS_EQUAL else TokenType.LESS)
      case '>' => addToken(if (matchChar('=')) TokenType.GREATER_EQUAL else TokenType.GREATER)
      case '/' => if (matchChar('/')) {
        while (peek() != '\n' && !isAtEnd()) advance()
      } else {
        addToken(TokenType.SLASH)
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
    val tokenType = keywords.getOrElse(text, TokenType.IDENTIFIER)
    addToken(tokenType)
  }

  def advance(): Char = {
    current += 1
    source.charAt(current - 1)
  }

  def addToken(tokenType: TokenType): Unit = {
    addToken(tokenType, null)
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
    addToken(TokenType.STRING, value)
  }

  def number(): Unit = {
    while (isDigit(peek())) advance()

    if (peek() == '.' && isDigit(peekNext())) {
      advance()
      while (isDigit(peek())) advance()
    }

    addToken(TokenType.NUMBER, source.substring(start, current).toDouble)
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
    "and" -> TokenType.AND,
    "class" -> TokenType.CLASS,
    "else" -> TokenType.ELSE,
    "false" -> TokenType.FALSE,
    "for" -> TokenType.FOR,
    "fun" -> TokenType.FUN,
    "if" -> TokenType.IF,
    "nil" -> TokenType.NIL,
    "or" -> TokenType.OR,
    "print" -> TokenType.PRINT,
    "return" -> TokenType.RETURN,
    "super" -> TokenType.SUPER,
    "this" -> TokenType.THIS,
    "true" -> TokenType.TRUE,
    "var" -> TokenType.VAR,
    "while" -> TokenType.WHILE
  )
}

enum TokenType {
  case LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE, COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,
       BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL,
       IDENTIFIER, STRING, NUMBER,
       AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR, PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,
       EOF
}

case class Token(tokenType: TokenType, lexeme: String, literal: Any, line: Int)

sealed trait Expr
case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
case class Grouping(expression: Expr) extends Expr
case class Literal(value: Any) extends Expr
case class Unary(operator: Token, right: Expr) extends Expr

def printAst(expr: Expr): String = expr match {
  case Binary(left, op, right) => s"(${op.lexeme} ${printAst(left)} ${printAst(right)})"
  case Grouping(expr) => s"(group ${printAst(expr)})"
  case Literal(value) => if (value == null) "nil" else value.toString
  case Unary(op, right) => s"(${op.lexeme} ${printAst(right)})"
}

class Parser(tokens: Array[Token]) {
  var current: Int = 0

  case class ParseError() extends RuntimeException

  def parse(): ZIO[Any, ParseError, Expr] = {
    try
      ZIO.succeed(expression())
    catch 
      case (error: ParseError) => ZIO.fail(error)
  }

  def expression(): Expr = equality()

  def equality(): Expr = {
    var expr = comparison()

    while (matchToken(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL)) {
      val operator = previous()
      val right = comparison()
      expr = Binary(expr, operator, right)
    }

    expr
  }

  def comparison(): Expr = {
    var expr = term()

    while (matchToken(TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL)) {
      val operator = previous()
      val right = term()
      expr = Binary(expr, operator, right)
    }

    expr
  }

  def term(): Expr = {
    var expr = factor()

    while (matchToken(TokenType.MINUS, TokenType.PLUS)) {
      val operator = previous()
      val right = factor()
      expr = Binary(expr, operator, right)
    }

    expr
  }

  def factor(): Expr = {
    var expr = unary()

    while (matchToken(TokenType.SLASH, TokenType.STAR)) {
      val operator = previous()
      val right = unary()
      expr = Binary(expr, operator, right)
    }

    expr
  }

  def unary(): Expr = {
    if (matchToken(TokenType.BANG, TokenType.MINUS)) {
      val operator = previous()
      val right = unary()
      Unary(operator, right)
    } else {
      primary()
    }
  }

  def primary(): Expr = {
    if (matchToken(TokenType.FALSE)) return Literal(false)
    if (matchToken(TokenType.TRUE)) return Literal(true)
    if (matchToken(TokenType.NIL)) return Literal(null)
    if (matchToken(TokenType.NUMBER, TokenType.STRING)) {
      return Literal(previous().literal)
    }
    if (matchToken(TokenType.LEFT_PAREN)) {
      val expr = expression()
      consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.")
      return Grouping(expr)
    }
    throw error(peek(), "Expect expression.")
  }

  def consume(tokenType: TokenType, message: String): Token = {
    if (check(tokenType)) return advance()
    throw error(peek(), message)
  }

  def error(token: Token, message: String): ParseError = {
    Lox.error(token.line, message)
    ParseError()
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

  def isAtEnd(): Boolean = peek().tokenType == TokenType.EOF

  def previous(): Token = tokens(current - 1)

  def synchronize(): Unit = {
    advance()

    while (!isAtEnd()) {
      if (previous().tokenType == TokenType.SEMICOLON) return

      peek().tokenType match {
        case TokenType.CLASS | TokenType.FUN | TokenType.VAR | TokenType.FOR | TokenType.IF | TokenType.WHILE | TokenType.PRINT | TokenType.RETURN => return
        case _ => advance()
      }
    }
  }
}

object Interpreter {  // singleton
  def interpret(expr: Expr): Unit = {
    try 
      val value = eval(expr)
      println(value)
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
  private def eval(expr: Expr): Any = {
    expr match {
      case Binary(left, op, right) => {
        val leftVal = eval(left)
        val rightVal = eval(right)
        op.tokenType match {
          case TokenType.MINUS => {
            checkNumberOperands(op, leftVal, rightVal)
            leftVal.asInstanceOf[Double] - rightVal.asInstanceOf[Double]
          }
          case TokenType.PLUS => {
            (leftVal, rightVal) match {
              case (a: Double, b: Double) => a + b
              case (a: String, b: String) => a + b
              case _ => throw new RuntimeError(op, "Operands must be two numbers or two strings.")
            }
          }
          case TokenType.SLASH => {
            checkNumberOperands(op, leftVal, rightVal)
            leftVal.asInstanceOf[Double] / rightVal.asInstanceOf[Double]
          }
          case TokenType.STAR => {
            checkNumberOperands(op, leftVal, rightVal)
            leftVal.asInstanceOf[Double] * rightVal.asInstanceOf[Double]
          }
          case TokenType.GREATER => {
            checkNumberOperands(op, leftVal, rightVal)
            leftVal.asInstanceOf[Double] > rightVal.asInstanceOf[Double]
          }
          case TokenType.GREATER_EQUAL => {
            checkNumberOperands(op, leftVal, rightVal)
            leftVal.asInstanceOf[Double] >= rightVal.asInstanceOf[Double]
          }
          case TokenType.LESS => {
            checkNumberOperands(op, leftVal, rightVal)
            leftVal.asInstanceOf[Double] < rightVal.asInstanceOf[Double]
          }
          case TokenType.LESS_EQUAL => {
            checkNumberOperands(op, leftVal, rightVal)
            leftVal.asInstanceOf[Double] <= rightVal.asInstanceOf[Double]
          }
          case TokenType.BANG_EQUAL => !isEqual(leftVal, rightVal)
          case TokenType.EQUAL_EQUAL => isEqual(leftVal, rightVal)
          case _ => null
        }
      }
      case Grouping(expression) => eval(expression)
      case Literal(value) => value
      case Unary(op, right) => {
        val rightVal = eval(right)
        checkNumberOperand(op, right)
        op.tokenType match {
          case TokenType.MINUS => -rightVal.asInstanceOf[Double]
          case TokenType.BANG => !isTruthy(rightVal)
          case _ => null // unreachable todo fix
        }
      }
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

case class RuntimeError(token: Token, message: String) extends RuntimeException(message)