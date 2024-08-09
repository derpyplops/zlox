import zio._
import zio.UIO
import scala.io.Source
import java.io.IOException

object Lox extends ZIOAppDefault {

  var hadError: Boolean = false

  override def run =
    program.provide(
      ZLayer.succeed(ZIOAppArgs(Chunk.empty)),
      ZLayer.succeed(Console.ConsoleLive)
    )

  val program: ZIO[Console & ZIOAppArgs, IOException, Unit] = for {
    args <- ZIOAppArgs.getArgs
    exitCode <- args.toList match {
      case Nil => runPrompt.as(ExitCode.success)
      case file :: Nil => runFile(file).map { _ =>
        if (hadError) ExitCode(65) else ExitCode.success
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
    _ <- ZIO.foreach(tokens)(token => Console.printLine(token.toString))
  } yield ()

  def error(line: Int, message: String): Unit = {
    report(line, "", message)
  }

  def report(line: Int, where: String, message: String): Unit = {
    Console.printError(s"[line $line] Error$where: $message")
    hadError = true
  }
}

class Scanner(val source: String) {
  var tokens: Array[Token] = Array()
  var start: Int = 0
  var current: Int = 0
  var line: Int = 1

  def scanTokens: UIO[Array[Token]] = {
    println("hi")
    while (!isAtEnd()) {
      start = current
      scanToken()
    }

    tokens :+ Token(TokenType.EOF, "", null, line)
    return ZIO.succeed(tokens)
  }

  def isAtEnd(): Boolean = {
    println("current: " + current)
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
    println("here")
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