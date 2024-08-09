import zio._
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
  val tokens: List[Token] = List()
  var start: Int = 0
  var current: Int = 0
  var line: Int = 1

  def scanTokens: UIO[List[Token]] = ZIO.succeed(List())
}

enum TokenType {
  case LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE, COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,
       BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL,
       IDENTIFIER, STRING, NUMBER,
       AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR, PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,
       EOF
}

case class Token(tokenType: TokenType, lexeme: String, literal: Any, line: Int)