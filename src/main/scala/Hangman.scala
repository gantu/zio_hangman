import zio.App
import zio.console._
import java.io.{File, IOException}
import zio._
import scala.io.{BufferedSource, Source}

object Hangman extends App {

  def run(args: List[String]) = hangman.fold(_ => 1, _ => 0)

  val hangman : ZIO[Console, IOException, Unit] = for {
      _ <- putStrLn("Welcome!")
      name <- getName
      _ <- putStrLn(s"Hi $name. Let's begin!")
      word <- chooseWord
      state = State(name, Set(), word)
      _ <- renderState(state)
      _ <- gameLoop(state)
    } yield()

  def gameLoop(state: State): ZIO[Console, IOException, State] = for {
      guess <- getChoice
      state <- IO.succeed(state.copy(guesses = state.guesses + guess))
      _ <- renderState(state)
      loop <- if (state.playerWon) putStrLn(s"Congrats ${state.name} you won the game!").const(false)
              else if (state.playerLost) putStrLn(s"Sorry ${state.name} you lost the game. The word was ${state.word}").map( _ => false).const(false)
              else if (state.word.contains(guess)) putStrLn(s"${state.name} correct guess!").const(true)
              else putStrLn(s"${state.name} that is wrong but keep trying").const(true)
      state <- if (loop) gameLoop(state) else IO.succeed(state)
    } yield state

  //zy val dictionary: List[String] = scala.io.Sources.fromResource("words.txt").getLines.toList
  val getChoice: ZIO[Console, IOException, Char] = for {
      line <- putStrLn(s"Please enter a letter") *> getStrLn
      char <- line.toLowerCase.trim.headOption match {
        case None => putStrLn(s"No character entered") *> getChoice
        case Some(x) => IO.succeed(x)
      }
    } yield char

  val getName: ZIO[Console, IOException, String] = putStrLn("Your name please") *> getStrLn

  def nextInt(max: Int): IO[Nothing, Int] = IO.succeedLazy(scala.util.Random.nextInt(max))

  def chooseWord: IO[IOException, String] = for {
      d <- dictionary
      rand <- nextInt(d.length)
    } yield d(rand)

  def renderState(state: State): ZIO[Console, IOException, Unit] = {
    val word = state.word.toList.map( c =>
        if (state.guesses.contains(c)) s" $c " else " "
      ).mkString("")

    val line = List.fill(state.word.length)(" - ").mkString("")
    val guesses = " Guesses: " + state.guesses.toList.sorted.mkString("")
    val text = word + "\n"+ line + "\n\n" + guesses + "\n"
    putStrLn(text)
  }

  val dictionary: IO[IOException, List[String]] = openFile("words.txt").bracket(closeFile(_)) { file =>
      IO.succeedLazy(file.getLines.toList)
  }
  def openFile(s:String): IO[IOException, BufferedSource] = IO.succeedLazy(Source.fromResource(s))
  def closeFile(f: BufferedSource): UIO[Unit] = IO.succeed(f.close)
}

case class State(name: String, guesses: Set[Char] = Set.empty[Char], word: String) {
  final def failures: Int = (guesses -- word.toSet).size
  final def playerLost: Boolean = failures > 10
  final def playerWon: Boolean = (word.toSet -- guesses).size == 0
}
