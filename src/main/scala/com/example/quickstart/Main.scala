package com.example.quickstart

import cats.Applicative
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp, Sync}
import io.circe.generic.semiauto._
import io.circe.{Decoder, Encoder}
import org.http4s.Method.GET
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.implicits._
import org.http4s.{EntityDecoder, EntityEncoder}

import scala.concurrent.ExecutionContext.global
import scala.io.StdIn

object Main extends IOApp {
  private val dsl = new Http4sClientDsl[IO] {}
  import dsl._

  private val clientIO = BlazeClientBuilder[IO](global).resource.use { client => IO(client) }
  private val jokeCacheIO = Ref.of[IO, JokeCache](JokeCache())

  def run(args: List[String]): IO[ExitCode] = myApplication

  private def myApplication: IO[ExitCode] = IO.suspend {
    for {
      client <- clientIO
      joke <- getJoke(client)
      _ <- IO(println(joke.value))
      _ <- IO(println("Do you want another truth about Chuck Norris? y/n"))
      answer <- IO(StdIn.readLine())
      reaction <- if (answer == "y") myApplication else IO(ExitCode.Success)
    } yield reaction
  }

  private def getJoke(client: Client[IO]) =
    client
      .expect[Joke](GET(uri"https://api.chucknorris.io/jokes/random"))
      .redeemWith(recoverWithCachedJoke, updateCache)

  private val updateCache = { joke: Joke =>
    for {
      jokeCacheRef <- jokeCacheIO
      _ <- jokeCacheRef.update(old => old.next(joke))
    } yield joke
  }

  private lazy val recoverWithCachedJoke = { _: Throwable =>
    for {
      jokeCacheRef <- jokeCacheIO
      cachedJoke <- jokeCacheRef.get
      joke <- IO(cachedJoke.joke)
    } yield joke
  }
}

final case class JokeCache(joke: Joke = Joke("Corona virus washes his hands after meeting Chuck Norris.")) {
  def next(joke: Joke): JokeCache = JokeCache(joke)
}

final case class Joke(value: String) extends AnyVal
object Joke {
  implicit val jokeDecoder: Decoder[Joke] = deriveDecoder[Joke]

  implicit def jokeEntityDecoder[F[_] : Sync]: EntityDecoder[F, Joke] = jsonOf

  implicit val jokeEncoder: Encoder[Joke] = deriveEncoder[Joke]

  implicit def jokeEntityEncoder[F[_] : Applicative]: EntityEncoder[F, Joke] = jsonEncoderOf
}
