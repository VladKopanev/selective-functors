package com.kopanev.cats.selective

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.Console.io._

object OverUnderSimpleDemo extends IOApp {

  import cats.implicits._
  import Selective._

  override def run(args: List[String]): IO[ExitCode] = {
    val over = ifS[Over[String, ?], Boolean](Over("a"))(Over("b"))(Over("c")) *> Over("d") *> whenS[Over[String, ?]](Over("e"))(Over("f"))(Over("g"))
    val under = ifS[Under[String, ?], Boolean](Under("a"))(Under("b"))(Under("c")) *> Under("d") *> whenS[Under[String, ?]](Under("e"))(Under("f"))(Under("g"))

    for {
      (o, u) <- IO.pure(over) product IO.pure(under)
      _ <- putStrLn(o)
      _ <- putStrLn(u)
    } yield ExitCode.Success
  }
}
