package com.kopanev.cats.selective

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.Console.io._
import cats.implicits._

object BuildTaskDemo extends IOApp {

  import BuildTask._
  import Selective._

  override def run(args: scala.List[String]): IO[ExitCode] = {

    type FilePath = String

    def script[F[_]: Selective]: Script[FilePath, String] = {
      case "release.tar" => Some(BuildTask[FilePath, String, F[String]](fetch => tar(List(fetch("LICENSE"), fetch("exe")))))
      case "exe" => Some(BuildTask[FilePath, String, F[String]] { fetch =>
        val src = fetch("src.ml")
        val cfg = fetch("config")
        val libc = fetch("lib.c")
        val libml = fetch("lib.ml")

        compile(List(src, ifS(parse(cfg))(libc)(libml)))
      })
      case _ => None
    }

    putStrLn(dependenciesOver[FilePath, String, Over[List[String], ?]](script.apply("release.tar"))) *> IO.pure(ExitCode.Success)
  }
}

