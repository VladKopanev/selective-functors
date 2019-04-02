package com.kopanev.cats.selective

import cats._
import cats.kernel.Monoid

trait BuildTask[K, V] {

  def run[F[_]: Selective](f: K => F[V]): F[V]
}

object BuildTask {
  import Selective._
  import cats.implicits._

  type Script[K, V] = K => Option[BuildTask[K, V]]

  def apply[K, V, F[_]](f: (K => F[V]) => F[V]): BuildTask[K, V] = new BuildTask[K, V] {
    override def run[G[_]: Selective](fa: K ~> Id): G[V] = f.apply(fa.apply _)
  }

  def dependenciesOver[K, V, F[_] <: Over[List[V], ?]](task: BuildTask[K, V])(implicit lMonoid: Monoid[List[K]]): List[K] =
    task.run(k => Over(List(k))).getOver

  def dependenciesUnder[K, V](task: BuildTask[K, V])(implicit lMonoid: Monoid[List[K]]): List[K] =
    task.run[Under[List[K], ?]](k => Under(List(k))).getUnder

  def tar[F[_]: Applicative, V: Monoid](artifacts: List[F[V]]): F[V] = Applicative[F].map(artifacts.sequence)(_.fold)

  def compile[F[_]: Applicative, V: Monoid](artifacts: List[F[V]]): F[V] = Applicative[F].map(artifacts.sequence)(_.fold)

  def parse[F[_]: Functor](cfg: F[String]): F[Boolean] = cfg.map(_ => false)

}