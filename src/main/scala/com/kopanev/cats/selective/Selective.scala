package com.kopanev.cats.selective

import cats.{Applicative, Monoid, Show}

trait Selective[F[_]] extends Applicative[F] {

  def select[A, B](fab: F[Either[A, B]])(f: F[A => B]): F[B]

  def branch[A, B, C](selector: F[Either[A, B]])(caseLeft: F[A => C])(caseRight: F[B => C]): F[C] = {
    def discardLeft: Either[A, B] => Either[A, Either[B, C]] = _.map(Left(_))

    def convertCaseLeft: (A => C) => A => Either[B, C] = f => f.andThen(Right(_))

    val wrapChoice: F[Either[A, Either[B, C]]] = map(selector)(discardLeft)

    val wrapLeftCase: F[A => Either[B, C]] = map(caseLeft)(convertCaseLeft)

    val wrapSec: F[Either[B, C]] = select(wrapChoice)(wrapLeftCase)
    select(wrapSec)(caseRight)
  }

  def ifS[A](f: F[Boolean])(left: F[A])(right: F[A]): F[A] =
    branch(map(f)(Either.cond(_, (), ())))(map[A, Unit => A](left)(Function.const))(map[A, Unit => A](right)(Function.const))

  def whenS(f: F[Boolean])(left: F[Unit])(right: F[Unit]): F[Unit] =
    ifS(f)(left)(right)

  def bindBool[A](b: F[Boolean])(f: Boolean => F[A]): F[A] =
    ifS(b)(f(true))(f(false))

  def orS(a: F[Boolean], b: F[Boolean]): F[Boolean] = ifS(a)(pure(true))(b)

  def andS(a: F[Boolean], b: F[Boolean]): F[Boolean] = ifS(a)(b)(pure(false))

  def fromOptionS[A](f: F[A])(fOpt: F[Option[A]]): F[A] =
    select(map(fOpt)(_.fold[Either[Unit, A]](Left(()))(a => Right(a))))(map(f)(x => _ => x))
}

object Selective {

  case class Over[M, A](getOver: M) //basically a Const
  case class Under[M, A](getUnder: M) //basically a Const

  def apply[M[_] : Selective]: Selective[M] = implicitly[Selective[M]]

  implicit def catsSelectiveInstanceForOver[M: Monoid, B]: Selective[Over[M, ?]] = new Selective[Over[M, ?]] {

    override def pure[A](x: A): Over[M, A] = Over[M, A](Monoid[M].empty)

    override def select[A, B](fab: Over[M, Either[A, B]])(f: Over[M, A => B]): Over[M, B] =
      Over(Monoid[M].combine(fab.getOver, f.getOver))

    override def ap[A, B](ff: Over[M, A => B])(fa: Over[M, A]): Over[M, B] =
      Over(Monoid[M].combine(ff.getOver, fa.getOver))
  }

  implicit def catsSelectiveInstanceForUnder[M: Monoid, B]: Selective[Under[M, ?]] = new Selective[Under[M, ?]] {

    override def pure[A](x: A): Under[M, A] = Under[M, A](Monoid[M].empty)

    override def select[A, B](fab: Under[M, Either[A, B]])(f: Under[M, A => B]): Under[M, B] =
      Under(fab.getUnder)

    override def ap[A, B](ff: Under[M, A => B])(fa: Under[M, A]): Under[M, B] =
      Under(Monoid[M].combine(ff.getUnder, fa.getUnder))
  }

  implicit def showInstanceForOver[M, A]: Show[Over[M, A]] = _.toString
  implicit def showInstanceForUnder[M, A]: Show[Under[M, A]] = _.toString

  def ifS[F[_] : Selective, A](f: F[Boolean])(left: F[A])(right: F[A]): F[A] = Selective[F].ifS(f)(left)(right)

  def whenS[F[_] : Selective](f: F[Boolean])(left: F[Unit])(right: F[Unit]): F[Unit] = Selective[F].whenS(f)(left)(right)
}
