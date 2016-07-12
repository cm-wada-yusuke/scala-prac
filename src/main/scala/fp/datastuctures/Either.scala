package fp.datastuctures

import fp.datastuctures.list._

/**
  * Either
  */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case l@Left(e) => l
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case r@Right(a) => r
    case l@Left(e) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (Right(a), Right(b)) => Right(f(a, b))
    case (Right(a), l@Left(ee)) => l
    case (l@Left(e), _) => l
  }

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    List.foldRight(es, Right(Nil): Either[E, List[A]])(
      (a, b) => (a, b) match {
        case (la@Left(e), _) => la
        case (_, lb@Left(e)) => lb
        case (Right(a), Right(b)) => Right(List.append(List(a), b))
      }
    )


  def travers[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    List.foldRight(as, Right(Nil): Either[E, List[B]])(
      (a, b) => (a, b) match {
        case (_, lb@Left(e)) => lb
        case (a, Right(b)) => f(a) match {
          case l@Left(e) => l
          case Right(fa) => Right(List.append(List(fa), b))
        }
      }
    )

}


