package chapter5

trait Stream[+A] {
  def headOption: Option[A] =
    this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

  def headOption_2: Option[A] =
    foldRight(None: Option[A])((h, t) => Some(h))

  def toList: List[A] = {
    def go(s: Stream[A], acc: List[A]): List[A] =
      s match {
        case Cons(h, t) => go(t(), h() :: acc)
        case _ => acc
      }

    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 1 => Cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => Cons(h(), Empty)
      case _ => Empty
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if n > 0 => t().drop(n-1)
      case _ => Empty
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => Cons(h(), t().takeWhile(p))
      case _ => Empty
    }

  def takeWhile_2(p: A => Boolean): Stream[A] =
    foldRight(Empty[A])((h, t) => if (p(h)) Cons(h, t) else Empty)

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def map[B](f: A=>B): Stream[B] =
    foldRight(Empty[B])((h, t) => Cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Empty[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)((h, t) => Cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty[B])((h, t) => f(h) append t)


}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] // () => A : サンク(thunk)

object Stream {
  def cons[A](hd: A, tl: Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    lazy val as: Stream[A] = Cons(() => a, () => as)
    as
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def loop(n1: Int, n2: Int): Stream[Int] = {
      cons(n1, loop(n2, n1+n2))
    }
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => Empty
    }

  def fibViaUnfold: Stream[Int] =
    unfold((0, 1)){ case (f0, f1) => Some((f0, (f1, f0+f1)))}

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n+1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  val onesViaUnfold: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  val ones: Stream[Int] = Stream.cons(1, ones)

}
