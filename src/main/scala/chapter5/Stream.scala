package chapter5

import chapter5.Stream.unfold

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

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this){
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)){
      case (Cons(h, t), 1) => Some((h(), (t(), 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n-1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold(this, s2) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(this, s2) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some((Option(h()), Option.empty[B]), (t(), Empty[B]))
      case (Empty, Cons(h, t)) => Some((Option.empty[A], Option(h())), (Empty[A], t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Option(h1()), Option(h2())), (t1(), t2()))
    }

  def startWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined) forAll {
      case (h1, h2) => h1 == h2
    }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some(s, s drop 1)
    } append Stream(Empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, Cons(b2, p1._2))
  })._2

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
