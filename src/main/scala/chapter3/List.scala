package chapter3

/* 抽象インタフェース
+A: 共変パラメータ
A が B の部分型であるとき，List[A] は List[B] の部分型になる．
(Dog が　Animal の部分型であるなら List[Dog] は　List[Animal] の部分型である)
 */
sealed trait List[+A]

case object Nil extends List[Nothing] // 空のリストを表すデータコンストラクタ
case class Cons[+A](head: A, tail: List[A]) extends List[A] // 空ではないリストを表すデータコンストラクタ

object List {

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  /* 可変相の引数を受け取る
   この関数によって List(1, 2, 3) や，List("A", "BB") などとすることができる．
   */
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  @scala.annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def append[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, y) => y + 1)

  def sum(ints: List[Int]): Int =
    foldRight(ints, 0)((x, y) => x + y)

  def sum2(ints: List[Int]): Int =
    foldLeft(ints, 0)((x, y) => x + y)

  def product(ds: List[Double]): Double =
    foldRight(ds, 1.0)((x, y) => x * y)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](head: A, l: List[A]): List[A] = l match {
    case Nil => Cons(head, Nil)
    case Cons(_, t) => Cons(head, t)
  }

  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  // List[A] で A の型は確定している
  @scala.annotation.tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  def add1(ints: List[Int]): List[Int] =
    foldRight(ints, Nil: List[Int])((h, t) => Cons(h + 1, t))

  def doubleToString(ds: List[Double]): List[String] =
    foldRight(ds, Nil: List[String])((h, t) => Cons(h.toString, t))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(as, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def add(a1: List[Int], a2: List[Int]): List[Int] =
    (a1, a2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, add(t1, t2))
    }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  @scala.annotation.tailrec
  def startWith[A](l: List[A], prefix: List[A]): Boolean =
    (l, prefix) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startWith(t1, t2)
      case _ => false
    }

  @scala.annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Nil => sub == Nil
      case _ if startWith(sup, sub) => true
      case Cons(_, t) => hasSubsequence(t, sub)
    }
}

object Main {
  def main(args: Array[String]): Unit = {
    val ex1: List[Double] = Nil
    val ex2: List[Int] = Cons(1, Nil)
    val ex3: List[String] = Cons("a", Cons("b", Nil))

    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }

    println(x)

    println(List.tail(List(1, 2, 3, 4)))
    println(List.setHead(5, List(1, 2, 3, 4)))
    println(List.drop(List(1, 2, 3, 4), 3))

    val xs: List[Int] = List(1, 2, 3, 4)
    val ex4 = List.dropWhile(xs)(x => x < 4)

    val ex5 = List.filter(xs)(x => x < 3)

    val ex6 = List.hasSubsequence(xs, List(1, 5))
    println(ex6)
  }
}