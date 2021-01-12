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
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](head: A, l: List[A]): List[A] = l match {
    case Nil => Cons(head, Nil)
    case Cons(_, t) => Cons(head, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

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
}

object Main {
  def main(args: Array[String]): Unit = {
    val ex1: List[Double] = Nil
    val ex2: List[Int] = Cons(1, Nil)
    val ex3: List[String] = Cons("a", Cons("b", Nil))

    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x+y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }

    println(x)

    println(List.tail(List(1, 2, 3, 4)))
    println(List.setHead(5, List(1, 2, 3, 4)))
    println(List.drop(List(1, 2, 3, 4), 3))
  }
}