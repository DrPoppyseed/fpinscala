package datastructures

import scala.annotation.tailrec

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  def sum(ints: List[Int]): Int = ints match
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)

  def product(ds: List[Double]): Double = ds match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)

  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail *))

  def tail[A](as: List[A]): List[A] = as match
    case Nil => Nil
    case Cons(x, xs) => xs

  def setHead[A](as: List[A], h: A): List[A] = as match
    case Nil => Nil
    case Cons(_, xs) => Cons(h, xs)

  @tailrec
  def drop[A](as: List[A], n: Int): List[A] = as match
    case Nil => Nil
    case Cons(_, xs) => drop(tail(xs), n - 1)

  @tailrec
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match
    case Nil => Nil
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => as

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))

  def init[A](as: List[A]): List[A] = as match
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, xs) => Cons(h, init(xs))

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, _ + _)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0, (_, acc) => acc + 1)

  @tailrec
  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B = as match
    case Nil => acc
    case Cons(h, t) => foldLeft(t, f(acc, h), f)

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ds: List[Double]): Double =
    foldLeft(ds, 1.0, _ * _)

  def lenViaFoldLeft[A](as: List[A]): Int =
    foldLeft(as, 0, (acc, _) => acc + 1)

  def reverseViaFoldLeft[A](as: List[A]): List[A] =
    foldLeft(as, List[A](), (acc, h) => Cons(h, acc))

  def foldRightViaFoldLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverseViaFoldLeft(as), acc, (a, b) => f(b, a))

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverseViaFoldLeft(a1), a2, (acc, h) => Cons(h, acc))

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2, Cons(_, _))

  def concat[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A], appendViaFoldRight)

  def mapAddOne(l: List[Int]): List[Int] =
    foldRightViaFoldLeft(l, Nil: List[Int], (h, t) => Cons(h + 1, t))

  def mapParseDoubleToString(l: List[Double]): List[String] =
    foldRightViaFoldLeft(l, Nil: List[String], (h, t) => Cons(h.toString, t))

  def map[A, B](as: List[A], f: A => B): List[B] =
    foldRightViaFoldLeft(as, Nil: List[B], (h, t) => Cons(f(h), t))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRightViaFoldLeft(as, Nil: List[A], (h, t) => if (f(h)) Cons(h, t) else t)

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    foldLeft(as, Nil: List[B], (t, h) => append(t, f(h)))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, h => if (f(h)) List(h) else List())

  def zipInts(l: List[Int], r: List[Int]): List[Int] = (l, r) match
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(hl, tl), Cons(hr, tr)) => Cons(hl + hr, zipInts(tl, tr))

  def zipWith[A, B, C](l: List[A], r: List[B], f: (A, B) => C): List[C] = (l, r) match
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(hl, tl), Cons(hr, tr)) => Cons(f(hl, hr), zipWith(tl, tr, f))

//  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
//