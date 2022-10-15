package datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def fold[B](f: A => B, g: (B, B) => B): B = this match
    case Leaf(v) => f(v)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))

  def size: Int =
    fold(_ => 1, 1 + _ + _)

  def depth: Int =
    fold(_ => 0, (l, r) => (l max r) + 1)

  def map[B](f: A => B): Tree[B] =
    fold(a => Leaf(f(a)), Branch(_, _))

object Tree:
  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Option[Int] = t match
    case Leaf(i) => if i > 0 then Some(i) else None
    case Branch(l, r) => l.firstPositive orElse r.firstPositive

  extension (t: Tree[Int]) def maximum: Int = t match
    case Leaf(i) => i
    case Branch(l, r) => l.maximum.max(r.maximum)