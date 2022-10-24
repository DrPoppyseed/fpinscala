package errorhandling

import scala.{Option as _, Some as _, None as _}

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = ???

  def flatMap[B](f: A => Option[B]): Option[B] = ???

  def getOrElse[B >: A](default: () => B): B = ???

  def orElse[B >: A](ob: => Option[B]): Option[B] = ???

  def filter(f: A => Boolean): Option[A] = ???
