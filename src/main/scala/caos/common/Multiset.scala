package caos.common

import scala.annotation.targetName

case class Multiset[A](data: Map[A, Int] = Map.empty):
  override def toString: String =
    data.flatMap { case element -> count =>
      List.fill(count)(element.toString)
    }.mkString(", ")
  end toString

  def isEmpty: Boolean =
    data.isEmpty
  end isEmpty

  def contains(element: A): Boolean =
    data.contains(element)
  end contains

  @targetName("add")
  def +(element: A): Multiset[A] =
    val newCount = data.getOrElse(element, 0) + 1
    Multiset(data + (element -> newCount))
  end +

  @targetName("concat")
  def ++(multisetB: Multiset[A]): Multiset[A] =
    Multiset(
      (data.keySet ++ multisetB.data.keySet).map ( element =>
        val countA = data.getOrElse(element, 0)
        val countB = multisetB.data.getOrElse(element, 0)
        element -> (countA + countB)
      ).toMap
    )
  end ++

  @targetName("sub")
  def -(element: A): Multiset[A] =
    data.get(element) match
      case Some(count) if count > 1 =>
        val newCount = count - 1
        Multiset(data + (element -> newCount))
      case _ =>
        Multiset(data - element)
  end -

  @targetName("exclude")
  def --(multisetB: Multiset[A]): Multiset[A] =
    val entriesA = data.map { case elementA -> countA =>
      val newCountA = countA - multisetB.data.getOrElse(elementA, 0)
      elementA -> newCountA
    }.filter { case _ -> count =>
      count > 0
    }
    Multiset(entriesA)
  end --

  def included(multisetB: Multiset[A]): Boolean =
    data.forall { case elementA -> countA =>
      multisetB.data.getOrElse(elementA, 0) >= countA
    }
  end included
end Multiset

object Multiset:
  def apply[A](): Multiset[A] =
    Multiset[A](Map.empty)
  end apply
end Multiset