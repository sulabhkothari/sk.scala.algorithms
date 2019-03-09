package CustomCollections

import scala.collection.mutable.{Builder, MapBuilder}
import scala.collection.generic.CanBuildFrom

import collection._

//Integrate a new kind of map into the collection framework. The idea is to implement a mutable map with String as
// the type of keys by a “Patricia trie”. The term Patricia is in fact an abbreviation for
// “Practical Algorithm to Retrieve Information Coded in Alphanumeric.”
// The idea is to store a set or a map as a tree where subsequent character in a search key determines uniquely a descendant tree.
// For instance a Patricia trie storing the three strings "abc", "abd", "al", "all", "xy"

class PrefixMap[T]
  extends mutable.Map[String, T]
    with mutable.MapLike[String, T, PrefixMap[T]] {
  var suffixes: immutable.Map[Char, PrefixMap[T]] = Map.empty
  var value: Option[T] = None

  def get(s: String): Option[T] =
    if (s.isEmpty) value
    else suffixes get (s(0)) flatMap (_.get(s substring 1))

  def withPrefix(s: String): PrefixMap[T] =
    if (s.isEmpty) this
    else {
      val leading = s(0)
      suffixes get leading match {
        case None =>
          suffixes = suffixes + (leading -> empty)
        case _ =>
      }
      suffixes(leading) withPrefix (s substring 1)
    }

  override def update(s: String, elem: T) =
    withPrefix(s).value = Some(elem)

  override def remove(s: String): Option[T] =
    if (s.isEmpty) {
      val prev = value;
      value = None;
      prev
    }
    else suffixes get (s(0)) flatMap (_.remove(s substring 1))

  def iterator: Iterator[(String, T)] =
    (for (v <- value.iterator) yield ("", v)) ++
      (for ((chr, m) <- suffixes.iterator;
            (s, v) <- m.iterator) yield (chr +: s, v))

  def +=(kv: (String, T)): this.type = {
    update(kv._1, kv._2);
    this
  }

  def -=(s: String): this.type = {
    remove(s);
    this
  }

  override def empty = new PrefixMap[T]
}


object PrefixMap extends {
  def empty[T] = new PrefixMap[T]

  def apply[T](kvs: (String, T)*): PrefixMap[T] = {
    val m: PrefixMap[T] = empty
    for (kv <- kvs) m += kv
    m
  }

  def newBuilder[T]: Builder[(String, T), PrefixMap[T]] =
    new MapBuilder[String, T, PrefixMap[T]](empty)

  implicit def canBuildFrom[T]
  : CanBuildFrom[PrefixMap[_], (String, T), PrefixMap[T]] =
    new CanBuildFrom[PrefixMap[_], (String, T), PrefixMap[T]] {
      def apply(from: PrefixMap[_]) = newBuilder[T]

      def apply() = newBuilder[T]
    }
}

object TestPrefixMap {
  def main(args: Array[String]): Unit = {
    val pm = PrefixMap("hello" -> 5, "hi" -> 2)
    println(pm map { case (k, v) => (k + "!", "x" * v) })
  }
}