package u04lab.polyglot.a01b
import u04lab.code.List.length

import scala.jdk.javaapi.OptionConverters
import u04lab.polyglot.{OptionToOptional, Pair}
import u04lab.code.Option
import u04lab.code.Option.*
import u04lab.code.List.*
import math.*
import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:
  private val seed = 42
  private val random = new Random(seed)
  private var minesSet = Cons(Pair(random.nextInt(size), random.nextInt(size)), Nil())
  private var selected = Nil[Pair[Int, Int]]()
  putMines()

  private def putMines(): Unit =
    while (length(minesSet) != mines) {
      minesSet = append(minesSet, Cons(Pair(random.nextInt(size), random.nextInt(size)), Nil()))
    }

  private def neighbours(x: Int, y: Int): Int =
    length(filter(minesSet)(l => abs(l.getX - x) <= 1 & abs(l.getY - y) <= 1))

  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    if (contains(minesSet, Pair(x,y)))
      return OptionToOptional(Option.None())
    else
      selected = append(selected, Cons(Pair(x,y), Nil()) )
    OptionToOptional(Some(neighbours(x,y))) // Option => Optional converter

  def won =
    (length(selected) + length(minesSet)).equals(size * size)
