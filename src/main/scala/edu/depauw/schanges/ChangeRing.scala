package edu.depauw.schanges

import edu.depauw.schanges.Rest.t

object Bell:
  import MIDINote.*
  
  val names = "1234567890ETABCDFGHJKLMNPQRSUVWYZ"

  val pitches = Seq(
    C(4), D(4), E(4), F(4), G(4), A(4), B(4),
    C(5), D(5), E(5), F(5), G(5), A(5), B(5),
    C(6), D(6), E(6), F(6), G(6), A(6), B(6),
    C(7), D(7), E(7), F(7), G(7), A(7), B(7),
    C(8), D(8), E(8), F(8), G(8)
  )

case class Stage(size: Int):
  def pitches: Seq[MIDIMelNote] = Bell.pitches.take(size).reverse

object Stage:
  val Singles = Stage(3)
  val Minimus = Stage(4)
  val Doubles = Stage(5)
  val Minor = Stage(6)
  val Triples = Stage(7)
  val Major = Stage(8)
  val Caters = Stage(9)
  val Royal = Stage(10)
  val Cinques = Stage(11)
  val Maximus = Stage(12)

case class Row(positions: Int*):
  val stage: Stage = Stage(positions.size)

  def apply(position: Int): Int = positions(
    position
  ) // note: positions go from 0 to stage.size - 1

  def permute[T](items: Seq[T]): Seq[T] = {
    val n = math.min(items.size, stage.size)
    Seq.tabulate(n)(i => items(math.min(this(i), n - 1)))
  }

  override def toString: String =
    (0 until stage.size).map(i => Bell.names(apply(i))).mkString

  def indexOf(bell: Int): Int = positions.indexOf(bell)

object Row:
  def rounds(stage: Stage): Row = Row((0 until stage.size)*)

trait Change:
  def apply(position: Int): Int

  def apply(row: Row): Row = {
    val n = row.stage.size
    Row(Array.tabulate(n) { i =>
      row(math.min(this(i), n - 1))
    }*)
  }

object Change:
  /** Construct a Change where the given places are made. Note that place
    * numbers start at 0. For example, Change(0, 3) is the transposition
    * 02135476....
    *
    * @param places
    * @return
    */
  def apply(places: Int*): Change = {
    val n = places.size
    val last = if n == 0 then 0 else places(n - 1) + 1
    val transposition = Array.fill(last)(0)
    for i <- 0 until last do
      val nextIndex = places.indexWhere(p => p >= i)
      val next = places(nextIndex)
      if i == next then transposition(i) = i
      else if (next - i) % 2 == 0 then transposition(i) = i + 1
      else transposition(i) = math.max(i - 1, 0)
    SeqChange(transposition*)
  }

  // def apply(notation: String): Change = Method(notation).changes(0)

object IdentityChange extends Change:
  def apply(position: Int): Int = position

object CrossChange extends Change:
  def apply(position: Int): Int =
    if position % 2 == 0
    then position + 1
    else position - 1

case class SeqChange(positions: Int*) extends Change:
  def apply(position: Int): Int = {
    val n = positions.size
    if position < n
    then positions(position)
    else if (position - n) % 2 == 0
    then position + 1
    else position - 1
  }

case class Block(first: Row, rows: Seq[Row]):
  def stage: Stage = first.stage

  def last: Row = if rows.isEmpty then first else rows.last

  def toEvent[P <: Pitch, V <: Volume](
      items: Seq[Event[P, V]],
      handGap: Event[P, V]
  ): Event[P, V] = {
    type T = Event[P, V]
    def playRow(row: Row): T = row.permute(items).foldLeft(EmptyEvent: T)(_ - _)
    rows.map(playRow).zipWithIndex.foldLeft(EmptyEvent: T) {
      case (accum, (e, i)) =>
        accum - e - (if i % 2 == 1 then handGap else EmptyEvent)
    }
  }

  def size: Int = rows.size

  def ++(that: Block): Block = Block(first, rows ++ that.rows)

  def isTrue: Boolean = rows.distinct.size == rows.size

  def isCourse: Boolean = first == last

  def observe(bell: Int): Int = rows(rows.size - 3).indexOf(bell - 1) + 1

  def alter(alteration: Composition): Block = {
    alteration(Block(first, rows.dropRight(alteration.size)))
  }

  def calls(plain: Composition, calls: Call*): Block = {
    calls.foldLeft(this) {
      case (block, Call(alt, obs, pos, count)) => plain.call(block, alt, obs, pos, count)
    }
  }

  override def toString: String = rows.mkString("\n")

trait Composition:
  def stage: Stage

  def rounds: Row = Row.rounds(stage)

  def apply(row: Row = rounds): Block

  def apply(block: Block): Block = block ++ apply(block.last)

  def course(row: Row = rounds): Block = {
    var result = apply(row)
    while !result.isCourse do result = apply(result)
    result
  }

  def course(block: Block): Block = {
    var result = apply(block)
    while !result.isCourse do result = apply(result)
    result
  }

  def call(block: Block, alteration: Composition, observed: Int, position: Int, count: Int): Block = {
    var result = apply(block)
    var n = 1
    while result.observe(observed) != position || n != count do
      if result.observe(observed) == position then n += 1
      result = apply(result)
    result.alter(alteration)
  }

  def calls(calls: Call*): Block = Block(rounds, Seq()).calls(this, calls*)

  def callsToRounds(calls: Call*): Block = {
    var result = this.calls(calls*)
    while result.last != rounds do
      result = apply(result)
    result
  }

  def andThen(that: Composition): Composition = SeqComposition(this, that)

  def repeat(times: Int): Composition = SeqComposition(Seq.fill(times)(this)*)

  def +(that: Composition): Composition = this.andThen(that)

  def *(times: Int): Composition = this.repeat(times)

  def size: Int

case class SeqComposition(parts: Composition*) extends Composition:
  def stage: Stage = parts(0).stage

  def apply(row: Row): Block =
    parts.foldLeft(Block(row, Seq())) { case (b, c) =>
      c(b)
    }

  def size: Int = parts.map(_.size).sum

// TODO ParComposition? Add covers?

case class Method(stage: Stage, changes: Change*) extends Composition:
  def apply(row: Row): Block = {
    val rows = changes
      .foldLeft((row, Seq[Row]())) { case ((r, rs), c) =>
        val nr = c(r)
        (nr, rs :+ nr)
      }
      ._2
    Block(row, rows)
  }

  def size: Int = changes.size

object Method:
  def apply(stage: Stage, notation: String): Method = {
    case class State(changes: Seq[Change], places: Seq[Int])

    val State(changes, places) = notation.foldLeft(State(Seq(), Seq())) {
      case (state, c) if Bell.names.contains(c) =>
        state.copy(places = state.places :+ Bell.names.indexOf(c))
      case (state, c) if "Xx-".contains(c) =>
        if state.places.isEmpty
        then State(state.changes :+ CrossChange, Seq())
        else State(state.changes :+ Change(state.places*) :+ CrossChange, Seq())
      case (state, '.') =>
        State(state.changes :+ Change(state.places*), Seq())
      case (state, ',') =>
        val front =
          if state.places.isEmpty
          then state.changes
          else state.changes :+ Change(state.places*)
        State(front ++ front.reverse.tail, Seq())
      case (state, c) if " \t\n\r".contains(c) =>
        state
    }

    Method(stage, (if places.isEmpty then changes else changes :+ Change(places*))*)
  }

case class Call(alteration: Composition, observed: Int, position: Int, count: Int = 1)
