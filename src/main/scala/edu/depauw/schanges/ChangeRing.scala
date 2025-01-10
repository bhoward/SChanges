package edu.depauw.schanges

case class Stage(size: Int)

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

val Bells = "1234567890ETABCDFGHJKLMNPQRSUVWYZ"

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
    (0 until stage.size).map(i => Bells(apply(i))).mkString

  def indexOf(bell: Int): Int = positions.indexOf(bell)

  def call(plain: Composition, alteration: Composition, observed: Int, position: Int, count: Int): Block =
    plain.call(this, alteration, observed, position, count)

  def calls(calls: Call*): Block = Block(this, Seq()).calls(calls*)

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

  def apply(notation: String): Change = Method(notation).changes(0)

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

  def call(plain: Composition, alteration: Composition, observed: Int, position: Int, count: Int): Block =
    plain.call(this, alteration, observed, position, count)

  def calls(calls: Call*): Block = {
    calls.foldLeft(this) {
      case (block, Call(plain, alt, obs, pos, count)) => block.call(plain, alt, obs, pos, count)
    }
  }

  override def toString: String = rows.mkString("\n")

trait Composition:
  def apply(row: Row): Block

  def apply(block: Block): Block = block ++ apply(block.last)

  def course(row: Row): Block = {
    var result = apply(row)
    while !result.isCourse do result = apply(result)
    result
  }

  def course(block: Block): Block = {
    var result = apply(block)
    while !result.isCourse do result = apply(result)
    result
  }

  def call(row: Row, alteration: Composition, observed: Int, position: Int, count: Int): Block = {
    var result = apply(row)
    var n = 1
    while result.observe(observed) != position || n != count do
      if result.observe(observed) == position then n += 1
      result = apply(result)
    result.alter(alteration)
  }

  def call(block: Block, alteration: Composition, observed: Int, position: Int, count: Int): Block = {
    var result = apply(block)
    var n = 1
    while result.observe(observed) != position || n != count do
      if result.observe(observed) == position then n += 1
      result = apply(result)
    result.alter(alteration)
  }

  def andThen(that: Composition): Composition = SeqComposition(this, that)

  def repeat(times: Int): Composition = SeqComposition(Seq.fill(times)(this)*)

  def +(that: Composition): Composition = this.andThen(that)

  def *(times: Int): Composition = this.repeat(times)

  def size: Int

case class SeqComposition(parts: Composition*) extends Composition:
  def apply(row: Row): Block = parts.foldLeft(Block(row, Seq())) { case (b, c) =>
    c(b)
  }

  def size: Int = parts.map(_.size).sum

// TODO ParComposition? Add covers?

case class Method(changes: Change*) extends Composition:
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
  def apply(notation: String): Method = {
    case class State(changes: Seq[Change], places: Seq[Int])

    val State(changes, places) = notation.foldLeft(State(Seq(), Seq())) {
      case (state, c) if Bells.contains(c) =>
        state.copy(places = state.places :+ Bells.indexOf(c))
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

    Method((if places.isEmpty then changes else changes :+ Change(places*))*)
  }

case class Call(plain: Composition, alteration: Composition, observed: Int, position: Int, count: Int = 1)

object MajorDemos:
  import MIDINote.*
  import MIDIInstrument.*

  val bells = Array(C(5), B(4), A(4), G(4), F(4), E(4), D(4), C(4)).map(_.e)

  def blockToMidi(block: Block, secondsPerRow: Double = 2.0) = {
    val event = block.toEvent(bells, Rest.e)
    val song = Song("", Section(60 / secondsPerRow * bells.size / 2, ChurchBell(Rest.q - event)))
    Render(song)
  }

  val roundsRow = Row.rounds(Stage(bells.size))
  val rounds = Method(IdentityChange, IdentityChange)(roundsRow)

  @main def plainHunt(): Unit = {
    val method = Method("x1x1x1x1,1")
    val course = method(roundsRow)
    Play(blockToMidi(rounds ++ course ++ rounds))
  }

  @main def plainBob(): Unit = {
    val plain = Method("x1x1x1x1,2")

    // val course = (plain * 7)(roundsRow)
    val course = plain.course(roundsRow)
    println(course.isTrue)
    println(course.size)

    Play(blockToMidi(rounds ++ course ++ rounds))
  }

  @main def sampleComposition(): Unit = {
    /* From https://complib.org/composition/40519
    5040 Plain Bob Major
    Composed by Cornelius Charge
    23456	W	M	H
    34256			2
    46235	s	–	2
    34265	–		3
    63245	–		3
    43265	s		3
    23645	2		–
    3 part.
     */
    val plain = Method("x1x1x1x1,2")
    val bob = Method("x1x1x1x1,4")
    val single = Method("x1x1x1x1,234")

    val c1 = (plain * 6 + bob) * 2
    val c2 = single + plain * 4 + bob + bob
      + plain * 6 + bob
    val c3 = (bob + plain * 5 + bob + c1) * 2
    val c4 = single + plain * 5 + bob + c1
    val c5 = bob + plain * 6
      + bob + plain * 5 + bob

    val course = (c1 + c2 + c3 + c4 + c5).course(roundsRow)
    println(course.isTrue)
    println(course.size)

    Play(blockToMidi(rounds ++ course ++ rounds))
  }

  @main def sampleComposition2(): Unit = {
    /* From https://complib.org/composition/40519
    5040 Plain Bob Major
    Composed by Cornelius Charge
    23456	W	M	H
    34256			2
    46235	s	–	2
    34265	–		3
    63245	–		3
    43265	s		3
    23645	2		–
    3 part.
     */
    val plain = Method("x1x1x1x1,2")
    val bob = Method("4")
    val single = Method("234")

    val obs = 8
    val middle = 6
    val wrong = 7
    val home = 8

    val bobWrong = Call(plain, bob, obs, wrong)
    val bobMiddle = Call(plain, bob, obs, middle)
    val bobHome = Call(plain, bob, obs, home)
    val singleWrong = Call(plain, single, obs, wrong)

    val course = roundsRow.calls(
      bobHome, bobHome,
      singleWrong, bobMiddle, bobHome, bobHome,
      bobWrong, bobHome, bobHome, bobHome,
      bobWrong, bobHome, bobHome, bobHome,
      singleWrong, bobHome, bobHome, bobHome,
      bobWrong, bobWrong, bobHome,

      bobHome, bobHome,
      singleWrong, bobMiddle, bobHome, bobHome,
      bobWrong, bobHome, bobHome, bobHome,
      bobWrong, bobHome, bobHome, bobHome,
      singleWrong, bobHome, bobHome, bobHome,
      bobWrong, bobWrong, bobHome,

      bobHome, bobHome,
      singleWrong, bobMiddle, bobHome, bobHome,
      bobWrong, bobHome, bobHome, bobHome,
      bobWrong, bobHome, bobHome, bobHome,
      singleWrong, bobHome, bobHome, bobHome,
      bobWrong, bobWrong, bobHome
    )

    println(course.isTrue)
    println(course.size)

    Play(blockToMidi(rounds ++ course ++ rounds))
  }

  @main def nineTailorsPart1(): Unit = {
    /*
    A SHORT TOUCH OF KENT TREBLE BOB MAJOR
    (Two courses)

    704
    By the course ends
    64352
    23456
    8th the Observation

    Call her in the middle with a double, before,
    wrong and home.
    Repeated once.
    (TROYTE)
     */
    val plain = Method("34x34.18x12x18x12x18x12x18,18")
    val bob = Method("14")
    val single = Method("1234")

    val obs = 8
    val before = 1
    val middle = 6
    val wrong = 7
    val home = 8

    val bobMiddle = Call(plain, bob, obs, middle)
    val bobBefore = Call(plain, bob, obs, before)
    val bobWrong = Call(plain, bob, obs, wrong)
    val bobHome = Call(plain, bob, obs, home)

    val course = roundsRow.calls(
      bobMiddle, bobMiddle, bobBefore, bobWrong, bobHome,
      bobMiddle, bobMiddle, bobBefore, bobWrong, bobHome
    )

    println(course.isTrue)
    println(course.isCourse)
    println(course.size)

    Play(blockToMidi(rounds ++ course ++ rounds))
  }

  @main def nineTailorsPart2(): Unit = {
    /*
    A FULL PEAL OF GRANDSIRE TRIPLES (HOLT'S TEN-PART PEAL)

    5040
    By the Part Ends
    First Half  Second Half
    246375      257364
    267453      276543
    275634      264735
    253746      243657
    235476      234567
    2nd the Observation

    Call her:
      1st Half) Out of the hunt, middle, in and out at 5, right,
      middle, wrong, right, middle and into the hunt (4 times repeated).
      2nd Half) Out of the hunt, wrong, right, middle, wrong, right,
      in and out at 5, wrong and into the hunt (4 times repeated).
    The last call in each half is a single; Holt's Single must be used
    in ringing this peal.

    See also https://complib.org/composition/29040
    */
    val plain = Method("3.1.7.1.7.1.7.1.7.1.7.1.7.1") // 3,1.7.1.7.1.7.1
    val bob = Method("3.1") // need to replace last two changes
    val single = Method("3.14567")

    val obs = 2
    val in = 3
    val out = 4
    val middle = 5
    val wrong = 6
    val right = 7

    val bobIn = Call(plain, bob, obs, in)
    val bobOut = Call(plain, bob, obs, out)
    val bobOutAt5 = Call(plain, bob, obs, out, 5)
    val bobMiddle = Call(plain, bob, obs, middle)
    val bobWrong = Call(plain, bob, obs, wrong)
    val bobRight = Call(plain, bob, obs, right)
    val singleIn = Call(plain, single, obs, in)

    val course = roundsRow.calls(
      bobOut, bobMiddle, bobIn, bobOutAt5, bobRight, bobMiddle, bobWrong, bobRight, bobMiddle, bobIn,
      bobOut, bobMiddle, bobIn, bobOutAt5, bobRight, bobMiddle, bobWrong, bobRight, bobMiddle, bobIn,
      bobOut, bobMiddle, bobIn, bobOutAt5, bobRight, bobMiddle, bobWrong, bobRight, bobMiddle, bobIn,
      bobOut, bobMiddle, bobIn, bobOutAt5, bobRight, bobMiddle, bobWrong, bobRight, bobMiddle, bobIn,
      bobOut, bobMiddle, bobIn, bobOutAt5, bobRight, bobMiddle, bobWrong, bobRight, bobMiddle, singleIn,
      bobOut, bobWrong, bobRight, bobMiddle, bobWrong, bobRight, bobIn, bobOutAt5, bobWrong, bobIn,
      bobOut, bobWrong, bobRight, bobMiddle, bobWrong, bobRight, bobIn, bobOutAt5, bobWrong, bobIn,
      bobOut, bobWrong, bobRight, bobMiddle, bobWrong, bobRight, bobIn, bobOutAt5, bobWrong, bobIn,
      bobOut, bobWrong, bobRight, bobMiddle, bobWrong, bobRight, bobIn, bobOutAt5, bobWrong, bobIn,
      bobOut, bobWrong, bobRight, bobMiddle, bobWrong, bobRight, bobIn, bobOutAt5, bobWrong, singleIn
    )

    println(course.isTrue)
    println(course.isCourse)
    println(course.size)

    Play(blockToMidi(rounds ++ course ++ rounds))
  }

  @main def nineTailorsPart3(): Unit = {
    /*
    A SHORT TOUCH OF STEDMAN'S TRIPLES (Five Parts)

    840
    By the Part Ends
    5 6 1 2 3 4
    3 4 1 5 6 2
    6 2 1 3 4 5
    4 5 1 6 2 3
    2 3 1 4 5 6
    Treble the observation.

    Call her the last whole turn, out quick, in slow, the second half turn and out slow.
    Four times repeated.
    (Troyte.)
    */
    val plain = Method("3.1.7.3.1.3,1")
    val oddBob = Method("5.3.1.3.1.3.7.1.3.1")
    val evenBob = Method("5.1.3.1")

    val obs = 1
    val last = 5
    val outQ = 7
    val inS = 1
    val second = 3
    val outS = 5

    val bobLast = Call(plain, oddBob, obs, last)
    val bobOutQ = Call(plain, oddBob, obs, outQ)
    val bobInS = Call(plain, oddBob, obs, inS)
    val bobSecond = Call(plain, evenBob, obs, second)
    val bobOutS = Call(plain, evenBob, obs, outS)
    val toRounds = Call(plain, plain, obs, 2)

    val course = roundsRow.calls(
      bobLast, bobOutQ, bobInS, bobSecond, bobOutS,
      bobLast, bobOutQ, bobInS, bobSecond, bobOutS,
      bobLast, bobOutQ, bobInS, bobSecond, bobOutS,
      bobLast, bobOutQ, bobInS, bobSecond, bobOutS,
      bobLast, bobOutQ, bobInS, bobSecond, bobOutS,
      toRounds
    )

    // val bob = Method("3.1.5.3.1.3.1.3.7.1.3.1")
    // val bob2 = Method("3.1.7.3.1.3.1.3.5.1.3.1")
    //
    // val course = ((plain * 4 + bob
    //   + plain * 2 + bob
    //   + plain + bob
    //   + bob2
    //   + bob2
    //   + plain * 2) * 5)(roundsRow)

    println(course.isTrue)
    println(course.isCourse)
    println(course.size)

    Play(blockToMidi(rounds ++ course ++ rounds))
  }
