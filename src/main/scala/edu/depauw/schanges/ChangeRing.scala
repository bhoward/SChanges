package music

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

  def apply(position: Int): Int = positions(position) // note: positions go from 0 to stage.size - 1

  def permute[T](items: Seq[T]): Seq[T] = {
    val n = math.min(items.size, stage.size)
    Seq.tabulate(n)(i => items(math.min(this(i), n - 1)))
  }

  override def toString: String =
    (0 until stage.size).map(i => Bells(apply(i))).mkString

  def indexOf(bell: Int): Int = positions.indexOf(bell)

object Row:
  def rounds(stage: Stage): Row = Row((0 until stage.size)*)

trait Change:
  def apply(position: Int): Int

  def apply(row: Row): Row = {
    val n = row.stage.size
    Row(Array.tabulate(n) {
      i => row(math.min(this(i), n - 1))
    }*)
  }

object Change:
  /**
    * Construct a Change where the given places are made.
    * Note that place numbers start at 0. For example,
    * Change(0, 3) is the transposition 02135476....
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
      if i == next then
        transposition(i) = i
      else
        if (next - i) % 2 == 0 then
          transposition(i) = i + 1
        else
          transposition(i) = math.max(i - 1, 0)
    SeqChange(transposition*)
  }

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

case class Block(first: Row, rows: Row*):
  def last: Row = if rows.isEmpty then first else rows.last

  def toEvent[P <: Pitch, V <: Volume](items: Seq[Event[P, V]], handGap: Event[P, V]): Event[P, V] = {
    type T = Event[P, V]
    def playRow(row: Row): T = row.permute(items).foldLeft(EmptyEvent: T)(_ - _)
    rows.map(playRow).zipWithIndex.foldLeft(EmptyEvent: T) {
      case (accum, (e, i)) => accum - e - (if i % 2 == 1 then handGap else EmptyEvent)
    }
  }

  def size: Int = rows.size

  def ++(that: Block): Block = Block(first, (rows ++ that.rows)*)

  def isTrue: Boolean = rows.distinct.size == rows.size

  def isCourse: Boolean = first == last

  def observe(bell: Int): Int = rows(rows.size - 3).indexOf(bell)

trait Composition:
  def apply(row: Row): Block

  def apply(block: Block): Block = block ++ apply(block.last)

  def course(row: Row): Block = {
    var result = apply(row)
    while !result.isCourse do
      result = apply(result)
    result
  }

  def andThen(that: Composition): Composition = SeqComposition(this, that)

  def repeat(times: Int): Composition = SeqComposition(Seq.fill(times)(this)*)

  def +(that: Composition): Composition = this.andThen(that)

  def *(times: Int): Composition = this.repeat(times)

case class SeqComposition(parts: Composition*) extends Composition:
  def apply(row: Row): Block = parts.foldLeft(Block(row)) {
    case (b, c) => c(b)
  }

// TODO ParComposition? Add covers?

case class Method(changes: Change*) extends Composition:
  def apply(row: Row): Block = {
    val rows = changes.foldLeft((row, Seq[Row]())) {
      case ((r, rs), c) =>
        val nr = c(r)
        (nr, rs :+ nr)
    }._2
    Block(row, rows*)
  }

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
        val front = if state.places.isEmpty
            then state.changes
            else state.changes :+ Change(state.places*)
        State(front ++ front.reverse.tail, Seq())
      case (state, c) if " \t\n\r".contains(c) =>
        state
    }

    Method((if places.isEmpty then changes else changes :+ Change(places*))*)
  }

// TODO: Composition; Calls?

object ChangeRing:
  @main def changeRinging(): Unit = {
    import MIDINote.*
    import MIDIInstrument.*

    val bells = Array(C(5), B(4), A(4), G(4), F(4), E(4), D(4), C(4)).map(_.e)
    val roundsRow = Row.rounds(Stage(bells.size))

    def blockToEvent(block: Block) = block.toEvent(bells, Rest.e)

    val rounds = blockToEvent(Method(IdentityChange, IdentityChange)(roundsRow))

    val phMethod = Method("x1x1x1x1,1")
    
    val phEvent = blockToEvent(phMethod(roundsRow))
    val plainHunt = rounds | phEvent | rounds
    
    val phSong = Song("Plain Hunt", Section(120, Piano(Rest.q - plainHunt)))

    val plain = Method("x1x1x1x1,2")
    val bob = Method("x1x1x1x1,4")
    val single = Method("x1x1x1x1,234")

    // val pbCourse = (plain * 7)(roundsRow)
    val pbCourse = plain.course(roundsRow)
    println(pbCourse.isTrue)
    println(pbCourse.size)
    val pbEvent = blockToEvent(pbCourse)
    val plainBob = rounds | pbEvent | rounds

    val pbSong = Song("Plain Bob", Section(120, Piano(Rest.q - plainBob)))

    Play(Render(phSong))

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
    val c1 = (plain * 6 + bob) * 2
    val c2 = single + plain * 4 + bob + bob
        + plain * 6 + bob
    val c3 = (bob + plain * 5 + bob + c1) * 2
    val c4 = single + plain * 5 + bob + c1
    val c5 = bob + plain * 6
        + bob + plain * 5 + bob

    val charge = (c1 + c2 + c3 + c4 + c5).course
    val chargeCourse = charge(roundsRow)
    println(chargeCourse.isTrue)
    println(chargeCourse.size)
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
    val bob = Method("34x34.18x12x18x12x18x12x18,14")
    val single = Method("34x34.18x12x18x12x18x12x18,1234")

    val roundsRow = Row.rounds(Stage.Major)
    
    val call1 = plain + bob
    val call2 = call1 + bob
    val call3 = call2 + plain * 2 + bob
    val call4 = call3 + plain * 2 + bob
    val call5 = call4 + plain + bob

    val touch = call5.course(roundsRow)
    println(touch.isTrue)
    println(touch.isCourse)
    println(touch.size)
  }