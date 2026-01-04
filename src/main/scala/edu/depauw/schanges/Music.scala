package edu.depauw.schanges

case class Song(title: String, sections: Section*):
  def duration: Duration = sections.map(_.duration).fold(Duration.zero)(_ + _)
  def >(section: Section): Song = Song(title, sections :+ section*)

case class Section(bpm: Double, parts: Part*):
  def duration: Duration = parts.map(_.duration).fold(Duration.zero)(_ max _)
  def +(part: Part): Section = Section(bpm, parts :+ part*)

trait Part:
  def duration: Duration

trait Pitch

trait Volume

trait Instrument[P <: Pitch, V <: Volume]

case class InstrumentPart[P <: Pitch, V <: Volume](
    instrument: Instrument[P, V],
    event: Event[P, V]
) extends Part:
  def duration: Duration = event.duration

opaque type Duration = Double

object Duration:
  def apply(beats: Double) = beats
  val zero: Duration = Duration(0)
  val whole: Duration = Duration(4)
  val half: Duration = Duration(2)
  val quarter: Duration = Duration(1)
  val eighth: Duration = Duration(0.5)
  val sixteenth: Duration = Duration(0.25)
  val thirtysecond: Duration = Duration(0.125)

extension (d: Duration)
  def toDouble: Double = d
  def +(other: Duration): Duration = Duration(d + other)
  infix def max(other: Duration): Duration = Duration(Math.max(d, other))
  def *(factor: Double): Duration = Duration(d * factor)
  def dot: Duration = Duration(d * 1.5)
  def dotdot: Duration = Duration(d * 1.75)

extension (x: Double) def beats: Duration = Duration(x)

trait Event[+P <: Pitch, +V <: Volume]:
  def duration: Duration
  def -[Q >: P <: Pitch, F >: V <: Volume](
      that: Event[Q, F]
  ): Event[Q, F] = SeqEvent(this, that)

  def |[Q >: P <: Pitch, F >: V <: Volume](
      that: Event[Q, F]
  ): Event[Q, F] = this - that

  def /[Q >: P <: Pitch, F >: V <: Volume](
      that: Event[Q, F]
  ): Event[Q, F] = ParEvent(this, that)

  def *(repetitions: Int): Event[P, V] = {
    repetitions match
      case 0 => EmptyEvent
      case 1 => this
      case n if n % 2 == 0 =>
        val half = this * (n / 2)
        half - half
      case _ => this * (repetitions - 1) - this
  }

extension (repetitions: Int)
  def *[P <: Pitch, V <: Volume](
      event: Event[P, V]
  ): Event[P, V] = event * repetitions

case class SeqEvent[P <: Pitch, V <: Volume](
    events: Event[P, V]*
) extends Event[P, V]:
  def duration: Duration =
    events.map(_.duration).fold(Duration.zero)(_ + _)

  override def -[Q >: P <: Pitch, F >: V <: Volume](
      that: Event[Q, F]
  ): Event[Q, F] = {
    that match
      case SeqEvent(events*) => SeqEvent(this.events :++ events*)
      case _                 => SeqEvent(this.events :+ that*)
  }

case object EmptyEvent extends Event[Nothing, Nothing]:
  def duration: Duration = 0.beats

case class ParEvent[P <: Pitch, V <: Volume](
    events: Event[P, V]*
) extends Event[P, V]:
  def duration: Duration =
    events.map(_.duration).fold(Duration.zero)(_ max _)

  override def /[Q >: P <: Pitch, F >: V <: Volume](
      that: Event[Q, F]
  ): Event[Q, F] = {
    that match
      case ParEvent(events*) => ParEvent(this.events :++ events*)
      case _                 => ParEvent(this.events :+ that*)
  }

case class DurationEvent[P <: Pitch, V <: Volume](
    duration: Duration,
    event: Event[P, V]
) extends Event[P, V]

case class OffsetEvent[P <: Pitch, V <: Volume](
    offset: Duration,
    event: Event[P, V]
) extends Event[P, V]:
  def duration: Duration = event.duration

case class Note[P <: Pitch, V <: Volume](
    pitch: P,
    volume: V,
    duration: Duration = Duration.quarter
) extends Event[P, V]:
  def w: Note[P, V] = copy(duration = Duration.whole)
  def h: Note[P, V] = copy(duration = Duration.half)
  def q: Note[P, V] = copy(duration = Duration.quarter)
  def e: Note[P, V] = copy(duration = Duration.eighth)
  def s: Note[P, V] = copy(duration = Duration.sixteenth)
  def t: Note[P, V] = copy(duration = Duration.thirtysecond)
  def dot: Note[P, V] = copy(duration = duration.dot)
  def dotdot: Note[P, V] = copy(duration = duration.dotdot)

case class Rest(duration: Duration) extends Event[Nothing, Nothing]:
  def dot: Rest = Rest(duration.dot)
  def dotdot: Rest = Rest(duration.dotdot)

object Rest:
  def w: Rest = Rest(Duration.whole)
  def h: Rest = Rest(Duration.half)
  def q: Rest = Rest(Duration.quarter)
  def e: Rest = Rest(Duration.eighth)
  def s: Rest = Rest(Duration.sixteenth)
  def t: Rest = Rest(Duration.thirtysecond)

@main def musicTest(): Unit = {
  import MIDINote.*
  import MIDIInstrument.*

  val demo1 = Song("Example")
    > Section(60)
    + Piano {
      Rest.q - C(4) * 3 |
        G(4).h.mp - F(4).e.mf - E(4).flat.e.f - D(4).e.ff - C(4).e.fff
    }
    + Drum {
      (LowBass.f - PedalHiHat.e.mf - Splash.e.mp - HighMidTom.f - Rest.q) * 2
    }
    > Section(120)
    + Piano {
      C(5).e - D(5).e - E(5).e - F(5).e - G(5).e - A(5).e - B(5).e - C(6).e
    }
    + Drum {
      HighTom - Crash - LowTom - Crash
    }
    > Section(60)
    + Harpsichord { C(3) * 4 }

  val sequence1 = Render(demo1)
  // Play(sequence1)

  type E = Event[MIDIMelodic, MIDIVolume]
  type N = Note[MIDIMelodic, MIDIVolume]
  def measure(a: N, b: N, c: N, d: N, e: N): E = {
    val half = a.h / (Rest.s - b.q.dotdot) / (Rest.e - c.s - d.s - e.s - c.s - d.s - e.s)
    half - half
  }

  val demo2 = Song("Prelude")
    > Section(112)
    + Piano {
      measure(C(4).p, E(4).p, G(4).p, C(5).p, E(5).p) |
      measure(C(4).p, D(4).p, A(4).p, D(5).p, F(5).p) |
      measure(B(3).mp, D(4).mp, G(4).mp, D(5).mp, F(5).mp) |
      measure(C(4).mf, E(4).mf, G(4).mf, C(5).mf, E(5).mf) |
      measure(C(4).f, E(4).f, A(4).f, E(5).f, A(5).f) |
      measure(C(4).p, D(4).p, F(4).sharp.p, A(4).p, D(5).p) |
      measure(B(3).f, D(4).f, G(4).f, D(5).f, G(5).f) |
      measure(B(3).p, C(4).p, E(4).p, G(4).p, C(5).p) |
      measure(A(3).pp, C(4).pp, E(4).pp, G(4).pp, C(5).pp) |
      measure(D(3).pp, A(3).pp, D(4).pp.pp, F(4).sharp.pp, C(5).pp) |
      measure(G(3).p, B(3).p, D(4).p, G(4).p, B(4).p) |
      measure(G(3).mp, B(3).flat.mp, E(4).mp, G(4).mp, B(4).flat.mp)
    }
  // TODO make it easier to apply dynamics to an event...

  val sequence2 = Render(demo2)
  Play(sequence2)
}

/* TODO
 * - More instruments
 */
