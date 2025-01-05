package edu.depauw.schanges

trait MIDIPitch extends Pitch:
  def pitch: Int

case class MIDIMelodic(pitch: Int) extends MIDIPitch:
  def sharp: MIDIMelodic = MIDIMelodic(pitch + 1)
  def flat: MIDIMelodic = MIDIMelodic(pitch - 1)

object MIDIMelodic:
  // C(4) is middle C = MIDI note 60
  def C(octave: Int): MIDIMelodic = MIDIMelodic(octave * 12 + 12)
  def D(octave: Int): MIDIMelodic = MIDIMelodic(octave * 12 + 14)
  def E(octave: Int): MIDIMelodic = MIDIMelodic(octave * 12 + 16)
  def F(octave: Int): MIDIMelodic = MIDIMelodic(octave * 12 + 17)
  def G(octave: Int): MIDIMelodic = MIDIMelodic(octave * 12 + 19)
  def A(octave: Int): MIDIMelodic = MIDIMelodic(octave * 12 + 21)
  def B(octave: Int): MIDIMelodic = MIDIMelodic(octave * 12 + 23)

case class MIDIRhythm(pitch: Int) extends MIDIPitch

object MIDIRhythm:
  val LowBass = MIDIRhythm(35)
  val HighBass = MIDIRhythm(36)
  val SideStick = MIDIRhythm(37)
  val Snare = MIDIRhythm(38)
  val HandClap = MIDIRhythm(39)
  val RimShot = MIDIRhythm(40)
  val LowFloorTom = MIDIRhythm(41)
  val ClosedHiHat = MIDIRhythm(42)
  val HighFloorTom = MIDIRhythm(43)
  val PedalHiHat = MIDIRhythm(44)
  val LowTom = MIDIRhythm(45)
  val OpenHiHat = MIDIRhythm(46)
  val LowMidTom = MIDIRhythm(47)
  val HighMidTom = MIDIRhythm(48)
  val Crash = MIDIRhythm(49)
  val HighTom = MIDIRhythm(50)
  val Ride = MIDIRhythm(51)
  val Chinese = MIDIRhythm(52)
  val RideBell = MIDIRhythm(53)
  val Tambourine = MIDIRhythm(54)
  val Splash = MIDIRhythm(55)
  val CowBell = MIDIRhythm(56)

case class MIDIVolume(velocity: Int) extends Volume

object MIDIVolume:
  // These are just rough defaults
  // From https://www.reddit.com/r/AdvancedProduction/comments/wo90h0/midi_values_for_dynamic_levels/
  val fff = MIDIVolume(127)
  val ff = MIDIVolume(112)
  val f = MIDIVolume(96)
  val mf = MIDIVolume(80)
  val mp = MIDIVolume(64)
  val p = MIDIVolume(49)
  val pp = MIDIVolume(33)
  val ppp = MIDIVolume(16)

type MIDIMelNote = Note[MIDIMelodic, MIDIVolume]
type MIDIRhyNote = Note[MIDIRhythm, MIDIVolume]

// Import these to get short names for MIDI Note events
object MIDINote:
  def C(octave: Int): MIDIMelNote = Note(MIDIMelodic.C(octave), MIDIVolume.mf)
  def D(octave: Int): MIDIMelNote = Note(MIDIMelodic.D(octave), MIDIVolume.mf)
  def E(octave: Int): MIDIMelNote = Note(MIDIMelodic.E(octave), MIDIVolume.mf)
  def F(octave: Int): MIDIMelNote = Note(MIDIMelodic.F(octave), MIDIVolume.mf)
  def G(octave: Int): MIDIMelNote = Note(MIDIMelodic.G(octave), MIDIVolume.mf)
  def A(octave: Int): MIDIMelNote = Note(MIDIMelodic.A(octave), MIDIVolume.mf)
  def B(octave: Int): MIDIMelNote = Note(MIDIMelodic.B(octave), MIDIVolume.mf)
  val LowBass = Note(MIDIRhythm.LowBass, MIDIVolume.mf)
  val HighBass = Note(MIDIRhythm.HighBass, MIDIVolume.mf)
  val SideStick = Note(MIDIRhythm.SideStick, MIDIVolume.mf)
  val Snare = Note(MIDIRhythm.Snare, MIDIVolume.mf)
  val HandClap = Note(MIDIRhythm.HandClap, MIDIVolume.mf)
  val RimShot = Note(MIDIRhythm.RimShot, MIDIVolume.mf)
  val LowFloorTom = Note(MIDIRhythm.LowFloorTom, MIDIVolume.mf)
  val ClosedHiHat = Note(MIDIRhythm.ClosedHiHat, MIDIVolume.mf)
  val HighFloorTom = Note(MIDIRhythm.HighFloorTom, MIDIVolume.mf)
  val PedalHiHat = Note(MIDIRhythm.PedalHiHat, MIDIVolume.mf)
  val LowTom = Note(MIDIRhythm.LowTom, MIDIVolume.mf)
  val OpenHiHat = Note(MIDIRhythm.OpenHiHat, MIDIVolume.mf)
  val LowMidTom = Note(MIDIRhythm.LowMidTom, MIDIVolume.mf)
  val HighMidTom = Note(MIDIRhythm.HighMidTom, MIDIVolume.mf)
  val Crash = Note(MIDIRhythm.Crash, MIDIVolume.mf)
  val HighTom = Note(MIDIRhythm.HighTom, MIDIVolume.mf)
  val Ride = Note(MIDIRhythm.Ride, MIDIVolume.mf)
  val Chinese = Note(MIDIRhythm.Chinese, MIDIVolume.mf)
  val RideBell = Note(MIDIRhythm.RideBell, MIDIVolume.mf)
  val Tambourine = Note(MIDIRhythm.Tambourine, MIDIVolume.mf)
  val Splash = Note(MIDIRhythm.Splash, MIDIVolume.mf)
  val CowBell = Note(MIDIRhythm.CowBell, MIDIVolume.mf)

extension [V <: Volume](note: Note[MIDIMelodic, V])
  def sharp: Note[MIDIMelodic, V] = note.copy(pitch = note.pitch.sharp)
  def flat: Note[MIDIMelodic, V] = note.copy(pitch = note.pitch.flat)

extension [P <: Pitch](note: Note[P, MIDIVolume])
  def fff: Note[P, MIDIVolume] = note.copy(volume = MIDIVolume.fff)
  def ff: Note[P, MIDIVolume] = note.copy(volume = MIDIVolume.ff)
  def f: Note[P, MIDIVolume] = note.copy(volume = MIDIVolume.f)
  def mf: Note[P, MIDIVolume] = note.copy(volume = MIDIVolume.mf)
  def mp: Note[P, MIDIVolume] = note.copy(volume = MIDIVolume.mp)
  def p: Note[P, MIDIVolume] = note.copy(volume = MIDIVolume.p)
  def pp: Note[P, MIDIVolume] = note.copy(volume = MIDIVolume.pp)
  def ppp: Note[P, MIDIVolume] = note.copy(volume = MIDIVolume.ppp)

case class MIDIInstrument[P <: MIDIPitch](
  bankMSB: Byte,
  bankLSB: Byte,
  program: Byte
) extends Instrument[P, MIDIVolume]:
  def apply(event: Event[P, MIDIVolume]): InstrumentPart[P, MIDIVolume] =
    InstrumentPart(this, event)

object MIDIInstrument:
  val Piano = MIDIInstrument[MIDIMelodic](0x79, 0, 0)
  val Harpsichord = MIDIInstrument[MIDIMelodic](0x79, 0, 6)
  val Drum = MIDIInstrument[MIDIRhythm](0x78, 0, 0)

object Render:
  import javax.sound.midi.*

  val TICKS_PER_BEAT = 96

  def apply(song: Song): Sequence = {
    val sequence = Sequence(Sequence.PPQ, TICKS_PER_BEAT)
    val control = sequence.createTrack()

    var tick = 1L
    for section <- song.sections do
      tick = processSection(sequence, control, section, tick)

    sequence
  }

  def processSection(sequence: Sequence, control: Track, section: Section, tick: Long): Long = {
    val usecPerBeat = (60_000_000 / section.bpm).toInt
    control.add(MidiEvent(setTempo(usecPerBeat), tick))

    var channel = 0
    for part <- section.parts do {
      val track = sequence.createTrack()
      processPart(track, channel, part, tick)
      channel += 1
    }

    tick + (section.duration.toDouble * TICKS_PER_BEAT).toLong
  }

  def processPart(track: Track, channel: Int, part: Part, tick: Long): Unit = {
    part match
      case InstrumentPart(instrument: MIDIInstrument[_], event) => 
        track.add(MidiEvent(setBankMSB(instrument.bankMSB, channel), tick))
        track.add(MidiEvent(setBankLSB(instrument.bankLSB, channel), tick))
        track.add(MidiEvent(setProgram(instrument.program, channel), tick))
        processEvent(track, channel, event, tick)

      case _ => sys.error("Not a MIDI instrument")
  }

  def processEvent[P <: Pitch, V <: Volume](
    track: Track,
    channel: Int,
    event: Event[P, V],
    tick: Long
  ): Long = {
    event match
      case SeqEvent(events*) =>
        events.foldLeft(tick)((t, e) => processEvent(track, channel, e, t))
      case ParEvent(events*) =>
        events.foldLeft(tick)((t, e) => math.max(t, processEvent(track, channel, e, tick)))
      case EmptyEvent =>
        tick
      case DurationEvent(duration, event) =>
        processEvent(track, channel, event, tick)
        tick + durationToTicks(duration)
      case OffsetEvent(offset, event) =>
        val off = durationToTicks(offset)
        processEvent(track, channel, event, tick + off) - off
      case Rest(duration) =>
        tick + durationToTicks(duration)
      case Note(pitch: MIDIPitch, volume: MIDIVolume, duration) =>
        track.add(MidiEvent(noteOn(pitch, volume, channel), tick))
        val stop = tick + durationToTicks(duration)
        track.add(MidiEvent(noteOff(pitch, volume, channel), stop))
        stop
      case _ =>
        sys.error("Unrecognized event")
  }

  def durationToTicks(duration: Duration): Long =
    (duration.toDouble * TICKS_PER_BEAT).toLong

  def setTempo(usecPerBeat: Int): MidiMessage = {
    val data = Array[Byte](
      (usecPerBeat >> 16 & 0xff).toByte,
      (usecPerBeat >> 8 & 0xff).toByte,
      (usecPerBeat & 0xff).toByte
    )
    MetaMessage(Play.SET_TEMPO, data, 3)
  }

  def setBankMSB(bankMSB: Byte, channel: Int): MidiMessage =
    ShortMessage(ShortMessage.CONTROL_CHANGE, channel, 0, bankMSB)

  def setBankLSB(bankLSB: Byte, channel: Int): MidiMessage =
    ShortMessage(ShortMessage.CONTROL_CHANGE, channel, 32, bankLSB)

  def setProgram(program: Byte, channel: Int): MidiMessage =
    ShortMessage(ShortMessage.PROGRAM_CHANGE, channel, program, 0)

  def noteOn(p: MIDIPitch, v: MIDIVolume, channel: Int): MidiMessage =
    ShortMessage(ShortMessage.NOTE_ON, channel, p.pitch, v.velocity)

  def noteOff(p: MIDIPitch, v: MIDIVolume, channel: Int): MidiMessage =
    ShortMessage(ShortMessage.NOTE_OFF, channel, p.pitch, v.velocity)

object Play:
  import javax.sound.midi.*

  val END_OF_TRACK = 0x2f
  val SET_TEMPO = 0x51

  def apply(sequence: Sequence): Unit = {
    val sequencer = MidiSystem.getSequencer()

    sequencer.open()
    sequencer.setSequence(sequence)

    sequencer.addMetaEventListener { metaMsg =>
      metaMsg.getType() match
        case END_OF_TRACK =>
          if !sequencer.isRunning()
          then
            Thread.sleep(2000) // hack
            sequencer.close()
        case SET_TEMPO =>
          val data = metaMsg.getData()
          val usecPerBeat = (data(0) << 16) + (data(1) << 8) + data(2)
          sequencer.setTempoInMPQ(usecPerBeat)
        case _ => {}
    }

    sequencer.start();
  }
