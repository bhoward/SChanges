package edu.depauw.schanges

object CRDemos:
  def blockToMidi(block: Block, addRounds: Boolean = true, secondsPerRow: Double = 2.0) = {
    val stage = block.stage
    val fullBlock = if addRounds then
      val rounds = Method(stage, IdentityChange, IdentityChange)()
      rounds ++ block ++ rounds
    else
      block

    val event = fullBlock.toEvent(stage.pitches, Rest.q)
    val song = Song("",
      Section(60 / secondsPerRow * stage.size,
      MIDIInstrument.ChurchBell(Rest.q - event)))
    Render(song)
  }

  @main def plainHuntSingles(): Unit = {
    val method = Method(Stage.Singles, "x1x,1")
    val course = method()
    Play(blockToMidi(course, secondsPerRow = 1.5))
  }

  @main def plainHuntMinimus(): Unit = {
    val method = Method(Stage.Minimus, "x1x1,1")
    val course = method()
    Play(blockToMidi(course, secondsPerRow = 1.5))
  }

  @main def plainHuntDoubles(): Unit = {
    val method = Method(Stage.Doubles, "x1x1x,1")
    val course = method()
    Play(blockToMidi(course, secondsPerRow = 1.5))
  }

  @main def plainHuntMinor(): Unit = {
    val method = Method(Stage.Minor, "x1x1x1,1")
    val course = method()
    Play(blockToMidi(course))
  }

  @main def plainHuntTriples(): Unit = {
    val method = Method(Stage.Triples, "x1x1x1x,1")
    val course = method()
    Play(blockToMidi(course))
  }

  @main def plainHuntMajor(): Unit = {
    val method = Method(Stage.Major, "x1x1x1x1,1")
    val course = method()
    Play(blockToMidi(course))
  }

  @main def plainHuntCaters(): Unit = {
    val method = Method(Stage.Caters, "x1x1x1x1x,1")
    val course = method()
    Play(blockToMidi(course))
  }

  @main def plainHuntRoyal(): Unit = {
    val method = Method(Stage.Royal, "x1x1x1x1x1,1")
    val course = method()
    Play(blockToMidi(course))
  }

  @main def plainHuntCinques(): Unit = {
    val method = Method(Stage.Cinques, "x1x1x1x1x1x,1")
    val course = method()
    Play(blockToMidi(course, secondsPerRow = 2.5))
  }

  @main def plainHuntMaximus(): Unit = {
    val method = Method(Stage.Maximus, "x1x1x1x1x1x1,1")
    val course = method()
    Play(blockToMidi(course, secondsPerRow = 3))
  }

  @main def plainHunt24(): Unit = {
    val method = Method(Stage(24), "x1x1x1x1x1x1x1x1x1x1x1x1,1")
    val course = method()
    Play(blockToMidi(course, secondsPerRow = 4.5))
  }

  @main def plainBob(): Unit = {
    val plain = Method(Stage.Major, "x1x1x1x1,2")

    // val course = (plain * 7)()
    val course = plain.course()
    println(course.isTrue)
    println(course.size)

    Play(blockToMidi(course))
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
    val plain = Method(Stage.Major, "x1x1x1x1,2")
    val bob = Method(Stage.Major, "x1x1x1x1,4")
    val single = Method(Stage.Major, "x1x1x1x1,234")

    val c1 = (plain * 6 + bob) * 2
    val c2 = single + plain * 4 + bob + bob
      + plain * 6 + bob
    val c3 = (bob + plain * 5 + bob + c1) * 2
    val c4 = single + plain * 5 + bob + c1
    val c5 = bob + plain * 6
      + bob + plain * 5 + bob

    val course = (c1 + c2 + c3 + c4 + c5).course()
    println(course.isTrue)
    println(course.size)

    Play(blockToMidi(course))
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
    val plain = Method(Stage.Major, "x1x1x1x1,2")
    val bob = Method(Stage.Major, "4")
    val single = Method(Stage.Major, "234")

    val obs = 8
    val middle = 6
    val wrong = 7
    val home = 8

    val W = Call(bob, obs, wrong)
    val M = Call(bob, obs, middle)
    val H = Call(bob, obs, home)
    val sW = Call(single, obs, wrong)

    val course = plain.callsToRounds(
      H, H,
      sW, M, H, H,
      W, H, H, H,
      W, H, H, H,
      sW, H, H, H,
      W, W, H,

      H, H,
      sW, M, H, H,
      W, H, H, H,
      W, H, H, H,
      sW, H, H, H,
      W, W, H,

      H, H,
      sW, M, H, H,
      W, H, H, H,
      W, H, H, H,
      sW, H, H, H,
      W, W, H
    )

    println(course.isTrue)
    println(course.size)

    Play(blockToMidi(course))
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
    val plain = Method(Stage.Major, "34x34.18x12x18x12x18x12x18,18")
    val bob = Method(Stage.Major, "14")
    val single = Method(Stage.Major, "1234")

    val obs = 8
    val middle = 6
    val before = 1
    val wrong = 7
    val home = 8

    val M = Call(bob, obs, middle)
    val B = Call(bob, obs, before)
    val W = Call(bob, obs, wrong)
    val H = Call(bob, obs, home)

    val course = plain.callsToRounds(
      M, M, B, W, H,
      M, M, B, W, H
    )

    println(course.isTrue)
    println(course.isCourse)
    println(course.size)

    Play(blockToMidi(course))
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
    val plain = Method(Stage.Major, "3.1.7.1.7.1.7.1.7.1.7.1.7.1") // 3,1.7.1.7.1.7.1
    val bob = Method(Stage.Major, "3.1") // need to replace last two changes
    val single = Method(Stage.Major, "3.14567")

    val obs = 2
    val in = 3
    val out = 4
    val middle = 5
    val wrong = 6
    val right = 7

    val I = Call(bob, obs, in)
    val O = Call(bob, obs, out)
    val V = Call(bob, obs, out, 5)
    val M = Call(bob, obs, middle)
    val W = Call(bob, obs, wrong)
    val R = Call(bob, obs, right)
    val sI = Call(single, obs, in)

    val course = plain.callsToRounds(
      O, M, I, V, R, M, W, R, M, I,
      O, M, I, V, R, M, W, R, M, I,
      O, M, I, V, R, M, W, R, M, I,
      O, M, I, V, R, M, W, R, M, I,
      O, M, I, V, R, M, W, R, M, sI,
      O, W, R, M, W, R, I, V, W, I,
      O, W, R, M, W, R, I, V, W, I,
      O, W, R, M, W, R, I, V, W, I,
      O, W, R, M, W, R, I, V, W, I,
      O, W, R, M, W, R, I, V, W, sI
    )

    println(course.isTrue)
    println(course.isCourse)
    println(course.size)

    Play(blockToMidi(course))
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
    (TROYTE)
    */
    val plain = Method(Stage.Major, "3.1.7.3.1.3,1")
    val oddBob = Method(Stage.Major, "5.3.1.3.1.3.7.1.3.1")
    val evenBob = Method(Stage.Major, "5.1.3.1")

    val obs = 1
    val last = 5
    val outQ = 7
    val inS = 1
    val second = 3
    val outS = 5

    val bobLast = Call(oddBob, obs, last)
    val bobOutQ = Call(oddBob, obs, outQ)
    val bobInS = Call(oddBob, obs, inS)
    val bobSecond = Call(evenBob, obs, second)
    val bobOutS = Call(evenBob, obs, outS)

    val course = plain.callsToRounds(
      bobLast, bobOutQ, bobInS, bobSecond, bobOutS,
      bobLast, bobOutQ, bobInS, bobSecond, bobOutS,
      bobLast, bobOutQ, bobInS, bobSecond, bobOutS,
      bobLast, bobOutQ, bobInS, bobSecond, bobOutS,
      bobLast, bobOutQ, bobInS, bobSecond, bobOutS
    )

    // val bob = Method("3.1.5.3.1.3.1.3.7.1.3.1")
    // val bob2 = Method("3.1.7.3.1.3.1.3.5.1.3.1")
    //
    // val course = ((plain * 4 + bob
    //   + plain * 2 + bob
    //   + plain + bob
    //   + bob2
    //   + bob2
    //   + plain * 2) * 5)()

    println(course.isTrue)
    println(course.isCourse)
    println(course.size)

    Play(blockToMidi(course))
  }

  @main def nineTailorsPart4(): Unit = {
    /*
    A FULL PEAL OF KENT TREBLE BOB MAJOR (Three Parts)

    5376
    By the Course Ends
    6 5 4 3 2
    3 4 5 6 2
    2 3 6 4 5
    3 5 6 4 2
    4 2 3 5 6
    8th the Observation.
    
    Call her before, middle with a double, wrong with a double and home;
    wrong with a double and home with a double; middle with a double,
    wrong and home with a double; before, middle with a double,
    wrong and home with a double; before, middle with a double and
    wrong with a double. Twice repeated.
    (J. WILDE)

    Correction from https://www.handbellringing.co.uk/blog/a-small-dorothy-l-sayers-mystery:
    M  B  W  H  23456
    -----------------
    2  -  2  1  65432
          2  2  34562
    2     1  2  23645
    2  -  1  2  35624
       -  2     42356
    -----------------
    3 part.
    */
    val plain = Method(Stage.Major, "34x34.18x12x18x12x18x12x18,18")
    val bob = Method(Stage.Major, "14")

    val obs = 8
    val middle = 6
    val before = 1
    val wrong = 7
    val home = 8

    val M = Call(bob, obs, middle)
    val B = Call(bob, obs, before)
    val W = Call(bob, obs, wrong)
    val H = Call(bob, obs, home)

    val course = plain.callsToRounds(
      M, M, B, W, W, H,
      W, W, H, H,
      M, M, W, H, H,
      M, M, B, W, H, H,
      B, W, W,

      M, M, B, W, W, H,
      W, W, H, H,
      M, M, W, H, H,
      M, M, B, W, H, H,
      B, W, W,

      M, M, B, W, W, H,
      W, W, H, H,
      M, M, W, H, H,
      M, M, B, W, H, H,
      B, W, W
    )

    println(course.isTrue)
    println(course.isCourse)
    println(course.size)

    Play(blockToMidi(course))
  }
