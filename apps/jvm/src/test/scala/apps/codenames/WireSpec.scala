package apps.ul2025app29

import munit.FunSuite
import ujson.{Obj, Str, Num, Null}

class WireSpec extends FunSuite:

  test("event round-trips every variant") {
    val events = Seq[Event](
      Event.Hint("tree", 2),
      Event.CardClicked(5),
      Event.RoleChosen(Role.Spymaster(1)),
      Event.RoleChosen(Role.Operative(2)),
      Event.SkippedTurn
    )

    events.foreach { e =>
      val decoded = Wire.eventFormat.decode(Wire.eventFormat.encode(e))
      assert(decoded.isSuccess, decoded.toString)
      assertEquals(decoded.get, e)
    }
  }

  test("event decode fails on unknown id") {
    val badJson = Obj("id" -> Str("Nope"))
    val res = Wire.eventFormat.decode(badJson)
    assert(res.isFailure)
  }

  test("selection view round-trip") {
    val roles = Map[cs214.webapp.UserId, Role](
      "u1" -> Role.Spymaster(1),
      "u2" -> Role.Operative(1)
    )
    val view = View.SelectionView(roles)
    val decoded = Wire.viewFormat.decode(Wire.viewFormat.encode(view))
    assert(decoded.isSuccess, decoded.toString)
    assertEquals(decoded.get, view)
  }

  test("spymaster view round-trip with all card types") {
    val cards = Map[CardId, (String, CardType, Boolean)](
      0 -> ("apple", CardType.Blue, false),
      1 -> ("river", CardType.Red, true),
      2 -> ("garden", CardType.Neutral, false),
      3 -> ("knife", CardType.Assassin, true)
    )
    val view = View.SpymasterView(
      cards,
      scores = Map(1 -> 3, 2 -> 4),
      hint = Some("hint" -> 2),
      players = Vector("u1" -> Role.Spymaster(1)),
      phaseView = PhaseView.ViewingFlip(3)
    )
    val decoded = Wire.viewFormat.decode(Wire.viewFormat.encode(view))
    assert(decoded.isSuccess, decoded.toString)
    assertEquals(decoded.get, view)
  }

  test("operative view round-trip") {
    val words = Map[CardId, String](0 -> "apple", 1 -> "river")
    val flipped = Map[CardId, CardType](1 -> CardType.Red)
    val view = View.OperativeView(
      allCards = words,
      flippedCards = flipped,
      scores = Map(1 -> 0, 2 -> 1),
      hint = None,
      players = Vector("u1" -> Role.Operative(1)),
      phaseView = PhaseView.ChoosingCards
    )
    val decoded = Wire.viewFormat.decode(Wire.viewFormat.encode(view))
    assert(decoded.isSuccess, decoded.toString)
    assertEquals(decoded.get, view)
  }

  test("end screen round-trip") {
    val view = View.EndScreen(List(2))
    val decoded = Wire.viewFormat.decode(Wire.viewFormat.encode(view))
    assert(decoded.isSuccess, decoded.toString)
    assertEquals(decoded.get, view)
  }

  test("spymaster cards reject invalid cardType ordinal") {
    val bad = Obj(
      "id" -> Str("SpymasterView"),
      "allCards" -> Obj("0" -> Obj("word" -> Str("x"), "cardType" -> Num(99), "isFlipped" -> true)),
      "scores" -> Obj("1" -> Num(0)),
      "players" -> Obj("u1" -> Str("Spymaster(1)")),
      "phaseView" -> Obj("id" -> Str("GivingHints"))
    )
    val res = Wire.viewFormat.decode(bad)
    assert(res.isFailure)
  }

  test("phaseView decode fails on unknown id") {
    val bad = Obj(
      "id" -> Str("SpymasterView"),
      "allCards" -> Obj("0" -> Obj("word" -> Str("x"), "cardType" -> Num(0), "isFlipped" -> true)),
      "scores" -> Obj("1" -> Num(0)),
      "players" -> Obj("u1" -> Str("Spymaster(1)")),
      "phaseView" -> Obj("id" -> Str("Nope"))
    )
    val res = Wire.viewFormat.decode(bad)
    assert(res.isFailure)
  } 

  test("scores reject non-integer keys") {
    val bad = Obj(
      "id" -> Str("OperativeView"),
      "allCards" -> Obj("0" -> Str("word")),
      "flippedCards" -> Obj("0" -> Num(0)),
      "scores" -> Obj("teamX" -> Num(1)),
      "players" -> Obj("u1" -> Str("Operative(1)")),
      "phaseView" -> Obj("id" -> Str("Waiting"))
    )
    val res = Wire.viewFormat.decode(bad)
    assert(res.isFailure)
  }

  test("scores reject non-integer values") {
    val bad = Obj(
      "id" -> Str("SpymasterView"),
      "allCards" -> Obj("0" -> Obj("word" -> Str("x"), "cardType" -> Num(0), "isFlipped" -> true)),
      "scores" -> Obj("1" -> Str("one")),
      "players" -> Obj("u1" -> Str("Spymaster(1)")),
      "phaseView" -> Obj("id" -> Str("GivingHints"))
    )
    val res = Wire.viewFormat.decode(bad)
    assert(res.isFailure)
  }

  test("players reject unknown role string") {
    val bad = Obj(
      "id" -> Str("SpymasterView"),
      "allCards" -> Obj("0" -> Obj("word" -> Str("x"), "cardType" -> Num(0), "isFlipped" -> true)),
      "scores" -> Obj("1" -> Num(0)),
      "players" -> Obj("u1" -> Str("Spy(1)")),
      "phaseView" -> Obj("id" -> Str("GivingHints"))
    )
    val res = Wire.viewFormat.decode(bad)
    assert(res.isFailure)
  }

  test("operative cards reject non-integer cardId key") {
    val bad = Obj(
      "id" -> Str("OperativeView"),
      "allCards" -> Obj("foo" -> Str("word")),
      "flippedCards" -> Obj("0" -> Num(0)),
      "scores" -> Obj("1" -> Num(0)),
      "players" -> Obj("u1" -> Str("Operative(1)")),
      "phaseView" -> Obj("id" -> Str("Waiting"))
    )
    val res = Wire.viewFormat.decode(bad)
    assert(res.isFailure)
  }

  test("flippedCards reject invalid ordinal") {
    val bad = Obj(
      "id" -> Str("OperativeView"),
      "allCards" -> Obj("0" -> Str("word")),
      "flippedCards" -> Obj("0" -> Num(99)),
      "scores" -> Obj("1" -> Num(0)),
      "players" -> Obj("u1" -> Str("Operative(1)")),
      "phaseView" -> Obj("id" -> Str("Waiting"))
    )
    val res = Wire.viewFormat.decode(bad)
    assert(res.isFailure)
  }

  test("hint decode fails on non-integer nbOfWords") {
    val badHint = Obj("id" -> Str("Hint"), "word" -> Str("x"), "nbOfWords" -> Num(2.5))
    val view = Obj(
      "id" -> Str("SpymasterView"),
      "allCards" -> Obj("0" -> Obj("word" -> Str("x"), "cardType" -> Num(0), "isFlipped" -> true)),
      "scores" -> Obj("1" -> Num(0)),
      "players" -> Obj("u1" -> Str("Spymaster(1)")),
      "phaseView" -> Obj("id" -> Str("GivingHints")),
      "hint" -> badHint
    )
    val res = Wire.viewFormat.decode(view)
    assert(res.isFailure)
  }

  test("views reject missing required fields") {
    val missingPhase = Obj(
      "id" -> Str("SpymasterView"),
      "allCards" -> Obj("0" -> Obj("word" -> Str("x"), "cardType" -> Num(0), "isFlipped" -> true)),
      "scores" -> Obj("1" -> Num(0)),
      "players" -> Obj("u1" -> Str("Spymaster(1)"))
    )
    val res = Wire.viewFormat.decode(missingPhase)
    assert(res.isFailure)
  }
