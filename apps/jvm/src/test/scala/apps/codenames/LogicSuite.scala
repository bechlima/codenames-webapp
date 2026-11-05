package apps.ul2025app29

import cs214.webapp.utils.WebappSuite
import cs214.webapp.Action
import scala.util.Try

class LogicSuite extends WebappSuite[Event, State, View]:

  override val sm: Logic = new Logic()

  private val players = Vector(
    "p0" -> Role.Spymaster(1),
    "p1" -> Role.Operative(1),
    "p2" -> Role.Spymaster(2),
    "p3" -> Role.Operative(2)
  )

  private def selectTeams(): State.GameState =
    sm.setSeed(0L)
    var st: State = sm.init(Seq.empty)
    players.foreach { case (uid, role) =>
      st = assertSingleRender(sm.transition(st)(uid, Event.RoleChosen(role)))
    }
    st.asInstanceOf[State.GameState]

  private def lastRender(ts: Try[Seq[Action[State]]]): State.GameState =
    assertSuccess(ts).collect { case Action.Render(s) => s }.last.asInstanceOf[State.GameState]

  private def activeOperative(gs: State.GameState): (String, Role.Operative) =
    gs.players.head match
      case (uid, op: Role.Operative) => (uid, op)
      case other => fail(s"Expected operative active, got $other")

  test("selection creates full board with correct color counts") {
    val gs = selectTeams()
    assertEquals(gs.allCards.size, 25)
    assertEquals(gs.players.size, 4)
    val assassinCount = gs.allCards.values.count(_._2 == CardType.Assassin)
    assertEquals(assassinCount, 1)
    val blue = gs.allCards.values.count(_._2 == CardType.Blue)
    val red  = gs.allCards.values.count(_._2 == CardType.Red)
    assertEquals(blue + red + assassinCount + gs.allCards.values.count(_._2 == CardType.Neutral), 25)
    assert((blue == 9 && red == 8) || (blue == 8 && red == 9))
  }

  test("starting team gets 9 of its color and starts first") {
    val gs = selectTeams()
    val startingTeam = gs.players.head._2.team
    val blue = gs.allCards.values.count(_._2 == CardType.Blue)
    val red  = gs.allCards.values.count(_._2 == CardType.Red)
    if startingTeam == 1 then assertEquals(blue, 9) else assertEquals(red, 9)
  }

  test("hint moves to choosing cards and operative becomes active") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val actions = sm.transition(gs)(spymaster, Event.Hint("tree", 2))
    val next = lastRender(actions)
    assertEquals(next.phase, Phase.ChoosingCards)
    assertEquals(next.hint, Some("tree" -> 2))
    assert(next.players.head._2.isInstanceOf[Role.Operative])
  }

  test("correct guess keeps operative turn and decrements guesses") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val choosing = lastRender(sm.transition(gs)(spymaster, Event.Hint("sea", 2)))
    val (opId, opRole) = activeOperative(choosing)
    val ownColor = if opRole.team == 1 then CardType.Blue else CardType.Red
    val target = choosing.allCards.collectFirst { case (id, (_, ct, false)) if ct == ownColor => id }.get

    val afterGuess = lastRender(sm.transition(choosing)(opId, Event.CardClicked(target)))
    assertEquals(afterGuess.phase, Phase.ChoosingCards)
    assertEquals(afterGuess.players.head._1, opId)
    assertEquals(afterGuess.hint.map(_._2), Some(1))
  }

  test("correct guess increments active team score") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val choosing = lastRender(sm.transition(gs)(spymaster, Event.Hint("sea", 2)))
    val (opId, opRole) = activeOperative(choosing)
    val ownColor = if opRole.team == 1 then CardType.Blue else CardType.Red
    val target = choosing.allCards.collectFirst { case (id, (_, ct, false)) if ct == ownColor => id }.get
    val beforeScore = choosing.scores(opRole.team)

    val afterGuess = lastRender(sm.transition(choosing)(opId, Event.CardClicked(target)))
    assertEquals(afterGuess.scores(opRole.team), beforeScore + 1)
  }

  test("hint budget zero ends turn after next correct guess") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val choosing = lastRender(sm.transition(gs)(spymaster, Event.Hint("sea", 0)))
    val (opId, opRole) = activeOperative(choosing)
    val ownColor = if opRole.team == 1 then CardType.Blue else CardType.Red
    val target = choosing.allCards.collectFirst { case (id, (_, ct, false)) if ct == ownColor => id }.get

    val afterGuess = lastRender(sm.transition(choosing)(opId, Event.CardClicked(target)))
    assertEquals(afterGuess.phase, Phase.GivingHints)
    afterGuess.players.head._2 match
      case Role.Spymaster(team) => assertNotEquals(team, opRole.team)
      case other => fail(s"Expected spymaster of other team, got $other")
  }

  test("wrong color ends turn and passes to other team spymaster") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val choosing = lastRender(sm.transition(gs)(spymaster, Event.Hint("rock", 1)))
    val (opId, opRole) = activeOperative(choosing)
    val ownColor = if opRole.team == 1 then CardType.Blue else CardType.Red
    val target = choosing.allCards.collectFirst { case (id, (_, ct, false)) if ct != ownColor && ct != CardType.Assassin => id }.get

    val afterGuess = lastRender(sm.transition(choosing)(opId, Event.CardClicked(target)))
    assertEquals(afterGuess.phase, Phase.GivingHints)
    afterGuess.players.head._2 match
      case Role.Spymaster(team) => assertNotEquals(team, opRole.team)
      case other => fail(s"Expected spymaster to lead next turn, got $other")
  }

  test("neutral ends turn without score change") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val choosing = lastRender(sm.transition(gs)(spymaster, Event.Hint("stone", 1)))
    val (opId, _) = activeOperative(choosing)
    val neutralId = choosing.allCards.collectFirst { case (id, (_, CardType.Neutral, false)) => id }.get

    val afterGuess = lastRender(sm.transition(choosing)(opId, Event.CardClicked(neutralId)))
    assertEquals(afterGuess.phase, Phase.GivingHints)
    assertEquals(afterGuess.scores, choosing.scores)
  }

  test("already flipped card does nothing") {
    val gs: State.GameState = selectTeams()
    val spymaster = gs.players.head._1
    val choosing: State.GameState = lastRender(sm.transition(gs)(spymaster, Event.Hint("stone", 1)))
    val flippedId = choosing.allCards.keys.head
    val flippedState: State.GameState =
      choosing.copy(allCards = choosing.allCards.updated(flippedId, (choosing.allCards(flippedId)._1, choosing.allCards(flippedId)._2, true)))
    val expectedPhase: Phase = choosing.phase

    val res = sm.transition(flippedState)(choosing.players.head._1, Event.CardClicked(flippedId))
    val after: State.GameState = lastRender(res)
    assertEquals(after.allCards(flippedId)._3, true)
    assertEquals(after.phase, expectedPhase) // phase should stay unchanged
  }

  test("all own cards flipped triggers win for active team") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val choosing = lastRender(sm.transition(gs)(spymaster, Event.Hint("all", 25)))
    val (opId, opRole) = activeOperative(choosing)
    val ownColor = if opRole.team == 1 then CardType.Blue else CardType.Red
    val ownCards = choosing.allCards.collect { case (id, (_, ct, false)) if ct == ownColor => id }.toList

    val finalState = ownCards.foldLeft(choosing) { (st, cid) =>
      lastRender(sm.transition(st)(opId, Event.CardClicked(cid)))
    }
    assertEquals(finalState.phase, Phase.EndScreen(List(opRole.team)))
  }

  test("wrong color increments opponent score and can win") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val choosing = lastRender(sm.transition(gs)(spymaster, Event.Hint("rock", 1)))
    val (opId, opRole) = activeOperative(choosing)
    val ownColor = if opRole.team == 1 then CardType.Blue else CardType.Red
    val oppColor = if ownColor == CardType.Blue then CardType.Red else CardType.Blue
    val target = choosing.allCards.collectFirst { case (id, (_, ct, false)) if ct == oppColor => id }.get
    val oppTeam = if opRole.team == 1 then 2 else 1

    val afterGuess = lastRender(sm.transition(choosing)(opId, Event.CardClicked(target)))
    assertEquals(afterGuess.scores(oppTeam), choosing.scores(oppTeam) + 1)
  }

  test("opponent wins immediately when their last card is hit") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val choosing = lastRender(sm.transition(gs)(spymaster, Event.Hint("rock", 1)))
    val (opId, opRole) = activeOperative(choosing)
    val ownColor = if opRole.team == 1 then CardType.Blue else CardType.Red
    val oppTeam = if opRole.team == 1 then 2 else 1
    val oppColor = if ownColor == CardType.Blue then CardType.Red else CardType.Blue
    val oppCards = choosing.allCards.collect { case (id, (_, ct, false)) if ct == oppColor => id }.toList
    val target = oppCards.head
    val preFlipped = oppCards.tail.foldLeft(choosing.allCards) { (cards, cid) =>
      val (w, ct, _) = cards(cid)
      cards.updated(cid, (w, ct, true))
    }
    val preScores = choosing.scores.updated(oppTeam, oppCards.size - 1)
    val nearlyDone = choosing.copy(allCards = preFlipped, scores = preScores)

    val finalState = lastRender(sm.transition(nearlyDone)(opId, Event.CardClicked(target)))
    assertEquals(finalState.phase, Phase.EndScreen(List(oppTeam)))
  }

  test("illegal hint in wrong phase fails") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val choosing = lastRender(sm.transition(gs)(spymaster, Event.Hint("legit", 1)))
    val res = sm.transition(choosing)(spymaster, Event.Hint("illegal", 1))
    assert(res.isFailure)
  }

  test("operative cannot hint") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val choosing = lastRender(sm.transition(gs)(spymaster, Event.Hint("legit", 1)))
    val operativeId = choosing.players.head._1
    val res = sm.transition(choosing)(operativeId, Event.Hint("nope", 1))
    assert(res.isFailure)
  }

  test("empty hint is rejected") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val res = sm.transition(gs)(spymaster, Event.Hint("", 1))
    assert(res.isFailure)
  }

  test("whitespace-only hint is rejected") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val res = sm.transition(gs)(spymaster, Event.Hint("  ", 2))
    assert(res.isFailure)
  }

  test("negative hint count is rejected") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val res = sm.transition(gs)(spymaster, Event.Hint("bad", -1))
    assert(res.isFailure)
  }

  test("assassin immediately ends game for other team") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val choosing = lastRender(sm.transition(gs)(spymaster, Event.Hint("doom", 1)))
    val (opId, opRole) = activeOperative(choosing)
    val assassinId = choosing.allCards.collectFirst { case (id, (_, CardType.Assassin, false)) => id }.get

    val after = lastRender(sm.transition(choosing)(opId, Event.CardClicked(assassinId)))
    assertEquals(after.phase, Phase.EndScreen(List(if opRole.team == 1 then 2 else 1)))
  }

  test("operative view hides unflipped colors and shows flipped own") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val choosing = lastRender(sm.transition(gs)(spymaster, Event.Hint("see", 1)))
    val (opId, opRole) = activeOperative(choosing)
    val ownColor = if opRole.team == 1 then CardType.Blue else CardType.Red
    val ownCardId = choosing.allCards.collectFirst { case (id, (_, ct, false)) if ct == ownColor => id }.get

    val afterFlip = lastRender(sm.transition(choosing)(opId, Event.CardClicked(ownCardId)))
    val operativeView = sm.project(afterFlip)(opId).asInstanceOf[View.OperativeView]
    assertEquals(operativeView.flippedCards.get(ownCardId), Some(ownColor))
    assertEquals(operativeView.allCards.size, choosing.allCards.size)
  }

  test("spymaster view shows all card colors in choosing phase") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val choosing = lastRender(sm.transition(gs)(spymaster, Event.Hint("see", 1)))
    val anySpymasterId = choosing.players.collectFirst { case (uid, r: Role.Spymaster) => uid }.get

    val view = sm.project(choosing)(anySpymasterId).asInstanceOf[View.SpymasterView]
    assertEquals(view.allCards.keySet, choosing.allCards.keySet)
    val sampleId = choosing.allCards.keys.head
    assertEquals(view.allCards(sampleId)._2, choosing.allCards(sampleId)._2)
  }

  test("operative view reveals flipped opponent card after wrong guess") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val choosing = lastRender(sm.transition(gs)(spymaster, Event.Hint("rock", 1)))
    val (opId, opRole) = activeOperative(choosing)
    val ownColor = if opRole.team == 1 then CardType.Blue else CardType.Red
    val oppColor = if ownColor == CardType.Blue then CardType.Red else CardType.Blue
    val target = choosing.allCards.collectFirst { case (id, (_, ct, false)) if ct == oppColor => id }.get

    val afterGuess = lastRender(sm.transition(choosing)(opId, Event.CardClicked(target)))
    val view = sm.project(afterGuess)(opId).asInstanceOf[View.OperativeView]
    assertEquals(view.flippedCards.get(target), Some(oppColor))
    val someOtherOpp = afterGuess.allCards.collectFirst { case (id, (_, ct, false)) if ct == oppColor && id != target => id }
    someOtherOpp.foreach { otherId =>
      assertEquals(view.flippedCards.get(otherId), None)
    }
  }

  test("card clicked outside choosing phase fails") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val res = sm.transition(gs)(spymaster, Event.CardClicked(0))
    assert(res.isFailure)
  }

  test("end screen rejects further events") {
    val gs = selectTeams()
    val ended = gs.copy(phase = Phase.EndScreen(List(1)))
    assert(sm.transition(ended)(gs.players.head._1, Event.CardClicked(0)).isFailure)
    assert(sm.transition(ended)(gs.players.head._1, Event.Hint("x", 1)).isFailure)
    assert(sm.transition(ended)(gs.players.head._1, Event.SkippedTurn).isFailure)
  }

  test("skip turn clears hint and returns to giving hints") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val choosing = lastRender(sm.transition(gs)(spymaster, Event.Hint("pass", 1)))
    val (opId, _) = activeOperative(choosing)

    val afterSkip = lastRender(sm.transition(choosing)(opId, Event.SkippedTurn))
    assertEquals(afterSkip.phase, Phase.GivingHints)
    assertEquals(afterSkip.hint, None)
    afterSkip.players.head._2 match
      case Role.Spymaster(team) => assertNotEquals(team, choosing.players.head._2.team)
      case other => fail(s"Expected spymaster to be active after skip, got $other")
  }

  test("skip fails for spymaster and outside choosing phase") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    assert(sm.transition(gs)(spymaster, Event.SkippedTurn).isFailure)
    val choosing = lastRender(sm.transition(gs)(spymaster, Event.Hint("ok", 1)))
    val nextHintPhase = lastRender(sm.transition(choosing)(choosing.players.head._1, Event.SkippedTurn))
    assert(sm.transition(nextHintPhase)(nextHintPhase.players.head._1, Event.SkippedTurn).isFailure)
  }

  test("action sequencing for hint includes pause and second render") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val actions = assertSuccess(sm.transition(gs)(spymaster, Event.Hint("tree", 1)))
    assertEquals(actions.length, 3)
    assert(actions.head.isInstanceOf[Action.Render[State]])
    assert(actions(1).isInstanceOf[Action.Pause[State]])
    assert(actions(2).isInstanceOf[Action.Render[State]])
  }

  test("action sequencing for card click includes viewing flip") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val choosing = lastRender(sm.transition(gs)(spymaster, Event.Hint("tree", 1)))
    val (opId, _) = activeOperative(choosing)
    val cardId = choosing.allCards.keys.head
    val actions = assertSuccess(sm.transition(choosing)(opId, Event.CardClicked(cardId)))
    assertEquals(actions.length, 3)
    assert(actions.head.isInstanceOf[Action.Render[State]])
    assert(actions(1).isInstanceOf[Action.Pause[State]])
    assert(actions(2).isInstanceOf[Action.Render[State]])
  }

  test("turn order rotates to next spymaster after turn end") {
    val gs = selectTeams()
    val spymaster = gs.players.head._1
    val choosing = lastRender(sm.transition(gs)(spymaster, Event.Hint("stone", 0)))
    val (opId, opRole) = activeOperative(choosing)
    val ownColor = if opRole.team == 1 then CardType.Blue else CardType.Red
    val target = choosing.allCards.collectFirst { case (id, (_, ct, false)) if ct == ownColor => id }.get
    val after = lastRender(sm.transition(choosing)(opId, Event.CardClicked(target)))
    after.players.head._2 match
      case Role.Spymaster(team) => assertNotEquals(team, opRole.team)
      case other => fail(s"Expected spymaster leading, got $other")
  }

  test("out of bounds card id fails") {
    val gs = selectTeams()
    val res = sm.transition(gs)(gs.players.head._1, Event.CardClicked(999))
    assert(res.isFailure)
  }

  private def allFlipped(allCards: Map[CardId, (String, CardType, Boolean)], team: Team): Boolean =
    val ownColor = if team == 1 then CardType.Blue else CardType.Red
    allCards.values.forall { case (_, ct, flipped) => ct != ownColor || flipped }
