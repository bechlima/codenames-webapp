package apps
package ul2025app29


import cs214.webapp.*
import cs214.webapp.Action.*
import cs214.webapp.server.StateMachine
import ujson.Value

import scala.annotation.unused
import scala.util.{Random, Try, Success, Failure}
import apps.ul2025app29.Role.Spymaster
import java.rmi.server.Operation
import scala.annotation.switch
import apps.ul2025app29.State.GameState
import java.util.IllegalFormatException
import scala.util.Using
import scala.io.Source




class Logic extends StateMachine[Event, State, View]:
    private val random = Random()
    // exposed for tests to seed deterministically
    private[ul2025app29] def setSeed(seed: Long): Unit = random.setSeed(seed)
    private val DISPLAY_HINT_MS = 2500
    private val DISPLAY_FLIP_MS = 2500
    private lazy val allWords: Vector[String] = {
        Using.resource(Source.fromResource("www/static/wordlist.txt")) { src =>
            src.getLines()
            .map(_.trim)
            .filter(s => s.nonEmpty)
            .toVector
            .distinct
            }
        }

    override val appInfo: AppInfo = AppInfo(
        id = "ul2025app29",
        name = "Codenames",
        description = "Codenames is a team word game where players guess secret words " +
          "using one-word clues, avoiding the opponent’s words and the assassin.",
        year = 2025
    )
    override def wire: AppWire[Event, View] = ul2025app29.Wire

    override def init(clients: Seq[UserId]): State =
      State.SelectionState(Map.empty[UserId, Role])

    override def transition(state: State)(userId: UserId, event: Event): Try[Seq[Action[State]]] =
        Try:
            state match
                case ss @ State.SelectionState(rolesChosen) =>
                    require(rolesChosen.size >= 0 && rolesChosen.size < 4)
                    event match
                        case Event.RoleChosen(role) =>
                            val newMap = rolesChosen.updated(userId, role)

                            if newMap.size == 4 then

                                val startingTeam = random.nextInt(2) + 1
                                val newPlayers = 
                                    newMap.toVector.sortBy(
                                        (uid, role) => (role.team != startingTeam, role.ordinal)
                                    )
                                Seq(Render(State.GameState(
                                    allCards = createAllCards(startingTeam),
                                    scores = Map(1 -> 0, 2 -> 0),
                                    players = newPlayers,
                                    hint = None,
                                    phase = Phase.GivingHints
                                )))
                            else
                                Seq(Render(ss.copy(rolesChosen = newMap)))

                        case Event.CardClicked(_) =>
                            throw new IllegalMoveException(s"User $userId tried to click a card")

                        case Event.Hint(_, _) =>
                            throw new IllegalMoveException(s"User $userId tried to send a hint")

                        case Event.SkippedTurn =>
                            throw new IllegalMoveException(s"User $userId tried to skip his turn while choosing roles")
                    
                case gs @ State.GameState(allCards, scores, players, hint, phase) =>
                    event match
                        case Event.Hint(hint, nbOfWords) => 
                            if phase == Phase.GivingHints then 
                                val cleanedHint = hint.trim
                                if cleanedHint.isEmpty then
                                    throw new IllegalMoveException(s"User $userId tried to give an empty hint")
                                if nbOfWords < 0 then
                                    throw new IllegalMoveException(s"User $userId tried to give a hint with a negative count")

                                val newPlayers =  players.tail :+ players.head 
                                    
                                Seq(Render(gs.copy(phase = Phase.DisplayingHint, hint = Option((cleanedHint, nbOfWords)))),
                                    Pause(DISPLAY_HINT_MS),
                                    Render(gs.copy(
                                        phase = Phase.ChoosingCards,
                                        players = newPlayers,
                                        hint = Some((cleanedHint, nbOfWords))
                                    )))
                            else
                                throw new IllegalMoveException(s"User $userId tried to give a hint outside of the hinting phase")
                        case Event.CardClicked(cardId) if phase == Phase.ChoosingCards =>
                            // Extract card
                            allCards(cardId) match
                                // already flipped: do nothing
                                case (_, _, true) =>
                                    Seq(Render(gs))

                                // not flipped yet
                                case (word, cardType, false) =>
                                    val teamPlaying = players.head._2.team              // 1 or 2
                                    val otherTeam   = if teamPlaying == 1 then 2 else 1

                                    // which color belongs to the current team?
                                    val (ownColor, oppColor) =
                                        if teamPlaying == 1 then (CardType.Blue, CardType.Red)
                                        else (CardType.Red, CardType.Blue)

                                    // mark card as flipped
                                    val newCards = allCards.updated(cardId, (word, cardType, true))

                                    // small helper to always do: View flip -> pause -> render final state
                                    def flipThen(nextGs: GameState) =
                                        Seq(
                                        Render(gs.copy(allCards = newCards, phase = Phase.ViewingFlip(cardId))),
                                        Pause(DISPLAY_FLIP_MS),
                                        Render(nextGs)
                                        )

                                    cardType match
                                        // clicked their own color
                                        case `ownColor` =>
                                            val newScores = scores.updated(teamPlaying, scores(teamPlaying) + 1)
                                            val teamWon = allFlipped(newCards, teamPlaying)

                                            if teamWon then
                                                flipThen(gs.copy(phase = Phase.EndScreen(List(teamPlaying))))
                                            else
                                                val (newPlayers, newHint, newPhase) =
                                                if hint.get._2 == 0 then
                                                    (players.tail :+ players.head, None, Phase.GivingHints)
                                                else
                                                    (players, Some((hint.get._1, hint.get._2 - 1)), phase)

                                                flipThen(GameState(newCards, newScores, newPlayers, newHint, newPhase))

                                        // clicked opponent color
                                        case `oppColor` =>
                                            val newScores = scores.updated(otherTeam, scores(otherTeam) + 1)
                                            val teamWon = allFlipped(newCards, otherTeam)

                                            if teamWon then
                                                flipThen(gs.copy(phase = Phase.EndScreen(List(otherTeam))))
                                            else
                                                val newPlayers = players.tail :+ players.head
                                                val newHint = None
                                                val newPhase = Phase.GivingHints
                                                flipThen(GameState(newCards, newScores, newPlayers, newHint, newPhase))

                                        // neutral: no score, just pass turn
                                        case CardType.Neutral =>
                                            val newPlayers = players.tail :+ players.head
                                            val newHint = None
                                            val newPhase = Phase.GivingHints
                                            flipThen(GameState(newCards, scores, newPlayers, newHint, newPhase))

                                        // assassin: other team wins immediately
                                        case CardType.Assassin =>
                                            flipThen(gs.copy(phase = Phase.EndScreen(List(otherTeam))))
                                        // should never reach here but have to do it due to the pattern match not being exhaustive
                                        case _ => 
                                            throw new IllegalStateException()

                        case Event.CardClicked(cardId) =>
                            throw new IllegalMoveException(
                                s"User $userId tried to click a card while outside of the clicking phase"
                            )
                       
                        case Event.SkippedTurn =>
                            players.head._2 match
                                case Role.Spymaster(_) =>
                                    throw new IllegalMoveException(s"User $userId tried to skip a turn while being spymaster")
                                case Role.Operative(_) =>
                                    val newPlayers = players.tail :+ players.head
                                    val newHint = None 
                                    val newPhase = Phase.GivingHints
                                    Seq(Render(GameState(allCards, scores, newPlayers, newHint, newPhase)))
                            
                        case Event.RoleChosen(role) =>
                            throw new IllegalMoveException(s"User $userId tried to choose a role")
                
                
    override def project(state: State)(userId: UserId): View =
        state match
            case State.GameState(allCards, scores, players, hint, phase) =>
                lazy val newAllCards = getAllCardsNoColor(allCards)
                lazy val newFlippedCards = getFlippedCards(allCards)
                val playersMap = players.toMap
                phase match
                    case Phase.GivingHints => 
                        if userId == players.head._1 then 
                            View.SpymasterView(allCards, scores, hint, players, PhaseView.GivingHints)
                        else 
                            playersMap(userId) match
                                case Role.Spymaster(team) =>
                                    View.SpymasterView(allCards, scores, hint, players, PhaseView.Waiting)
                                case Role.Operative(team) =>
                                    View.OperativeView(newAllCards, newFlippedCards, scores, hint, players, PhaseView.Waiting)

                    case Phase.ViewingFlip(cardId) =>
                        playersMap(userId) match
                            case Role.Spymaster(team) => 
                                View.SpymasterView(allCards, scores, hint, players, PhaseView.ViewingFlip(cardId))
                            case Role.Operative(team) =>
                                View.OperativeView(newAllCards, newFlippedCards, scores, hint, players, PhaseView.ViewingFlip(cardId))
        

                    case Phase.DisplayingHint =>
                        playersMap(userId) match
                            case Role.Spymaster(team) => 
                                View.SpymasterView(allCards, scores, hint, players, PhaseView.DisplayingHint)
                            case Role.Operative(team) =>
                                View.OperativeView(newAllCards, newFlippedCards, scores, hint, players, PhaseView.DisplayingHint)

                    case Phase.ChoosingCards =>
                        if userId == players.head._1 then
                            val team = playersMap(userId).team
                            View.OperativeView(newAllCards, newFlippedCards, scores, hint, players, PhaseView.ChoosingCards)
                        else 
                            playersMap(userId) match
                                case Role.Spymaster(team) =>
                                    View.SpymasterView(allCards, scores, hint, players, PhaseView.Waiting)
                                case Role.Operative(team) =>
                                    View.OperativeView(newAllCards, newFlippedCards, scores, hint, players, PhaseView.Waiting)

                    case Phase.EndScreen(winningTeam) => 
                        View.EndScreen(winningTeam)

            case State.SelectionState(rolesChosen) =>
                View.SelectionView(rolesChosen)

    /**
      * Method that creates the starting layout for the game by randomly choosing 25 cards, assigning to them
      * a type, and saying that they are not flipped.
      *
      * @param startingTeam: the team that starts the game since they get 1 extra card to guess
      * @return the layout as explaned above
      */
    private def createAllCards(startingTeam: Team): Map[CardId, (String, CardType, Flipped)] =
        val gameWords = random.shuffle(allWords).take(25)
        val cardIDs = 0 until 25

        val base =
        if startingTeam == 1 then
            List.fill(9)(CardType.Blue) ++ List.fill(8)(CardType.Red)
        else
            List.fill(9)(CardType.Red) ++ List.fill(8)(CardType.Blue)

        val cardTypes =
        random.shuffle(
            base ++ List.fill(7)(CardType.Neutral) :+ CardType.Assassin
        )

        val pairs =
        for
            ((i, s), ct) <- cardIDs.zip(gameWords).zip(cardTypes)
        yield (i, (s, ct, false))

        pairs.toMap

    /**
      * Method that takes the current layout and a team, and returns if the input team has won the game
      *
      * @param allCards: the current layout
      * @param team: the team
      * @return whether the input team has won the game or not
      */
    private def allFlipped(allCards: Map[CardId, (String, CardType, Flipped)], team: Team): Boolean =
        val ownColor =
            if team == 1 then CardType.Blue else CardType.Red
        allCards.
            values.
            forall((w, ct, f) => ownColor != ct || f)

    /**
      * Method that takes all of the cards with all of their information and returns only 
      * what that team's operative should see
      *
      * @param allCards the map that contains all of the cards in the game and their informations
      * @param team the team which we want to create the view for
      * @return the map that shows that team's operative view
      */
    private def getFlippedCards(allCards: Map[CardId, (String, CardType, Flipped)]): Map[CardId, CardType] =
        for 
            (k, v) <- allCards 
            if v._3
        yield (k, v._2) 

    /**
      * Method that takes all of the cards with all of their information and returns all of the cards without their colors
      * or if they are flipped or not.
      *
      * @param allCards the map that contains all of the cards in the game and their informations 
      * @return the map that shows all cards in the game
      */
    private def getAllCardsNoColor(allCards: Map[CardId, (String, CardType, Flipped)]): Map[CardId, String] = 
        for
            (k, v) <- allCards
        yield (k, v._1)
