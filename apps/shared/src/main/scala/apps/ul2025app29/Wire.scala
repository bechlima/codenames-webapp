package apps
package ul2025app29

import cs214.webapp.*
import ujson.*
import scala.util.Try

object Wire extends AppWire[Event, View]:

  // events

  override object eventFormat extends WireFormat[Event]:
    override def encode(event: Event): Value =
      event match
        case Event.Hint(wordHint, nbOfWords) =>
          Obj(
            "id" -> Str("Hint"),
            "hint" -> Str(wordHint),
            "number of words"-> Num(nbOfWords)
          )

        case Event.CardClicked(cardId) =>
          Obj(
            "id" -> Str("CardClicked"),
            "cardId" -> Num(cardId)
          )

        case Event.RoleChosen(role) =>
          val (roleName, team) = role match
            case Role.Spymaster(t) => ("Spymaster", t)
            case Role.Operative(t) => ("Operative", t)
          Obj(
            "id"-> Str("RoleChosen"),
            "role" -> Str(roleName),
            "team" -> Num(team)
          )

        case Event.SkippedTurn =>
          Obj("id" -> Str("SkippedTurn"))

    override def decode(json: Value): Try[Event] =
      Try {
        val obj = json.obj
        obj("id").str match
          case "Hint" =>
            val hint = obj("hint").str
            val nbOfWords = obj("number of words").num.toInt
            Event.Hint(hint, nbOfWords)

          case "CardClicked" =>
            val cardId = obj("cardId").num.toInt
            Event.CardClicked(cardId)

          case "RoleChosen" =>
            val roleStr = obj("role").str
            val team = obj("team").num.toInt
            val role = roleStr match
              case "Spymaster" => Role.Spymaster(team)
              case "Operative" => Role.Operative(team)
              case other       => throw DecodingException(s"Unknown role: $other")
            Event.RoleChosen(role)

          case "SkippedTurn" =>
            Event.SkippedTurn

          case other =>
            throw DecodingException(s"Unknown event id: $other")
      }

  // view
  override object viewFormat extends WireFormat[View]:
    // encode View
    override def encode(v: View): Value =
      v match
        case View.SelectionView(rolesChosen) =>
          Obj(
            "id" -> Str("SelectionView"),
            "rolesChosen" ->
              Obj.from(
                rolesChosen.map { case (userId, role) =>
                  val (roleName, team) = role match
                    case Role.Spymaster(t) => ("Spymaster", t)
                    case Role.Operative(t) => ("Operative", t)
                  userId.toString ->
                    Obj(
                      "role" -> Str(roleName),
                      "team" -> Num(team)
                    )
                }
              )
          )

        case View.SpymasterView(allCards, scores, hint, players, phaseView) =>
          Obj(
            "id" -> Str("SpymasterView"),
            "allCards" -> encodeSpymasterCards(allCards),
            "scores"-> encodeScores(scores),
            "players" -> encodePlayers(players),
            "hint" -> encodeHint(hint),
            "phaseView" -> encodePhaseView(phaseView)
          )

        case View.OperativeView(allCards, flippedCards, scores, hint, players, phaseView) =>
          Obj(
            "id" -> Str("OperativeView"),
            "allCards" -> encodeOperativeCards(allCards),
            "flippedCards" -> encodeFlippedCards(flippedCards),
            "scores"  -> encodeScores(scores),
            "players" -> encodePlayers(players),
            "hint" -> encodeHint(hint),
            "phaseView" -> encodePhaseView(phaseView)
          )

        case View.EndScreen(winningTeam) =>
          Obj(
            "id" -> Str("EndScreen"),
            "winningTeam" -> Arr.from(winningTeam.map(t => Num(t)))
          )

    // decode View 

    override def decode(json: Value): Try[View] =
      Try {
        json match
          case Obj(fields) =>
            val id = fields.get("id") match
              case Some(Str(s)) => s
              case Some(other)  => throw DecodingException(s"id must be string, got: $other")
              case None => throw DecodingException("Missing field: id")

            id match
              case "SelectionView" =>
                val rolesJson = fields.getOrElse(
                  "rolesChosen", 
                  throw DecodingException("rolesChosen not found" )
                ) match
                  case Obj(o) => o
                  case (other)  => throw DecodingException(s"rolesChosen must be object, got: $other")

                val rolesChosen: Map[UserId, Role] = 
                  rolesJson.map { (userIdStr, v) =>
                    val roleObj = v.obj
                    val roleStr = roleObj("role").str
                    val team= roleObj("team").num.toInt
                    val role = roleStr match
                      case "Spymaster" => Role.Spymaster(team)
                      case "Operative" => Role.Operative(team)
                      case other => throw DecodingException(s"Unknown role: $other")

                    (userIdStr: UserId) -> role
                  }.toMap
                View.SelectionView(rolesChosen)

              case "SpymasterView" =>
                val allCardsJson =
                  fields.getOrElse("allCards", throw DecodingException("Missing field: allCards"))
                val scoresJson =
                  fields.getOrElse("scores", throw DecodingException("Missing field: scores"))
                val playersJson =
                  fields.getOrElse("players", throw DecodingException("Missing field: scores"))
                val phaseJson =
                  fields.getOrElse("phaseView", throw DecodingException("Missing field: phaseView"))
                val hintJson = fields.get("hint")

                val allCards  = decodeSpymasterCards(allCardsJson)
                val scores = decodeScores(scoresJson)
                val phaseView = decodePhaseView(phaseJson)
                val players = decodePlayers(playersJson)
                val hint = decodeHint(hintJson)
                View.SpymasterView(allCards, scores, hint, players, phaseView)

              case "OperativeView" =>
                val allCardsJson =
                  fields.getOrElse("allCards", throw DecodingException("Missing field: allCards"))
                val flippedJson =
                  fields.getOrElse("flippedCards", throw DecodingException("Missing field: flippedCards"))
                val scoresJson =
                  fields.getOrElse("scores", throw DecodingException("Missing field: scores"))
                val playersJson =
                  fields.getOrElse("players", throw DecodingException("Missing field: scores"))
                val phaseJson =
                  fields.getOrElse("phaseView", throw DecodingException("Missing field: phaseView"))
                val hintJson = fields.get("hint")

                val allCards = decodeOperativeCards(allCardsJson)
                val flippedCards = decodeFlippedCards(flippedJson)
                val scores = decodeScores(scoresJson)
                val players = decodePlayers(playersJson)
                val phaseView = decodePhaseView(phaseJson)
                val hint = decodeHint(hintJson)

                View.OperativeView(allCards, flippedCards, scores, hint, players, phaseView)

              case "EndScreen" =>
                val arr = fields.getOrElse(
                  "winningTeam",
                  throw DecodingException("Missing field: winningTeam")
                ) match
                  case Arr(values) => values
                  case other =>
                    throw DecodingException(s"winningTeam must be array, got: $other")

                val teams: List[Team] = arr.toList.map {
                  case Num(n) =>
                    val t = n.toInt
                    if t.toDouble != n then
                      throw DecodingException(s"Team id must be integer, got: $n")
                    t
                  case other =>
                    throw DecodingException(s"Team id must be numeric, got: $other")
                }

                View.EndScreen(teams)

              case other =>
                throw DecodingException(s"Unknown View id: $other")

          case other =>
            throw DecodingException(s"Expected object for View JSON, got: $other")
      
      }


    /**
      * Encodes a [[PhaseView]] into its JSON representation.
      *
      * JSON format:
      * - `{ "id": "GivingHints" }`
      * - `{ "id": "ChoosingCards" }`
      * - `{ "id": "DisplayingHint" }`
      * - `{ "id": "Waiting" }`
      * - `{ "id": "ViewingFlip", "cardId": <Int> }`
      *
      * @param pv the phase view to encode
      * @return a JSON [[Value]] describing the phase view
    */
    private def encodePhaseView(pv: PhaseView): Value =
      pv match
        case PhaseView.GivingHints =>
          Obj("id" -> Str("GivingHints"))
        case PhaseView.ChoosingCards =>
          Obj("id" -> Str("ChoosingCards"))
        case PhaseView.DisplayingHint =>
          Obj("id" -> Str("DisplayingHint"))
        case PhaseView.Waiting =>
          Obj("id" -> Str("Waiting"))
        case PhaseView.ViewingFlip(cardId) =>
          Obj(
            "id" -> Str("ViewingFlip"),
            "cardId" -> Num(cardId)
          )

    /**
      * Decodes a [[PhaseView]] from its JSON representation.
      *
      * Expects an object containing an `id` field. For `ViewingFlip`, a numeric
      * `cardId` field is also required.
      *
      * @param json the JSON value to decode
      * @throws DecodingException if the JSON structure is invalid or the `id` is unknown
      * @return the decoded [[PhaseView]]
    */
    private def decodePhaseView(json: Value): PhaseView =
      val obj = json.obj
      obj("id").str match
        case "GivingHints"=> PhaseView.GivingHints
        case "ChoosingCards"  => PhaseView.ChoosingCards
        case "DisplayingHint" => PhaseView.DisplayingHint
        case "Waiting" => PhaseView.Waiting
        case "ViewingFlip" =>
          val cardId = obj("cardId").num.toInt
          PhaseView.ViewingFlip(cardId)
        case other =>
          throw DecodingException(s"Unknown phaseView id: $other")


    /**
      * Encodes team scores into JSON.
      *
      * JSON format:
      * ```json
      * {
      * "<teamId>": <score>,
      * ...
      * }
      * ```
      * where `teamId` is serialized as a string and `score` is an integer.
      *
      * @param scores mapping from [[Team]] to its score
      * @return a JSON object containing all scores
    */
    private def encodeScores(scores: Map[Team, Score]): Value =
      Obj.from(
        scores.map { case (team, score) =>
          team.toString -> Num(score)
        }
      )


    /**
      * Decodes team scores from JSON.
      *
      * Keys must be string-encoded integers representing team identifiers.
      * Values must be integers (no fractional values allowed).
      *
      * @param json the JSON value to decode
      * @throws DecodingException if the JSON is not an object, team ids are invalid,
      * or scores are not integers
      * @return a map from team id to score
    */
    private def decodeScores(json: Value): Map[Team, Score] =
      json match
        case Obj(scoreObj) =>
          scoreObj.map { case (k, v) =>
            val team =
              try k.toInt
              catch case _: NumberFormatException =>
                throw DecodingException(s"Team id must be an Int, got: $k")

            val score = v match
              case Num(n) =>
                val s = n.toInt
                if s.toDouble != n then
                  throw DecodingException(s"Score must be integer, got: $n")
                s
              case other =>
                throw DecodingException(s"Score must be numeric, got: $other")

            team -> score
          }.toMap
        case other =>
          throw DecodingException(s"scores must be object, got: $other")


    /**
      * Encodes players into JSON.
      *
      * JSON format:
      * ```json
      * {
      * "<userId>": "<role>",
      * ...
      * }
      * ```
      * where roles are encoded using their `toString` representation.
      *
      * @param players vector of (userId, role) pairs
      * @return a JSON object representing all players
    */    
    private def encodePlayers(players: Vector[(UserId, Role)]): Value =
      Obj.from( 
        players.map { case (userId, role) => 
          userId.toString -> Str(role.toString)
        })


    /**
      * Decodes players from JSON.
      *
      * Expects an object whose keys are user ids (strings) and whose values are
      * valid role string representations, e.g. `Spymaster(1)` or `Operative(2)`.
      *
      * @param json the JSON value to decode
      * @throws DecodingException if a user id or role is invalid
      * @return a vector of decoded (userId, role) pairs
    */
    private def decodePlayers(json : Value): Vector[(UserId, Role)] = 
      json match
        case Obj(playersObj) =>
          playersObj.map { case (k, v) =>
            val userId = try k.toString
              catch case _: Exception =>
                throw DecodingException(s"userId must be a String, got: $k")

            val role = v match
              case Str(n) => n match
                case "Spymaster(1)" => Role.Spymaster(1)
                case "Operative(1)" => Role.Operative(1)
                case "Spymaster(2)" => Role.Spymaster(2)
                case "Operative(2)" => Role.Operative(2)
                case _ => 
                  throw DecodingException(s"role is not valid, got: $n")
              case other => 
                  throw DecodingException(s"invalid json object, got: $other")

            userId -> role
          }.toVector
        case other =>
          throw DecodingException(s"scores must be object, got: $other")

    /**
      * Encodes an optional hint.
      *
      * - `None` is encoded as `null`
      * - `Some(word, n)` is encoded as:
      * `{ "id": "Hint", "word": <String>, "nbOfWords": <Int> }`
      *
      * @param hint optional hint (word, number of words)
      * @return a JSON value representing the hint
    */    
    private def encodeHint(hint: Option[(String, Int)]): Value =
      hint match
        case None => Null
        case Some((word, n)) =>
          Obj(
            "id" -> Str("Hint"),
            "word" -> Str(word),
            "nbOfWords" -> Num(n)
          )
    /**
      * Decodes an optional hint from JSON.
      *
      * Accepts either `null`/missing (decoded as `None`) or a valid hint object.
      *
      * @param jsonOpt optional JSON value
      * @throws DecodingException if required fields are missing or malformed
      * @return an optional (word, number of words) tuple
    */
    private def decodeHint(jsonOpt: Option[Value]): Option[(String, Int)] =
      jsonOpt match
        case None | Some(Null) => None
        case Some(Obj(obj)) if obj("id") == Str("Hint") =>
          val word = obj.get("word") match
            case Some(Str(s)) => s
            case Some(other)  =>
              throw DecodingException(s"hint.word must be string, got: $other")
            case None =>
              throw DecodingException("Missing field: hint.word")

          val n = obj.get("nbOfWords") match
            case Some(Num(v)) =>
              val i = v.toInt
              if i.toDouble != v then
                throw DecodingException(s"hint.nbOfWords must be integer, got: $v")
              i
            case Some(other) =>
              throw DecodingException(s"hint.nbOfWords must be numeric, got: $other")
            case None =>
              throw DecodingException("Missing field: hint.nbOfWords")

          Some((word, n))
        case Some(other) =>
          throw DecodingException(s"hint must be object or null, got: $other")

    /**
      * Encodes spymaster card information into JSON.
      *
      * JSON format:
      * ```json
      * {
      * "<cardId>": {
      * "word": <String>,
      * "cardType": <Int>,
      * "isFlipped": <Boolean>
      * }
      * }
      * ```
      * where `cardType` is encoded using its ordinal value.
      *
      * @param allCards mapping from card id to (word, card type, flipped flag)
      * @return a JSON object representing all cards
    */    
    private def encodeSpymasterCards(
        allCards: Map[CardId, (String, CardType, Flipped)]
    ): Value =
      Obj.from(
        allCards.map { case (cardId, (word, cardType, flipped)) =>
          cardId.toString ->
            Obj(
              "word" -> Str(word),
              "cardType" -> Num(cardType.ordinal),
              "isFlipped" -> Bool(flipped)
            )
        }
      )
    /**
      * Decodes spymaster card information from JSON.
      *
      * Ensures card ids are integers, card types are valid ordinals, and all
      * required fields are present.
      *
      * @param json the JSON value to decode
      * @throws DecodingException if the structure or data types are invalid
      * @return a map from card id to (word, card type, flipped flag)
    */
    private def decodeSpymasterCards(
        json: Value
    ): Map[CardId, (String, CardType, Flipped)] =
      json match
        case Obj(allCardsObj) =>
          allCardsObj.map { case (k, v) =>
            val cardId =
              try k.toInt
              catch case _: NumberFormatException =>
                throw DecodingException(s"CardId must be integer, got: $k")

            val (word, cardType, isFlipped) = v match
              case Obj(cardInfoObj) =>
                val word = cardInfoObj.get("word") match
                  case Some(Str(w)) => w
                  case Some(other)  =>
                    throw DecodingException(s"word must be String, got: $other")
                  case None =>
                    throw DecodingException("Missing field: word")

                val cardType = cardInfoObj.get("cardType") match
                  case Some(Num(ctype)) =>
                    val ord = ctype.toInt
                    if ord.toDouble != ctype then
                      throw DecodingException(
                        s"cardType must be an integer ordinal, got: $ctype"
                      )
                    try CardType.fromOrdinal(ord)
                    catch
                      case _: Throwable =>
                        throw DecodingException(s"Invalid cardType ordinal: $ord")
                  case Some(other) =>
                    throw DecodingException(s"cardType must be numeric, got: $other")
                  case None =>
                    throw DecodingException("Missing field: cardType")

                val isFlipped = cardInfoObj.get("isFlipped") match
                  case Some(Bool(b)) => b
                  case Some(other) =>
                    throw DecodingException(s"isFlipped must be Boolean, got: $other")
                  case None =>
                    throw DecodingException("Missing field: isFlipped")

                (word, cardType, isFlipped)
              case other =>
                throw DecodingException(
                  s"AllCards decoding data type mismatch, got: $other"
                )

            cardId -> (word, cardType, isFlipped)
          }.toMap
        case other =>
          throw DecodingException(s"allCards must be object, got: $other")

    /**
      * Encodes operative-visible cards into JSON.
      *
      * JSON format:
      * ```json
      * { "<cardId>": "<word>" }
      * ```
      *
      * @param allCards mapping from card id to its visible word
      * @return a JSON object representing operative cards
    */
    private def encodeOperativeCards(allCards: Map[CardId, String]): Value =
      Obj.from(
        allCards.map { case (cardId, word) =>
          cardId.toString -> Str(word)
        }
      )
    /**
      * Decodes operative-visible cards from JSON.
      *
      * @param json the JSON value to decode
      * @throws DecodingException if card ids are not integers or values are not strings
      * @return a map from card id to word
    */
    private def decodeOperativeCards(json: Value): Map[CardId, String] =
      json match
        case Obj(obj) =>
          obj.map { case (k, v) =>
            val cardId =
              try k.toInt
              catch case _: NumberFormatException =>
                throw DecodingException(s"CardId must be integer, got: $k")

            val word = v match
              case Str(w) => w
              case other =>
                throw DecodingException(
                  s"Operative allCards values must be strings, got: $other"
                )

            cardId -> word
          }.toMap
        case other =>
          throw DecodingException(s"allCards must be object, got: $other")

    /**
      * Encodes flipped cards into JSON.
      *
      * JSON format:
      * ```json
      * { "<cardId>": <cardTypeOrdinal> }
      * ```
      *
      * @param flipped mapping from card id to its revealed card type
      * @return a JSON object representing flipped cards
    */
    private def encodeFlippedCards(
        flipped: Map[CardId, CardType]
    ): Value =
      Obj.from(
        flipped.map { case (cardId, cardType) =>
          cardId.toString -> Num(cardType.ordinal)
        }
      )


    /**
      * Decodes flipped cards from JSON.
      *
      * Card types must be valid integer ordinals.
      *
      * @param json the JSON value to decode
      * @throws DecodingException if the JSON is malformed or contains invalid ordinals
      * @return a map from card id to card type
    */
    private def decodeFlippedCards(
        json: Value
    ): Map[CardId, CardType] =
      json match
        case Obj(obj) =>
          obj.map { case (k, v) =>
            val cardId =
              try k.toInt
              catch case _: NumberFormatException =>
                throw DecodingException(s"CardId must be integer, got: $k")

            val cardType = v match
              case Num(n) =>
                val ord = n.toInt
                if ord.toDouble != n then
                  throw DecodingException(
                    s"cardType must be integer ordinal, got: $n"
                  )
                try CardType.fromOrdinal(ord)
                catch
                  case _: Throwable =>
                    throw DecodingException(s"Invalid cardType ordinal: $ord")
              case other =>
                throw DecodingException(s"cardType must be numeric, got: $other")

            cardId -> cardType
          }.toMap
        case other =>
          throw DecodingException(s"flippedCards must be object, got: $other")
