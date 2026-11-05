package apps
package ul2025app29
import Event.*
import cs214.webapp.* 
import cs214.webapp.client.* 
import cs214.webapp.client.graphics.WebClientAppInstance 
import org.scalajs.dom 
import scalatags.JsDom.all.* 
import scala.scalajs.js.annotation.JSExportTopLevel // The view - just data to display 
import scala.collection.immutable.SortedMap
import org.scalajs.dom.HTMLButtonElement
import PhaseView.*
import apps.ul2025app29.Role.*
import scala.collection.mutable.ListBuffer
import scala.caps.use
import apps.ul2025app29.Phase.EndScreen

@JSExportTopLevel("ul2025app29") 
object CodenamesUI extends WSClientApp: 
  def appId: String = "ul2025app29" 
  def uiId: String = "html" 
  def init(userId: UserId, sendMessage: ujson.Value => Unit, target: dom.Element): ClientAppInstance = 
    CodenamesUIInstance(userId, sendMessage, target) 
class CodenamesUIInstance(userId: UserId, sendMessage: ujson.Value => Unit, target: dom.Element) 
    extends WebClientAppInstance[Event, View](userId, sendMessage, target):
  val allRoles =List(Role.Spymaster(1),Role.Spymaster(2),Role.Operative(1),Role.Operative(2))
  var choseCard:Option[CardId]=None
  
  override val wire = ul2025app29.Wire

  //ids must be unique change them to cls
  override def render(userId: UserId, view: View): Frag = 
    choseCard=None
    view match
        case View.SelectionView(rolesChosen) => renderChoosingRoles(rolesChosen)
        case View.SpymasterView(allCards, scores, hint,vector, phaseView) => renderPlayerView(userId,allCards, scores, hint, phaseView,vector)
        case View.OperativeView(allCards, flippedCards, scores, hint,vector ,phaseView) =>{
          val fcards= allCards.map((k,v)=>{
            val pot=flippedCards.get(k)
            if pot.isDefined then (k,(v,pot.get,true)) else (k,(v,CardType.Neutral,false))
          })
          renderPlayerView(userId,fcards, scores, hint, phaseView,vector)
        }
        case View.EndScreen(winningTeam) => renderEndScreenView(winningTeam)

  /**
    * this method takes the list of winning teams and renders the ending view of the game indication who won the game
    *
    * @param winningTeams the list of winning teams as a tie is possible
    * @return the graphical elements displaying the message of the winning teams
    */
  private def renderEndScreenView(winningTeams:List[Int]):Frag=
    val output= if winningTeams.length ==2 then "it's a Tie" else s"Team ${winningTeams.head} has won" 
    val color = if winningTeams.length ==2 then "#0f0101ff" else if winningTeams.head==1 then "#2c7be5" else "#e1320b"
    frag(div(cls:="end-screen",div(cls:="end-screen-element",style:=s" --static-color: ${color};",output),button("play another round")))

  /** 
   * a small helper function to reduce the repetion of code.
   * it plays the sound of the path @param path and then sends and event
   * @param path a path to a sound file
   */
  private def onClicked(path:String,event: => Unit):Unit=
    val sound=audio(src:= path,`type`:="audio/mpeg").render;sound.play();sound.onended=((_: dom.Event) => event)
  /**
    * a method to render the view of each player while they are still choosing their roles 
    * 
    * @param roles a map of all the userIds and their respective roles
    * @return UI of the PhaseView SelectionView
    */
  private def renderChoosingRoles(roles:Map[UserId,Role]):Frag=
    val inverse= roles.map((k,v)=> (v,k))
    
    val el1 = Seq(
      {
        val role = Spymaster(1)
        val text = inverse.getOrElse(role, "Spymaster 1")
        if roles.values.toList.contains(role) then
          button(cls:="shadow1", text)
        else
          button(cls:="role",onclick:= ((_: dom.Event) => onClicked("/static/Button_Click_Sound_Effect.mp3",sendEvent(RoleChosen(role)))), text)
      },
      {
        val role = Operative(1)
        val text = inverse.getOrElse(role, "Operative 1")
        if roles.values.toList.contains(role) then
          button(cls:="shadow1", text)
        else
          button(cls:="role",onclick:=((_: dom.Event) => onClicked("/static/Button_Click_Sound_Effect.mp3",sendEvent(RoleChosen(role)))), text)
      }
    )
    
    val el2 = Seq(
      {
        val role = Spymaster(2)
        val text = inverse.getOrElse(role, "Spymaster 2")
        if roles.values.toList.contains(role) then
          button(cls:="shadow2", text)
        else
          button(cls:="role",onclick:=((_: dom.Event) => onClicked("/static/Button_Click_Sound_Effect.mp3",sendEvent(RoleChosen(role)))), text)
      },
      {
        val role = Operative(2)
        val text = inverse.getOrElse(role, "Operative 2")
        if roles.values.toList.contains(role) then
          button(cls:="shadow2", text)
        else
          button(cls:="role",onclick:= ((_: dom.Event) => onClicked("/static/Button_Click_Sound_Effect.mp3",sendEvent(RoleChosen(role)))), text)
      }
    )
    val role=roles.get(userId)
    val text= if role.isEmpty then "Choose a Role" else 
      s"You choose to play as ${role.get.toString().dropRight(3)} of team ${role.get.toString().takeRight(2).head}"
    div(
      div(
      div(id:="message",div(text)),
      div(id:="allRoles",
        div(cls:="Team",el1),
        div(cls:="Team",el2))
      )
  )
  /**
    * an important function using the majority of all function to render the view of the player,
    * doesn't matter whether he is spymaster or operative they have the same method 
    * @param userId UserId
    * @param allCards all the cards after being processed for either spymaster or operative
    * @param scores the scores of each team
    * @param hint the hint that has been sent ,None othewise
    * @param phaseView the current phaseView of the userId
    * @param players all the players in the game
    * @return a graphical element drawing the whole view of each userId during the game
    */
  private def renderPlayerView(
    userId:UserId,
    allCards: Map[Int, (String, CardType, Boolean)],
    scores: Map[Int, Int],
    hint: Option[(String, Int)],
    phaseView: PhaseView,
    players:Vector[(UserId, Role)]
    ): Frag = 
      val mapOfPlayers=players.toMap
      frag(div(
        cls := "player-view-container",
        popCard(phaseView,allCards,mapOfPlayers.get(userId).get),
        div(cls:="left-column",renderTeams(mapOfPlayers,userId),
        renderCards(allCards,phaseView)),
        div(cls:="right-column",renderScores(scores),renderPhaseView(userId,phaseView,hint,players.head._2.team),
          renderChooseCards(phaseView),renderMaybeSkip(phaseView),renderOptionHint(phaseView,hint)
        )
      ))
  /**
   * a method called when a card is revealed to create an animation while revealing it
   * @param phaseView PhaseView of the userId
   * @param allCards all the cards processed depending on either it is a spymaster or operative
   * @param role the role of the userId
   * @return an animation of the card being flipped during the phaseView ViewingFlip
   */
  private def popCard(phaseView:PhaseView,allCards:Map[Int, (String, CardType, Boolean)],role:Role)= phaseView match
    case ViewingFlip(cardId) => div(cls:="fog-overlay",growingCard(allCards.get(cardId).get._2,allCards.get(cardId).get._1))
    case _ => div


  /**
    * a helper function to pattern/match on the card type and return the right div for the method popCard
    *
    * @param cardType the color/type of the card
    * @param cardName the name shown on the card
    * @return a HTMLDivElement of the card to be revealed 
    */
  private def growingCard(cardType:CardType,cardName:String)= cardType match
    case CardType.Blue     => div(cls := "pop", style := s"--pop-color: ${colorFor(CardType.Blue)};", cardName)
    case CardType.Red      => div(cls := "pop", style := s"--pop-color: ${colorFor(CardType.Red)};", cardName)
    case CardType.Neutral  => div(cls := "pop", style := s"--pop-color: ${colorFor(CardType.Neutral)};", cardName)
    case CardType.Assassin => div(cls := "pop", style := s"--pop-color: ${colorFor(CardType.Assassin)};", cardName)
  
  /**
   * a small helper function used to translate the Type of a card to its corresponding color
   * 
   * @param ct CardType
   * @return the string code of the color corresponding to the Type of the Card
   */
  private def colorFor(ct: CardType): String = ct match 
    case CardType.Blue    => "#2c7be5"
    case CardType.Red     => "#e1320b"
    case CardType.Neutral => "#ffffffff"
    case CardType.Assassin=> "#000000"
  
  /**
   * a method that changes the state of the choose button,for the playing operative, whenver a card is clicked or disclicked
   */
  private def syncChooseButton(): Unit =
    val btn = dom.document.getElementById("choose-btn").asInstanceOf[org.scalajs.dom.html.Button]
    if btn != null then
      val active = choseCard.nonEmpty
      btn.setAttribute("data-active", if active then "true" else "false")
  /**
   * a method used to send events when the operative had chosen a card and wants to send an event
   * @param phaseView the phaseView of the player
   * @return a button or an empty HTMLDivElement
   */
  private def renderChooseCards(phaseView:PhaseView)=
    if phaseView==PhaseView.ChoosingCards then 
      button(id:="choose-btn",cls := "choose-btn",attr("data-active") :="false",
        onclick:=((_: dom.Event) => if choseCard.isDefined then 
              onClicked("/static/Drum_Waiting_Sound_Effect.mp3",sendEvent(Event.CardClicked(choseCard.get)))
        ),"Choose"
      ) 
    else div()
  
  /** a helper function used to draw the graphical elemnts showing the score of each team
   * 
   * @param scores a map containing the score of each team
   * @return a HTMLElement containing the scores
   */
  private def renderScores(scores:Map[Int,Int])=
    div(
      id:="Scores",
      div("Scores",fontSize:=30,fontWeight:=1000,textAlign:="center"),
      div(cls:="oneScore1",div("Team 1:  ",fontWeight:=700 ),div(scores.get(1).get.toString) ),
      div(cls:="oneScore2",div("Team 2:  ",fontWeight:=700 ),div(scores.get(2).get.toString))
    )
  /**
    * a helper function to inform each player whether it is his role to play or he has to wait for his turn
    * and during the phaseView DisplayingHint an animation showing the hint is displaying on the screen above 
    * every other graphical element
    * @param userId
    * @param phaseView
    * @param hint
    * @param team
    * @return a part of of the UI layout informing each palyer what should do 
    */
  private def renderPhaseView(userId:UserId,phaseView:PhaseView,hint:Option[(String,Int)],team:Int)=phaseView match
    case DisplayingHint => div(cls := "fog-overlay",div(cls := "end-message-box",
                           div(s"Hint:    ${hint.get._1}"),div(s"Number:    ${hint.get._2}")))

    case GivingHints => div(cls:="renderPhaseView",div(s"${userId}-sama give the hints please"))

    case ChoosingCards => div(cls:="renderPhaseView",div(s"${userId}-sama it's your turn to choose cards"))

    case ViewingFlip(cardId) =>  div(cls:="renderPhaseView",div(s"${userId}-sama,Let's reveal the ${cardId} card"))

    case _ => div(cls:="renderPhaseView",div(s"wait ${userId}-sama ,It's not your turn yet"))
  
  /**
    * creates a button when clicked the operative confirms he has finished playing and an event 
    * to give the other team the role to play
    * @param phaseView
    * @return a button enabling skipping the role
    */  
  private def renderMaybeSkip(phaseView:PhaseView)=
    if phaseView==PhaseView.ChoosingCards then button(cls:="skip",onclick:=((_: dom.Event) => sendEvent(Event.SkippedTurn)),"Skip") else div()

  /**
    *  a method to draw the userIds of every single player with every player has his name bouncing
    *
    * @param players all the players of the game
    * @param userId
    * @return a HTMLDivElement to draw the userIds of the players
    */
  private def renderTeams(players:Map[UserId,Role],userId:UserId) =
    def index(role:Role):Int= role match
      case Spymaster(team) => team-1
      case Operative(team) => team+1
    val inverse=players.map((k,v)=>(v,k))
    val userIndex=index(players.get(userId).get)
    val list=allRoles.zipWithIndex.
        map((role,i) => if i==userIndex then div(cls:=s"Team_${role.team}_element glow",div(inverse.get(role).get)) 
        else div(cls:=s"Team_${role.team}_element",div(inverse.get(role).get) )
      ).toList
    div(
      id:="Teams",
      div(
        id:="Team_1",
        list.apply(0),
        list.apply(2),
      )
      ,div(
        id:="Team_2",
        list.apply(1),
        list.apply(3)
      )
    )
  /**
    * an important method to draws all the cards of the game,in a way that a flipped card no longer appears(its name) 
    * and the other ones are available to click and choose
    * @param allCards
    * @param phaseView
    * @return a HTMLDivElement of all the cards of the game 
    */
  private def renderCards(
    allCards: Map[CardId, (String, CardType, Flipped)],
    phaseView: PhaseView
  ) =
    val cards = (0 until allCards.size).map { cardId =>
      val (cardName, cardType, flipped) = allCards(cardId)
      val inputId = s"card-$cardId"

      val lbl =
        label(
          cls := s"card ${cssClass(cardType)}" + (if choseCard.contains(cardId) then " selected" else ""),
          `for` := inputId,
          (if !flipped then cardName else ""),
          onclick := ((_: dom.Event) => {
            if phaseView == PhaseView.ChoosingCards && !flipped then
              if choseCard.contains(cardId) then
                // toggle off
                choseCard = None
              else
                // select this card, deselect previous
                choseCard = Some(cardId)
              updateCardClasses()
              syncChooseButton()
          })
        )
      val inputi = if flipped then input(`type` := "checkbox", id := inputId, hidden := true, disabled := true) 
      else input(`type` := "checkbox", id := inputId, hidden := true)
      div(
        id := s"card-box-$cardId",
        // we can keep the input for form semantics if you want
        inputi,
        lbl
      )
    }.toList

    div(id := "cards", cards)

  /**
    * a helper method used in renderCards to update the cards such that
    * only one card is shown selected
    */
  private def updateCardClasses(): Unit =
  choseCard match
    case Some(id) =>
      // remove 'selected' from all other cards
      (0 until 25).foreach { cardId =>
        val lbl = dom.document.getElementById(s"card-box-$cardId").querySelector("label")
        if lbl != null then
          if cardId == id then lbl.classList.add("selected")
          else lbl.classList.remove("selected")
      }
    case None =>
      // no card selected → remove 'selected' from all
      (0 until 25).foreach { cardId =>
        val lbl = dom.document.getElementById(s"card-box-$cardId").querySelector("label")
        if lbl != null then lbl.classList.remove("selected")
      }
  /**
    * a small helper function used in renderCards to return the css class already created
    * depending on the type/color of the card
    * @param cardType
    * @return the css of the card 
    */
  private def cssClass(cardType:CardType):String= cardType match
    case CardType.Blue => "blue_card"
    case CardType.Red => "red_card"
    case CardType.Neutral => "neutral_card"
    case CardType.Assassin => "assassin_card"
  /**
    * a function used to give the possibility to the spymaster to give a hint 
    * or displays the hint to be solved if it exists
    * @param phaseView the PhaseView
    * @param hint a hint if exts
    * @return
    */
  private def renderOptionHint(phaseView:PhaseView,hint: Option[(String, Int)])=
    div(cls:="buttom",div(phaseView match
      case PhaseView.GivingHints => renderGivingHints
      case _ => div())
    ,(if hint.nonEmpty then div(cls:="hint",div(s"${hint.get._1}    ${hint.get._2}")) else div())  )


  /**
    * a method that creates two files for the hint and the number of hints and a button 
    * that listens on the two fields and when clicked send an event Hint with the content of the two fields
    * @return two fields and a button when clicked sends the event Hint
    */
  private def renderGivingHints =
    val input1 = input(`type` := "text", placeholder := "hint").render
    val input2 = input(`type` := "number", min := "1", value := "1", placeholder := "#words").render
    val sendBtn = button("send").render

    sendBtn.addEventListener("click", (_: dom.Event) =>
      val hint = input1.value.trim
      val nb = try input2.value.toInt catch case _: Throwable => 1
      sendEvent(Event.Hint(hint, nb))
    )

    div(input1, input2, sendBtn)
  
  override def css: String = super.css +
  """ | .skip{
      |   border-radius: 20px;
      |   background-color: #878181ff;
      |   transition: transform 0.2s ease, box-shadow 0.2s ease;
      |}
      |.skip:hover {
      |   transform: scale(1.05);
      |   box-shadow: 0 6px 18px rgba(0, 0, 0, 0.12);
      |}
      |.choose-btn[data-active="true"] {
      |  animation: pulseGlow 1.3s ease-in-out infinite;
      |  background: linear-gradient(135deg, #f59e0b, #ef4444);
      |  filter: brightness(1.12);
      |  box-shadow: 0 0 0 4px rgba(255,215,0,0.45), 0 10px 26px rgba(255,140,0,0.6);
      |  border-radius: 20px;
      |}
      |
      |.choose-btn[data-active="false"] {
      |  border-radius: 20px;
      |  background: #565654ff;
      |  animation: none;
      |  filter: none;
      |  box-shadow: none;
      |  cursor: not-allowed;
      |}
      |.glow {
      |  animation: pulseGlow 1.6s ease-in-out infinite;
      |  border-radius: 8px;
      |}
      |@keyframes pulseGlow {
      |  0%   { box-shadow: 0 6px 12px rgba(171, 7, 133, 0.2); transform: translateY(0); }
      |  50%  { box-shadow: 0 12px 30px rgba(225, 11, 179, 0.45); transform: translateY(-4px); }
      |  100% { box-shadow: 0 6px 12px rgba(211, 11, 225, 0.2); transform: translateY(0); }
      |}
      |#cards {
      |  display: grid;
      |  grid-template-columns: repeat(auto-fit, minmax(100px, 1fr));
      |  gap: 10px;
      |  width: 600px; 
      |  padding-right: 20px;
      |}
      |.card {
      |  width: 100%;
      |  max-width: 120px;
      |  display: inline-flex;
      |  align-items: center;
      |  justify-content: center;
      |  padding: 12px 18px;
      |  margin: 8px;
      |  border-radius: 10px;
      |  background: #fff;
      |  border: 2px solid rgba(0,0,0,0.06);
      |  cursor: pointer;
      |  transition: transform 0.16s ease, box-shadow 0.16s ease, border-color 0.16s ease;
      |  position: relative;
      |}
      |
      |/* use the "selected" class instead of :checked */
      |.card.selected {
      |  transform: translateY(-6px) scale(1.03);
      |  border-color: gold;
      |  box-shadow: 0 16px 30px rgba(243,156,18,0.25);
      |}
      |
      | 
      | .pop {
      |  width: 150px;
      |  height: 150px;
      |  border-radius: 50%;
      |  display: flex;
      |  align-items: center;
      |  justify-content: center;
      |  color: white;
      |  font-weight: 700;
      |  animation: pop-grow 3s ease-out forwards;
      |  background-color: var(--pop-color, #ffffffff);
      |  z-index: 1000;
      |}
      |
      |@keyframes pop-grow {
      |  0%   { transform: scale(.3);  background-color: #ffffffff; opacity: 1; }
      |  50%  { transform: scale(1.4); background-color: color-mix(in srgb, orange 60%, var(--pop-color) 40%);opacity: 1; }
      |  100% { transform: scale(2);   background-color: var(--pop-color); opacity: 1; }
      |}
      |.fog-overlay {
      |  position: fixed;        
      |  top: 0;
      |  left: 0;
      |  width: 100%;
      |  height: 100%;
      |  background: rgba(0, 0, 0, 0.4);  
      |  backdrop-filter: blur(4px);      
      |  display: flex;
      |  justify-content: center;
      |  align-items: center;            
      |  z-index: 9999;                   
      |}
      |.end-message-box {
      |  background: white;
      |  padding: 20px 30px;
      |  border-radius: 40px;
      |  border: 5px solid black;
      |  box-shadow: 0 4px 20px rgba(0,0,0,0.2);
      |  box-width: 5px;
      |  font-size: 70px;
      |  color:  #090808ff;
      |  font-weight: 700;
      |  text-align: center;
      |  min-width: 250px;
      |  animation: popScale 1s ease-in-out infinite alternate;
      |  transform-origin: center center;
      |}
      |@keyframes popScale {
      |  0%   { transform: scale(0.9); color: #090808ff; }
      |  50%  { transform: scale(1.1); color: #090808ff; }
      |  100% { transform: scale(0.95); color: #090808ff; }
      |}
      | .renderPhaseView{
      |   font-weight: 700;
      |   background-color: #7c6661ff;
      |   border-radius: 20px;
      |   border-width: 1rem;
      |   border-color: #6a6262ff;
      |   display: inline-block;
      |   text-align: center;
      |   padding: 5px 5px;
      |}
      | .player-view-container {
      |   display: flex; 
      |   gap: 1rem; 
      |   padding: 1rem; 
      |   width: 800px;
      |   box-sizing: border-box; 
      |}
      | .left-column {
      |   min-width: 250px; 
      |   max-width: 600px;
      |   flex-shrink: 0; 
      |   display: flex;
      |   flex-direction: column; 
      |   gap: 50px; 
      |}
      | .right-column {
      |   width: 200px;
      |   flex: 1; 
      |   justify-content: flex-end; 
      |   display: flex;
      |   flex-direction: column; 
      |   gap: 20px; 
      |}
      | .your_turn{
      |    display: flex;
      |    justify-content: center;
      |    border-radius: 20px;
      |    border-width: 1rem;
      |    font-size: 20px;
      |    font-weight: 700;
      |}
      | .role{
      |    border-radius: 20px;
      |    width: 200px; 
      |    padding: 10px; 
      |}
      | .role:hover {
      |    transform: scale(1.1); 
      |    transition: transform 0.3s ease;
      |}
      |.end-screen {
      |  display: flex;
      |  flex-direction: column;   /* vertical order */
      |  align-items: center;      /* horizontal centering */
      |  justify-content: center;  /* vertical centering (optional) */
      |  gap: 24px;                /* space between div and button */
      |}
      | .end-screen-element{
      |    max-width: 100%;
      |    text-align: center;
      |    font-size: 200px;
      |    font-weight: 700;
      |}
      | .hint{
      |    display: flex;
      |    gap: 10px;
      |    flex-wrap: wrap;
      |}
      | .shadow1{
      |    box-shadow: 0 6px 12px rgba(95, 46, 46, 0.25);
      |    transform: translateY(-2px);
      |    background: linear-gradient(120deg, #4a6cf7, #6f8bff);
      |    border-radius: 20px;
      |    width: 200px; 
      |    padding: 10px; 
      |    cursor: not-allowed;
      |}
      | .shadow2{
      |    box-shadow: 0 6px 12px rgba(37, 31, 84, 0.25);
      |    transform: translateY(-2px);
      |    background: linear-gradient(120deg, #ff0000, #ff7f7f);
      |    border-radius: 20px;
      |    width: 200px; 
      |    padding: 10px;
      |    cursor: not-allowed; 
      |}
      | #message{
      |     font-weight: 700;
      |     text-align: center;
      |     background-color: #ede3a9ff;
      |     border-radius: 20px;
      |     border-width:10px;
      |     display: inline-block; /* Make the element adjust width to text */
      |     padding: 10px 100px;
      |}
      | #choosingView{
      |    display: flex; /* Activates flexbox layout */
      |    flex-direction: row; /* Default direction is row, which arranges items horizontally */
      |    gap: 10px; 
      |}
      | #allRoles{
      |    display: flex;
      |    gap: 4rem; /* space between team columns */
      |    margin: 2rem; /* reduce huge margin */
      |    
      |}
      | .Team{
      |    display: flex;
      |    align-items: center;
      |    flex-direction: column;
      |    gap: 0.5rem;
      |}
      | #Teams {
      |    padding-top: 30px;
      |    width: 100%;
      |    height: 80px;
      |    display: flex;
      |    align-items: center;
      |    justify-content: center;
      |    border-radius: 0.5rem;
      |    gap: 5rem;
      |}
      | #Team_1 {
      |    display: flex;
      |    flex-direction: column;
      |    align-items: center;
      |    justify-content: center;
      |    gap: 20px;
      | }
      | .Team_1_element{
      |    background: linear-gradient(120deg, #4a6cf7, #6f8bff);
      |    text-align: center;
      |    align-items: center;
      |    font-size: 20px;
      |    border-radius: 1rem;
      |    width: 150px;
      |    height: 40px;
      |}
      | #Team_2 {
      |    display: flex;
      |    flex-direction: column;
      |    align-items: center;
      |    justify-content: center;
      |    gap: 20px;
      | }
      | .Team_2_element{
      |    background: linear-gradient(120deg, #ff0000, #ff7f7f);
      |    text-align: center;
      |    align-items: center;
      |    font-size: 20px;
      |    border-radius: 1rem;
      |    width: 150px;
      |    height: 40px;
      |}
      | #Scores{
      |    display: flex;
      |    flex-direction: column;
      |    align-items: center;
      |    text-align: center;
      |    justify-content: center;
      |    background: #ece6eeff;
      |    border-radius: 20px;
      |    padding: 10px 10px;
      |    padding-bottom: 20px;
      |    gap: 15px;
      | }
      | .oneScore1{
      |    background: linear-gradient(120deg, #4a6cf7, #6f8bff);
      |    width: 100%;
      |    font-size: 20px;
      |    display: flex;
      |    flex-direction: row;
      |    box-shadow: 0 6px 12px rgba(110, 226, 246, 0.97);
      |    border-radius: 20px;
      |    padding-left: 5px;
      |}
      | .oneScore2{
      |    background: linear-gradient(120deg, #ff0000, #ff7f7f);
      |    width: 100%;
      |    font-size: 20px;
      |    display: flex;
      |    flex-direction: row;
      |    box-shadow: 0 6px 12px rgba(246, 110, 110, 0.97);
      |    border-radius: 20px;
      |    padding-left: 5px;
      |}
      | #SolvingHints {
      |    align-items: flex-start;
      |    }
      | .buttom{
      |    height: 50px
      |    display: flex;
      |    column-gap: 20px;
      |    justify-content: center;
      |    align-items: center;
      |}
      | .blue_card{
      |    color: #ffffffff;
      |    font-size: 20px;
      |    background: #5c83cbff;
      |    width: 7rem;
      |    height: 6rem;
      |    border-radius: 20px;
      |}
      | .red_card{
      |    color: #ffffffff;
      |    font-size: 20px;
      |    background: #c35454ff;
      |    width: 7rem;
      |    height: 6rem;
      |    border-radius: 20px;
      |}
      | .neutral_card{
      |    color: #ffffffff;
      |    font-size: 20px;
      |    background: #c3b091;
      |    width: 7rem;
      |    height: 6rem;
      |    border-radius: 20px;
      |}
      | .assassin_card{
      |    color: #ffffffff;
      |    font-size: 20px;
      |    background: #000000ff;
      |    width: 7rem;
      |    height: 6rem;
      |    border-radius: 20px;
      |}
      |.hint {
      |  min-width: 120px;
      |  padding: 16px;
      |  display: flex;
      |  flex-direction: column;
      |  align-items: center;
      |  justify-content: center;
      |  gap: 12px;
      |  background: linear-gradient(135deg, #1a1a2e, #2c3e50); /* gradient */
      |  border-radius: 12px;
      |  border: 1px solid rgba(255, 255, 255, 0.1);
      |  position: relative;
      |  overflow: hidden;
      |  color: white; /* content color */
      |}
      |/* Stars overlay using multiple tiny dots */
      |.hint::before {
      |  content: "";
      |  position: absolute;
      |  top: 0; left: 0;
      |  width: 100%; height: 100%;
      |  pointer-events: none;
      |  z-index: 0; /* behind content */
      |  background-image: radial-gradient(circle, #fff 1px, transparent 1px),
      |                    radial-gradient(circle, #ffe3cc 1px, transparent 1px),
      |                    radial-gradient(circle, #ccf0ff 1px, transparent 1px),
      |                    radial-gradient(circle, #ffdcdc 1px, transparent 1px);
      |  background-size: 20px 20px, 30px 30px, 25px 25px, 35px 35px;
      |  background-position: 0 0, 10px 5px, 5px 15px, 15px 20px;
      |  animation: twinkle 2s infinite alternate;
      |}
      |/* Twinkle effect */
      |@keyframes twinkle {
      |  0%   { opacity: 0.3; }
      |  50%  { opacity: 1; }
      |  100% { opacity: 0.3; }
      |}
      """.stripMargin