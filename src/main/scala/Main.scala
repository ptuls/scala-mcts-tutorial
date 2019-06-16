import game.{HexState, OXOState}
import mcts.UCT

import scala.util.Random

/**
  * Created by culim on 2/24/16.
  */
object Main extends App {

  // Choose a game, or leave at default.
  // -----------------------------------
  // Default to the OXO game.
  var state = new OXOState

  // Uncomment for a 7 x 7 Hex Board
  // var state = new HexState(7, 7)

  // --

  while (state.getAvailableActions.nonEmpty) {

    println(
      s"Player ${state.totalNumberOfPlayers + 1 - state.getLastPlayerWhoMoved}'s turn.")
    println(state.toString)

    var action: Int = -1
    if (state.getLastPlayerWhoMoved == 1) {
      // Now it is player 2's turn.
      action = UCT.search(state, 500)
    } else {
      // Now it is player 1's turn.
      action = UCT.search(state, 500)
    }

    println(
      s"Player ${state.totalNumberOfPlayers + 1 - state.getLastPlayerWhoMoved}'s best action is $action")
    println()
    state.doAction(action)

  }

  println(state.toString)

  state.getResult(state.lastPlayerWhoMoved) match {
    case 1.0 => println(s"Aha! Player ${state.lastPlayerWhoMoved} wins!")
    case 0.0 =>
      println(
        s"Hmm, Player ${state.totalNumberOfPlayers + 1 - state.lastPlayerWhoMoved} wins!")
    case _ => println(s"It's a draw!")
  }
}
