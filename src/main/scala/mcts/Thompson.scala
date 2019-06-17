package mcts

import util.Util

object Thompson {
  def search(rootState: GameState,
             maxIterations: Int,
             verbose: Boolean = false): Int = {
    val rootNode: GameNode = GameNode(state = rootState)

    var node: GameNode = null
    var state: GameState = null
    for (_ <- 1 to maxIterations) {
      node = rootNode
      state = rootState.getCopy

      // SELECTION phase of MCTS
      // ------------------------------------------------------
      while (node.untriedActions.nonEmpty && node.children.nonEmpty) {
        // Node has no unexplored actions and node has children nodes.
        node = node.selectChild(thompsonSample = true)
        state.doAction(node.action)
      }

      // EXPANSION phase of MCTS
      // -------------------------------------------------------
      // First, choose a random action from list of untried actions of the node
      // Second, apply it to the state.
      // Third, record this action and the resultant state as a child of the node.
      if (node.untriedActions.nonEmpty) {
        val action = Util.random(node.untriedActions)
        state.doAction(action)
        node.addChild(action, state)
      }

      // SIMULATION phase of MCTS
      // -------------------------------------------------------
      // First, choose a random action from list of available actions of the state
      // Second, apply it to the state.
      while (state.getAvailableActions.nonEmpty) {
        state.doAction(Util.random(state.getAvailableActions))
      }

      // BACKPROPAGATION phase of MCTS
      // ------------------------------------------------------------
      while (node != null) {
        node.update(state.getResult(node.playerIndex))
        node = node.parent
      }
    }

    if (verbose) {
      println(rootNode.treeToString(0))
    } else {
      println(rootNode.childrenToString())
    }

    rootNode.children.maxBy(_.numberOfVisits).action
  }
}
