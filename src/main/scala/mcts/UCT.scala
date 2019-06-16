package mcts

import util.Util

/**
  * Upper confidence bound applied to trees (UCT). Given child node v_i of a node
  * v, the UCT is computed as
  *
  * UCT(v_i, v) = Q(v_i) / N(v_i) + c sqrt(log(N(v)) / N(v_i))
  *
  * where Q(v_i) is the total simulation reward of v_i, N(v_i) is the number of
  * times v_i was visited, and c is the parameter that weights the second component,
  * or the exploration component. The first component Q(v_i) / N(v_i) is the exploitation
  * component.
  *
  * Note that
  * 1. N(v_i) = 0 results in UCT equal to infinity, so that all child nodes must be
  * expanded,
  * 2. Q(v_i) / N(v_i) must lie within [0, 1].
  */
object UCT {

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
        node = node.selectChild
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
