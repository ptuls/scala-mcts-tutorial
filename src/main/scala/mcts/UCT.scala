package mcts

import scala.util.Random

/**
  * Created by culim on 2/24/16.
  */
object UCT  {

    def search(rootState : GameState, maxIterations : Int, verbose : Boolean = false) : Int = {
        val rootNode : GameNode = new GameNode(state = rootState)

        var node : GameNode = null;
        var state : GameState = null;
        for (iteration <- 1 to maxIterations) {
            node = rootNode
            state = rootState.getCopy
            
            
            // Step (1) - Selection phase of MCTS
            // ------------------------------------------------------
            while (isNodeWithoutUntriedActions && isNodeAParent) {
                // Node has no unexplored actions and node has children nodes.
                node = node.selectChild
                state.doAction(node.action)
            }

            // Step (2) Expansion phase of MCTS
            // -------------------------------------------------------
            if (node.untriedActions.nonEmpty) {
                // TODO: Implement the following three steps
                // First, choose a random action from list of untried actions of the node
                // Second, apply it to the state.
                // Third, record this action and the resultant state as a child of the node.
            }

            // Step (3) - Simulation phase of MCTS
            // -------------------------------------------------------
            while (state.getAvailableActions.nonEmpty) {
                // TODO: Implement the following two steps
                // First, choose a random action from list of available actions of the state
                // Second,apply it to the state.
            }

            // Step (4) - Backpropagation phase of MCTS
            // ------------------------------------------------------------
            while (node != null) {
                node.update(state.getResult(node.playerIndex))
                node = node.parent
            }

        }

        if (verbose) {
            println(rootNode.treeToString(0))
        }
        else {
            println(rootNode.childrenToString());
        }

        return rootNode.children.sortBy(_.numberOfVisits).last.action
    }

}
