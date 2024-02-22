#' Title
#'
#' @param node
#' @param visited
#' @param path
#'
#' @return
#'
#'
#' @examples
#find_cycles <- function(node, visited_nodes, path) {
  # Mark the current node as visited
#  visited_nodes[node] <- TRUE
  # Add the current node to the path
#  path <- c(path, node)

  # Get the neighbors of the current node
#  neighbors <- neighbors(gfl, node, mode = "out")

#  for (neighbor in neighbors) {
#    if (neighbor %in% path) {
      # Found a cycle, add it to the list of feedback loops
#      cycle <- path[which(path == neighbor):length(path)]
#      feedback_loops <<- c(feedback_loops, list(cycle))
#    } else if (!visited_nodes[neighbor]) {
      # Continue DFS from the neighbor node
#      find_cycles(neighbor, visited_nodes, path)
#    }
#  }
#}


