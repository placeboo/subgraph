#' Neighbor
#' Whether two nodes are neighbors in a graph

#' @param node1 One node, numeric
#' @param node2 Another node, numeric
#' @param graph A graph, vector
#' @return TRUE if node1 and node2 are neighbors in the graph
#' @examples
#' is.neighbor(1, 2, c6(1:6)[1, ])

is.neighbor = function(node1, node2, graph){
        lines1.index = grep(node1, graph)
        lines2.index = grep(node2, graph)
        return(!length(intersect(lines1.index, lines2.index)) == 0)
}
