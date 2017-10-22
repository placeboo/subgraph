#' Neighbor
#' For specific node in one graph, list all the nodes whcih are connected to the node.

#' @param node The specific node in the graph, representing as a numeric.
#' @param graph The graph, representing as a vector. Each entry in the vector is writen such as "1-2", "6-2".

#' @return A numeric vector listing all the nodes which are connected to the nodex x in graph y.

#' @examples
#' graph = c6(1:6)[1, ]
#' neighbor(2, graph)
neighbor = function(node, graph){
        lines = graph[grep(node, graph)]
        allNodes = as.numeric(unlist(strsplit(lines, '-')))
        neighbor = as.vector(allNodes[! allNodes %in% node])
        return(neighbor)
}
