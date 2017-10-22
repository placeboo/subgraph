#' Find the Node
#' List a matrix to show the nodes in the graphs according to the times of being connected

#' @param times A numeric shows the times of nodes which have been connected
#' @param graph A vector or matrix shows the graphs. If the graph is a matrix, the nodes of these graphs should be the same

#' @examples
#' findNode(1, p3(1:3))
#' findNode(2, p3(1:3))

findNode = function(times, graph){
        if(is.vector(graph)){
                graph = t(as.matrix(graph))
        }
        temp.mat = t(apply(graph, 1, function(y)  as.numeric(unlist(strsplit(y, '-')))))
        # all nodes in the graph
        allNodes = sort(unique(temp.mat[1, ]))
        Node =  apply(temp.mat, 1, function(y) allNodes[table(y) == times])
        if(is.vector(Node)){
                return(Node)
        }
        return(t(Node))
}
