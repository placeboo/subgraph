#' butterfly graphs
#' List all possible butterfly subgraphs based on given FIVE nodes. Note that butterfly is the complement graph of co-butterfly based on give five nodes.

#' @param x The vector representing nodes

#' @return A matrix listing edges of butterfly graphs

#' @examples
#' butterfly(c(1:5))

butterfly = function(x){
        if(length(x) != 5){
                stop('The number of nodes should be FIVE!')
        }
        x = sort(x)
        co_butterfly.mat = co.butterfly(x)
        butterfly_prob.mat = 1 - fillprob(x, co_butterfly.mat)
        return(probToEdges(butterfly_prob.mat))
}
