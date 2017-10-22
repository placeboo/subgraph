#' k23 graphs
#' List all possible k23 graphs based on FIVE given nodes. k2 U k3 is the complement of k23

#' @param x The vector representing nodes

#' @return A matrix listing edges of k23 graphs

#' @examples
#' k23(1:5)

k23 = function(x){
        if(length(x)!= 5){
                stop('The number of nodes should be FIVE!')
        }
        x = sort(x)
        k23.prob = 1 - fillprob(x, k2_k3(x))
        return(probToEdges(k23.prob))
}
