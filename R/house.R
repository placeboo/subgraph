#' house graphs
#' List all possible house subgraphs based on given FIVE nodes. Note that house is the complement graph of p5 based on give five nodes.

#' @param x The vector representing nodes

#' @return A matrix listing edges of house graphs

#' @examples
#' house(c(1:5))
house = function(x){
        if(length(x) != 5){
                stop('The number of nodes should be FIVE!')
        }
        x = sort(x)
        p5.mat = p5(x)
        house_prob.mat = 1 - fillprob(x, p5.mat)
        return(probToEdges(house_prob.mat))
}
