#' p2_p3.bar graphs
#' List all possible p2_p3.bar graphs based on given nodes nodes. It is the complement graph of p2_p3.
#' @param x The vector representing nodes

#' @return A matrix listing edges of p2_p3.bar graphs

#' @examples
#' p2_p3.bar(c(1:5))

p2_p3.bar = function(x){
        if(length(x)!=5){
                stop('The number of nodes should be FIVE!')
        }
        p2_p3.mat = p2_p3(x)
        p2_p3.prob = 1 - fillprob(x, p2_p3.mat)
        return(probToEdges(p2_p3.prob))
}
