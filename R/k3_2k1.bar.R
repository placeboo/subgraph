#' k3_2k1.bar graphs
#' List all possible k3_2k1.bar graphs based on given nodes nodes. It is the complement graph of k3_2k1.
#' @param x The vector representing nodes

#' @return A matrix listing edges of k3_2k1.bar graphs

#' @examples
#' k3_2k1.bar(c(1:5))

k3_2k1.bar = function(x){
        if(length(x)!=5){
                stop('The number of nodes should be FIVE!')
        }
        k3_2k1.mat = k3(x)
        k3_2k1.bar.prob = 1 - fillprob(x, k3_2k1.mat)
        return(probToEdges(k3_2k1.bar.prob))
}
