#' P.bar graphs
#' List all possible P.bar graphs based on given FIVE nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of P.bar graphs

#' @examples
#' P.bar(c(1:5))
P.bar = function(x){
        if(length(x) !=5){
                stop("The number of nodes should be FIVE!")
        }
        x = sort(x)
        possibleEdges = paste(combn(x, 2)[1,],'-', combn(x, 2)[2,], sep = "")
        p.mat = P(x)
        p.eToP = fillprob(NN = x, p.mat) # from edges to prob.mat
        p_bar.eToP = 1 - p.eToP
        return(probToEdges(p_bar.eToP))
}
