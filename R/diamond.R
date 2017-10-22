#' Diamond graphs
#' List all possible fiveFlower graphs based on given Four nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of Diamond graphs

#' @examples
#' diamond(c(1:4))

diamond = function(x){
        if(length(x) != 4){
                stop("The number of nodes should be FOUR!")
        }
        # for four nodes, diamond is the complement of p2
        x = sort(x)
        # edges
        comb = combn(x, 2) # possible combination
        L = ncol(comb) # number of lines
        line.name = paste(comb[1,],'-', comb[2,], sep = "")
        p2_prob.mat = diag(1, ncol = length(line.name), nrow = length(line.name))
        diam_prob.mat = 1 - p2_prob.mat
        colnames(diam_prob.mat) = line.name
        return(probToEdges(diam_prob.mat))
}
