#' domino graphs
#' List all possible domino graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of domino graphs

#' @examples
#' domino(c(1:6))

domino = function(x){
        if(length(x)!=6){
                stop("The number of nodes should be SIX!")
        }
        x = sort(x)
        c6.mat = c6(x)
        domino.mat = matrix(NA, ncol = 7)
        # find which two should be connected
        for(i in 1: nrow(c6.mat)){ # go through eahc possible c6 graphs
                c6.vec = c6.mat[i, ]
                # start from 1
                neigOne = neighbor(1, c6.vec)
                # find the digonal of 1
                OneFour = x[!x %in% unique(c(neigOne, neighbor(neigOne[1], c6.vec), neighbor(neigOne[2], c6.vec))) ]
                # node next to 1
                two = neigOne[1]
                neigTwo = neighbor(two, c6.vec)
                # find the diagonal of two
                twoFive =  x[!x %in% unique(c(neigTwo, neighbor(neigTwo[1], c6.vec), neighbor(neigTwo[2], c6.vec))) ]
                # find the rest two nodes, and they are diagonal
                rst = x[!x %in% c(1, OneFour, two, twoFive)]
                cnct.vec = c(deToIn(1, OneFour), deToIn(two, twoFive), deToIn(rst[1], rst[2]))
                domino.temp = cbind(rbind(c6.vec, c6.vec, c6.vec), cnct.vec)
                domino.mat = rbind(domino.mat, domino.temp)
        }
        return(domino.mat[-1, ])
}
