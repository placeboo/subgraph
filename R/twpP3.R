#' 2p3 graphs
#' List all possible 2p3 graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of 2p3 graphs

#' @examples
#' twoP3(c(1:6))

twoP3 = function(x){
        if(length(x) !=6){
                stop("The number of nodes should be SIX!")
        }
        pair3.mat = combn(x, 3)
        firstThree = pair3.mat[, 1]
        rstThree = pair3.mat[, 21 - 1]
        p3.mat1 = p3(firstThree)
        p3.mat2 = p3(rstThree)

        # build matrix by repeating each row of p3.mat1 three times
        p3.ext.mat1 = matrix(rep(p3.mat1, each = 3), ncol = 2, byrow = F)
        p3.ext.mat2 = rbind(p3.mat2, p3.mat2, p3.mat2)

        twoP3.mat = cbind(p3.ext.mat1, p3.ext.mat2)

        for(i in 2: (ncol(pair3.mat)/2)){
                firstThree = pair3.mat[, i]
                rstThree = pair3.mat[, 21 - i]
                p3.mat1 = p3(firstThree)
                p3.mat2 = p3(rstThree)

                # build matrix by repeating each row of p3.mat1 three times
                p3.ext.mat1 = matrix(rep(p3.mat1, each = 3), ncol = 2, byrow = F)
                p3.ext.mat2 = rbind(p3.mat2, p3.mat2, p3.mat2)

                temp.mat = cbind(p3.ext.mat1, p3.ext.mat2)

                twoP3.mat = rbind(twoP3.mat, temp.mat)
        }
        return(twoP3.mat)
}

