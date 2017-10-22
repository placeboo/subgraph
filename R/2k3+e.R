#' 2K3+e graphs
#' List all possible 2K3+e graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of 2K3+e graphs

#' @examples
#' twoK3_e(c(1:6))

twoK3_e = function(x){
        if(length(x)!=6){
                stop("The number of nodes should be SIX!")
        }

        twoK3.mat = twoK3(x)
        pair3 = combn(x, 3)
        cnct.vec = c()
        for(i in 1: (ncol(pair3)/2)){
                # left + right = x
                left = pair3[, i]
                right = pair3[, 21 - i]
                # connect left and right
                cnct.vec = c(cnct.vec, paste(left[1], '-', right, sep = ''), paste(left[2], '-', right, sep = ''), paste(left[3], '-', right, sep = ''))
        }
        twoK3.mat = matrix(rep(twoK3.mat, each = 9), ncol = ncol(twoK3.mat), byrow = F)
        twoK3_e.mat = cbind(twoK3.mat, cnct.vec)
        return(twoK3_e.mat)
}
