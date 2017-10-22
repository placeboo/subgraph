#' Y1 graphs
#' List all possible Y1 graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of Y1 graphs

#' @examples
#' y1(c(1:6))

y1 = function(x){
        if(length(x)!=6){
                stop('The number of nodes should be SIX!')
        }
        pair3 = combn(x, 3)
        y1.mat = matrix(NA, ncol = 6)
        for(i in 1: ncol(pair3)){
                pair3.vec = pair3[, i]
                rst = x[!x%in%pair3.vec]
                c3.mat = k3(pair3.vec)
                cnct.mat = matrix(NA, ncol = 3)
                for(j in 1:3){ # go through each nodes in k3
                        c3.node = pair3.vec[j]
                        cnct.temp = paste(rst, '-', c3.node, sep = '')
                        cnct.mat = rbind(cnct.mat, cnct.temp)
                }
                cnct.mat = cnct.mat[-1, ]
                c3.mat = matrix(rep(c3.mat, each = 3), ncol = 3, byrow = F)
                y1.temp = cbind(c3.mat, cnct.mat)
                y1.mat = rbind(y1.mat, y1.temp)
        }
        return(y1.mat[-1, ])
}
