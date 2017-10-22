#' 5-pan graphs
#' List all possible 5-pan graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of 5-pan graphs

#' @examples
#' five_pan(c(1:6))

five_pan = function(x){
        if(length(x) != 6){
                stop('The number of nodes should be SIX!')
        }
        x = sort(x)
        pair5 = combn(x, 5)
        five_pan.mat = matrix(,, ncol = 6)
        for(i in 1:ncol(pair5)){
                pair5.vec = pair5[, i]
                pair5.vec = sort(pair5.vec)
                rst = x[! x%in%pair5.vec]
                c5.mat = c5(pair5.vec)
                c5.mat = matrix(rep(c5.mat, each = 5), ncol = 5, byrow = F)
                # connect the rest node to each nodes of c5
                pair5.mat = as.matrix(pair5.vec)
                cnct.mat = apply(pair5.mat, 1, function(z) deToIn(z, rst))
                cnct.mat = rep(cnct.mat, 12)
                five_pan.temp = cbind(c5.mat, cnct.mat)
                five_pan.mat = rbind(five_pan.mat, five_pan.temp)
        }
        return(five_pan.mat[-1,])
}
