#' x171 graphs
#' List all possible x171 graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of x171 graphs

#' @examples
#' x171(c(1:6))

x171 = function(x){
        if(length(x) != 6){
                stop("The number of nodes should be SIX!")
        }
        pair5 = combn(x, 5)
        x171.mat = matrix(NA, ncol = 7)
        for(i in 1:ncol(pair5)){
                pair5.vec = sort(pair5[, i])
                rst = x[! x%in% pair5.vec]
                co.fork.mat = co.fork(pair5.vec)
                tail.mat = as.matrix(findNode(1, co.fork.mat))
                cnct.mat = apply(tail.mat, 1, function(y) deToIn(rst, y))
                x171.temp = cbind(co.fork.mat, cnct.mat)
                x171.mat = rbind(x171.mat, x171.temp)
        }
        return(x171.mat[-1, ])
}
