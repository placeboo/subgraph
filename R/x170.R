#' x170 graphs
#' List all possible x170 graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of x170 graphs

#' @examples
#' x170(c(1:6))

x170 = function(x){
        if(length(x) != 6){
                stop("The number of nodes should be SIX!")
        }
        pair5 = combn(x, 5)
        x170.mat = matrix(NA, ncol = 7)
        for(i in 1:ncol(pair5)){
                pair5.vec = sort(pair5[, i])
                rst = x[! x%in% pair5.vec]
                dart.mat = dart(pair5.vec)
                tail.mat = as.matrix(findNode(1, dart.mat))
                cnct.mat = apply(tail.mat, 1, function(y) deToIn(rst, y))
                x170.temp = cbind(dart.mat, cnct.mat)
                x170.mat = rbind(x170.mat, x170.temp)
        }
        return(x170.mat[-1, ])
}
