#' x45 graphs
#' List all possible x45 graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of x45 graphs

#' @examples
#' x45(c(1:6))

x45 = function(x){
        if(length(x) != 6){
                stop("The number of nodes should be SIX!")
        }
        pair5 = combn(x, 5)
        x45.mat = matrix(NA, ncol = 7)
        for(i in 1: ncol(pair5)){
                pair5.vec = sort(pair5[, i])
                rst = x[!x %in% pair5.vec]
                k23.mat = k23(pair5.vec)
                hub.mat = findNode(2, k23.mat)
                cnct.mat = t(apply(hub.mat, 1, function(y) c(deToIn(rst, y[1]), deToIn(rst, y[2]), deToIn(rst, y[3]))))
                x45.temp = rbind(cbind(k23.mat, cnct.mat[, 1]), cbind(k23.mat, cnct.mat[, 2]), cbind(k23.mat, cnct.mat[, 3]))
                x45.mat = rbind(x45.mat, x45.temp)
        }
        return(x45.mat[-1, ])
}
