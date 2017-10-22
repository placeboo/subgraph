#' x167 graphs
#' List all possible x167 graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of x167 graphs

#' @examples
#' x167(c(1:6))

x167 = function(x){
        if(length(x) != 6){
                stop("The number of nodes should be SIX!")
        }
        pair5 = combn(x, 5)
        x167.mat = matrix(NA, ncol = 7)
        for(i in 1: ncol(pair5)){
                pair5.vec = sort(pair5[, i])
                rst = x[!x %in% pair5.vec]
                k23.mat = k23(pair5.vec)
                hub.mat = findNode(3, k23.mat)
                cnct.mat = t(apply(hub.mat, 1, function(y) c(deToIn(rst, y[1]), deToIn(rst, y[2]))))
                x167.temp = rbind(cbind(k23.mat, cnct.mat[, 1]), cbind(k23.mat, cnct.mat[, 2]))
                x167.mat = rbind(x167.mat, x167.temp)
        }
        return(x167.mat[-1, ])
}
