#' x169 graphs
#' List all possible x169 graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of x169 graphs

#' @examples
#' x169(c(1:6))


x169 = function(x){
        if(length(x) != 6){
                stop('The number of nodes should be SIX!')
        }
        x = sort(x)
        pair5 = combn(x, 5)
        x169.mat = matrix(,,ncol = 6)
        for(i in 1: ncol(pair5)){
                pair5.vec = pair5[, i]
                pair5.vec = sort(pair5.vec)
                rst = x[! x%in% pair5.vec]
                cricket.mat = cricket(pair5.vec)

                temp.mat = apply(cricket.mat, 1, function(y)  as.numeric(unlist(strsplit(y, '-'))))
                # the number appear twice is the hub
                # connect the hub
                hub.mat = apply(temp.mat, 2, function(y) pair5.vec[table(y) == 1])
                cnct.mat = t(apply(hub.mat, 2, function(z) c(deToIn(z[1], rst), deToIn(z[2], rst))))
                x169.temp = rbind(cbind(cricket.mat, cnct.mat[,1]), cbind(cricket.mat, cnct.mat[,2]))
                x169.mat = rbind(x169.mat, x169.temp)
        }
        return(x169.mat[-1, ])
}
