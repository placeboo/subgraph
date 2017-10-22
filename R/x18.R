#' x18 graphs
#' List all possible x18 graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of x18 graphs

#' @examples
#' x18(c(1:6))

x18 = function(x){
        if(length(x) != 6){
                stop('The number of nodes should be SIX!')
        }
        x = sort(x)
        pair5 = combn(x, 5)
        x18.mat = matrix(,, ncol = 6)
        for(i in 1:ncol(pair5)){
                pair5.vec = pair5[, i]
                pair5.vec = sort(pair5.vec)
                rst = x[! x%in%pair5.vec]
                p.mat = P(pair5.vec)

                # # find node which is connected once
                temp.mat = apply(p.mat, 1, function(y)  as.numeric(unlist(strsplit(y, '-'))))
                # the number appear twice is the hub
                # connect the hub
                hub.mat = as.matrix(apply(temp.mat, 2, function(y) pair5.vec[table(y) == 1]))
                cnct.mat = apply(hub.mat, 1, function(z) deToIn(z, rst))
                x18.temp.mat = cbind(p.mat, cnct.mat)
                x18.mat = rbind(x18.mat, x18.temp.mat)
        }
        return(x18.mat[-1,])
}
