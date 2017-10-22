#' x166 graphs
#' List all possible x166 graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of x166 graphs

#' @examples
#' x166(c(1:6))


x166 = function(x){
        if(length(x) != 6){
                stop('The number of nodes should be SIX!')
        }
        x = sort(x)
        pair5 = combn(x, 5)
        x166.mat = matrix(,,ncol = 6)
        for(i in 1: ncol(pair5)){
                pair5.vec = pair5[, i]
                pair5.vec = sort(pair5.vec)
                rst = x[! x%in% pair5.vec]
                bull.mat = bull(pair5.vec)

                temp.mat = apply(bull.mat, 1, function(y)  as.numeric(unlist(strsplit(y, '-'))))
                # the number appear twice is the hub
                # connect the hub
                hub.mat = apply(temp.mat, 2, function(y) pair5.vec[table(y) == 1])
                cnct.mat = t(apply(hub.mat, 2, function(z) c(deToIn(z[1], rst), deToIn(z[2], rst))))
                x166.temp = rbind(cbind(bull.mat, cnct.mat[,1]), cbind(bull.mat, cnct.mat[,2]))
                x166.mat = rbind(x166.mat, x166.temp)
        }
        return(x166.mat[-1, ])
}
