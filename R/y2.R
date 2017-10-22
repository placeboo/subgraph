#' Y2 graphs
#' List all possible Y2 graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of Y2 graphs

#' @examples
#' y2(c(1:6))
y2 = function(x){
        if(length(x)!=6){
                stop('The number of nodes should be SIX!')
        }
        pair5 = combn(x, 5)
        y2.mat = matrix(NA, ncol = 6)
        for(i in 1: ncol(pair5)){
                pair5.vec = sort(pair5[, i])
                rst = x[!x%in%pair5.vec]
                cricket.mat = cricket(pair5.vec)
                # find the nodes which been connected twice
                hub = findNode(2, cricket.mat)
                cnct.mat = apply(hub, 1, function(y)c(deToIn(rst, y[1]), deToIn(rst,y[2])))
                y2.temp = rbind(cbind(cricket.mat, cnct.mat[1,]), cbind(cricket.mat, cnct.mat[2,]))
                y2.mat = rbind(y2.mat, y2.temp)
        }
        return(y2.mat[-1, ])
}
