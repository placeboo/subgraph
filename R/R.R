#' R graphs
#' List all possible R graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of R graphs

#' @examples
#' R(c(1:6))


R = function(x){
        if(length(x) != 6){
                stop('The number of nodes should be SIX!')
        }
        pair4 = combn(x, 4)
        x = sort(x)
        R.map = matrix(,, ncol = 6)
        for(i in 1: ncol(pair4)){
                pair4.temp = pair4[ , i]
                rst = x[!x %in% pair4.temp]
                c4.mat = c4(pair4.temp)
                # connect rest nodes to c4 graph
                nodesCnctC4 = matrix(,,ncol = 2)
                for(j in 1:length(pair4.temp)){
                        temp.vec = c(deToIn(rst[1], pair4.temp[j]), deToIn(rst[2], pair4.temp[j]))
                        nodesCnctC4 = rbind(nodesCnctC4, temp.vec)
                }
                nodesCnctC4 = nodesCnctC4[-1, ]
                nodesCnctC4 = matrix(rep(nodesCnctC4, 3), ncol = 2, byrow = F)
                temp.mat = cbind(matrix(rep(c4.mat, each = 4), ncol = 4, byrow = F), nodesCnctC4)
                R.map = rbind(R.map, temp.mat)
        }
        return(R.map[-1, ])
}
