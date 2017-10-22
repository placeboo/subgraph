#' P graphs
#' List all possible P graphs based on given FIVE nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of P graphs

#' @examples
#' P(c(1:5))

P = function(x){
        if(length(x)!=5){
                stop("The number of nodes should be FIVE!")
        }
        mtx = matrix(NA, ncol = 5)
        for(i in 1: length(x)){
                i_node = x[i]
                temp_rest = x[-i]
                temp_c4 = matrix(rep(as.vector(c4(temp_rest)), each = 4), ncol = 4, byrow = F)
                i_connect_node = c()
                for(j in 1:length(temp_rest)){
                        i_connect_node = c(i_connect_node, deToIn(i_node, temp_rest[j]))
                }

                temp_mtx = cbind(temp_c4, rep(i_connect_node, 3))
                mtx = rbind(mtx, temp_mtx)
        }
        return(mtx[-1, ])
}
