#' co-butterfly graphs
#' List all possible co-butterfly subgraphs based on given FIVE nodes. Note that co-butterfly is the complement graph of butterfly based on give five nodes.

#' @param x The vector representing nodes

#' @return A matrix listing edges of co-butterfly graphs

#' @examples
#' co_butterfly(c(1:5))

co.butterfly = function(x){
        if(length(x) != 5){
                stop('The number of nodes should be FIVE!')
        }
        x = sort(x)
        mtx = matrix(NA, ncol = 4)
        for(i in 1: length(x)){
                temp_rest = x[-i]
                temp_c4 = c4(temp_rest)
                mtx = rbind(mtx, temp_c4)
        }
        return(mtx[-1, ])
}
