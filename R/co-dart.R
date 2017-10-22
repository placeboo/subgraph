#' co-dart graphs
#' List all possible co-dart graphs based on FIVE given nodes.

#' @param x The vector representing nodes

#' @return A matrix listing edges of co-dart graphs

#' @examples
#' co.dart(1:5)

co.dart = function(x){
        if(length(x)!=5){
                stop('The number of nodes should be FIVE!')
        }
        mtx = matrix(NA, ncol = 4)
        for(i in 1: length(x)){
                temp_rest = x[-i]
                temp_paw = paw(temp_rest)
                mtx = rbind(mtx, temp_paw)
        }
        return(mtx[-1, ])
}
