#' paw graphs
#' List all possible claw subgraphs based on given FOUR nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of paw graphs

#' @examples
#' paw(c(1:4))
paw = function(x){
        x = sort(x)
        if(length(x) != 4){
                stop("The number of nodes should be FOUR!")
        }
        mtx = matrix(NA, ncol = 4)
        for(i in 1: length(x)){ #isolate node
                i_node = x[i]
                temp_rest = x[-i]
                one_node = c(deToIn(i_node, temp_rest[1]), deToIn(i_node, temp_rest[2]), deToIn(i_node, temp_rest[3]))
                temp_mtx = cbind(matrix(rep(k3(temp_rest), 3), nrow = 3, byrow = T), one_node)
                mtx = rbind(mtx, temp_mtx)
        }
        return(mtx[-1, ])
}
