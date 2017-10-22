#' chair graphs
#' List all possible chair graphs based on given FIVE nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of chair graphs

#' @examples
#' chair(c(1:5))
chair = function(x){
        if(length(x) != 5){
                stop("The number of nodes should be FIVE!")
        }
        mtx = matrix(NA, ncol = 4)
        for(i in 1:length(x)){ # pick one node, connect the rest (except the hub)
                temp_rest = x[-i]
                i_node = x[i]
                # complete graph, removing non-overlap edge (do not share a same node)
                # temp_claw = matrix(rep(as.vector(claw(temp_rest)), each = 3), ncol = 3, byrow = F)
                temp_claw = claw(temp_rest)
                temp_vec = c()
                for(n in 1: nrow(temp_claw)){
                        temp_tab = as.matrix(table(unlist(strsplit(temp_claw[n,], split = "-"))))
                        node_connect = as.numeric(rownames(temp_tab)[which(temp_tab[,1] == 1)])
                        temp_vec = c(temp_vec, c(deToIn(i_node, node_connect[1]), deToIn(i_node, node_connect[2]), deToIn(i_node, node_connect[3])))
                }
                temp_mtx = cbind(matrix(rep(as.vector(temp_claw), each = 3), ncol = 3, byrow = F), temp_vec)
                mtx = rbind(mtx, temp_mtx)
        }
        return(mtx[-1, ])
}
