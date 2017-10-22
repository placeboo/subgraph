#' All subgraphs
#' List all subgraphs of specific number of nodes

#' @param NN Possible nodes.
#' @param x The matrix shows edges with repect to each subgraph of the complete graph. All entries of the matrix are strings, for example "1-3", which represent endpoints of the edge in increasing order. If it is not in increassing order, the function will correct the order.

#' @return A matrix filled with 1 and 0. Number 1 represents there is an edge in the subgraph, otherwise 0.

#' @examples
#' fillprob(c(1:5), rbind(c("1-2", "1-3"), c("2-4", "1-3")))
fillprob = function(NN, x){
        # N: number of noods
        # indic: indictor for the edge
        possible_lines = paste(combn(NN, 2)[1,],'-', combn(NN, 2)[2,], sep = "")
        mtx = matrix(0, nrow = nrow(x), ncol = length(possible_lines))
        colnames(mtx) = possible_lines
        index = 0
        for(i in 1:nrow(x)){
                # each entry of x should be nonincreasing
                # eg. 3-2 forbit
                temp.x = x[i, ]
                temp.mat = matrix(as.numeric(unlist(strsplit(temp.x, '-'))), ncol = 2, byrow = T)
                if(!all(temp.mat[, 2] > temp.mat[, 1])){ # find the nonincreasing order order
                        decrease.index = which(temp.mat[, 2] < temp.mat[, 1])
                        # correct into increassing order
                        x[i, decrease.index] = paste(temp.mat[decrease.index, 2], '-', temp.mat[decrease.index, 1], sep = '')
                }
                mtx[i, possible_lines %in% x[i, ]] = 1
        }
        return(mtx)
}
