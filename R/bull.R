#' bull graphs
#' List all possible bull graphs based on given FIVE nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of bull graphs

#' @examples
#' bull(c(1:5))
bull = function(x){
        if(length(x)!=5)
{
                stop("The number of nodes should be FIVE!")
        }
        x = sort(x)
        mtx = matrix(NA, ncol = 5)
        pair2 = combn(x, 2)
        for(n in 1: ncol(pair2)){
                pair = pair2[, n]
                rest = x[!x %in% pair]
                k3.temp = k3(rest)
                # connect the isolate two nodes to k3
                twonodes_k3 = rbind(c(deToIn(pair[1], rest[1]), deToIn(pair[2], rest[2])),
                                    c(deToIn(pair[1], rest[1]), deToIn(pair[2], rest[3])),
                                    c(deToIn(pair[1], rest[2]), deToIn(pair[2], rest[1])),
                                    c(deToIn(pair[1], rest[2]), deToIn(pair[2], rest[3])),
                                    c(deToIn(pair[1], rest[3]), deToIn(pair[2], rest[1])),
                                    c(deToIn(pair[1], rest[3]), deToIn(pair[2], rest[2])))
                temp_mtx = cbind(matrix(rep(k3.temp, 6), nrow = 6, byrow = T), twonodes_k3)
                mtx = rbind(mtx, temp_mtx)
        }
        return(mtx[-1, ])
}
