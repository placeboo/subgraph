#' p6 graphs
#' List all possible p6 subgraphs based on given FIVE nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of p6 graphs

#' @examples
#' p6(c(1:6))

p6 = function(x){
        if(length(x) != 6){
                stop("The number of nodes should be SIX!")
        }
                x = sort(x)
                num_node = length(x)
                mtx = matrix(NA, ncol = 5)
                permn6 = matrix(unlist(permn(x)), ncol = 6, byrow = T)
                permn6.unrep = permn6[1: (nrow(permn6)/2), ]
                # sort, rewrite 4-2 to 2-4
                vec.temp = c()
                for(i in 1: 5){
                        temp.sort = t(apply(permn6.unrep[, i:(i+1)], 1, sort))
                        vec.temp = c(vec.temp, paste(temp.sort[,1], '-', temp.sort[,2], sep = ""))
                }
                mtx = rbind(mtx, matrix(vec.temp, ncol = 5, byrow = F))
                return(mtx[-1, ])
}
