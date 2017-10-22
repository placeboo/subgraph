#' p5 graphs
#' List all possible p5 subgraphs based on given FIVE nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of p5 graphs

#' @examples
#' p5(c(1:5))

p5 = function(x){
        if(length(x) != 5){
                stop("The number of nodes should be five")
        }else{
                x = sort(x)
                num_node = length(x)
                mtx = matrix(NA, ncol = 4)
                permn5 = matrix(unlist(permn(x)), ncol = 5, byrow = T)
                permn5.unrep = permn5[1: (nrow(permn5)/2), ]
                # sort, rewrite 4-2 to 2-4
                vec.temp = c()
                for(i in 1: 4){
                        temp.sort = t(apply(permn5.unrep[, i:(i+1)], 1, sort))
                        vec.temp = c(vec.temp, paste(temp.sort[,1], '-', temp.sort[,2], sep = ""))
                }
                mtx = rbind(mtx, matrix(vec.temp, ncol = 4, byrow = F))
                return(mtx[-1, ])
        }

}
