#' p4 graphs
#' List all possible p4 graphs based on given four nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of p4 graphs

#' @examples
#' p4(c(1:4))

p4 = function(x){ # vector representing nodes
        num_node = length(x)
        mtx = matrix(NA, ncol = 3)
        # exclude one isolate x
        permn4 = matrix(unlist(permn(x)), ncol = 4, byrow = T)
        permn4.unrep = permn4[1: (nrow(permn4)/2), ]
        # sort, rewrite 4-2 to 2-4
        vec.temp = c()
        for(i in 1: 3){
                temp.sort = t(apply(permn4.unrep[, i:(i+1)], 1, sort))
                vec.temp = c(vec.temp, paste(temp.sort[,1], '-', temp.sort[,2], sep = ""))
        }
        mtx = rbind(mtx, matrix(vec.temp, ncol = 3, byrow = F))
        return(mtx[-1, ])
}
