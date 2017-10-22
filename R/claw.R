#' claw graphs
#' List all possible claw subgraphs based on given nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of claw graphs

#' @examples
#' claw(c(1:6))
claw = function(x){
        x = sort(x)
        pair4 = as.matrix(combn(x, 4))
        mtx = matrix(NA, ncol = 3)
        for(i in 1: 4){
                temp_vec = c()
                for(j in (1:4)[-i]){
                        sort.m = apply(rbind(pair4[i,], pair4[j,]), 2, sort)
                        temp_vec = c(temp_vec, paste(sort.m[1,], '-', sort.m[2,], sep = ""))
                }
                mtx = rbind(mtx, matrix(temp_vec, ncol = 3, byrow = F))
        }
        return(mtx[-1, ])
}
