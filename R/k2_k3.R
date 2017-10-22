#' k2 U k3 graphs
#' List all possible k2 U k3 graphs based on FIVE given nodes. k2 U k3 is the complement of k2,3

#' @param x The vector representing nodes

#' @return A matrix listing edges of k2 U k3 graphs

#' @examples
#' k2_k3(1:5)
k2_k3 = function(x){
        if(length(x)!= 5){
                stop('The number of nodes should be FIVE!')
        }
        x = sort(x)
        pair3 = combn(x, 3)
        k2_k3.mat = matrix(NA, ncol = 4)
        for(i in 1:ncol(pair3)){
                pair3.vec = sort(pair3[, i])
                c3.mat = k3(pair3.vec)
                rst = x[! x%in% pair3.vec]
                k2_k3.temp = cbind(c3.mat, deToIn(rst[1], rst[2]))
                k2_k3.mat = rbind(k2_k3.mat, k2_k3.temp)
        }
        return(k2_k3.mat[-1, ])
}
