#' p2_p3 graphs
#' List all possible p2_p3 graphs based on given nodes nodes.
#' @param x The vector representing nodes

#' @return A matrix listing edges of p2_p3 graphs

#' @examples
#' p2_p3(c(1:5))

p2_p3 = function(x){
        if(length(x)!=5){
                stop('The number of nodes should be FIVE!')
        }
        pair3 = combn(x, 3)
        p2_p3.mat = matrix(NA, ncol = 3)
        for(i in 1: ncol(pair3)){
                pair3.vec = sort(pair3[,i])
                rst = x[!x%in%pair3.vec]
                p3.mat = p3(pair3.vec)
                p2.vec = deToIn(rst[1], rst[2])
                p2_p3.temp = cbind(p3.mat, p2.vec)
                p2_p3.mat = rbind(p2_p3.mat, p2_p3.temp)
        }
        return(p2_p3.mat[-1, ])
}
