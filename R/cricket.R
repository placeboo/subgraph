#' cricket graphs
#' List all possible bull graphs based on given FIVE nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of cricket graphs

#' @examples
#' cricket(c(1:5))
#'
cricket = function(x){
        if(length(x) !=5){
                stop("The number of nodes should be FIVE!")
        }
        pair3 = combn(x, 3)
        cricket.mat = matrix(NA, ncol = 5)
        for(i in 1: ncol(pair3)){
                pair3.vec = sort(pair3[,i])
                rst = sort(x[!x%in%pair3.vec])
                k3.mat = k3(pair3.vec)
                cricket.temp = cbind(rbind(k3.mat, k3.mat, k3.mat), matrix(c(paste(rst, '-', pair3.vec[1], sep = ''), paste(rst, '-', pair3.vec[2], sep = ''), paste(rst, '-', pair3.vec[3], sep = '')), ncol=2, byrow=T))
                cricket.mat = rbind(cricket.mat, cricket.temp)
        }
        return(cricket.mat[-1, ])
}
