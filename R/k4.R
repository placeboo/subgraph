#' k4 graphs
#' List all possible k4 graphs based on given nodes >= 4

#' @param x The vector representing nodes

#' @return A matrix listing edges of k4 graphs

#' @examples
#' k4(c(1:6))
#' k4(1:4)

k4 = function(x){
        if(length(x) < 4){
                stop('The number of nodes should be larger than four!')
        }
        pair4 = as.matrix(combn(x, 4))
        k4.mat = matrix(,, ncol = 6)
        for(i in 1:ncol(pair4)){
                pair4.vec = pair4[, i]
                pair2.mat = combn(pair4.vec, 2)
                k4.temp = paste(pair2.mat[1,], '-',pair2.mat[2,], sep = '')
                k4.mat = rbind(k4.mat, k4.temp)
        }
        return(k4.mat[-1,])
}
