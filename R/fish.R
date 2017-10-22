#' fish graphs
#' List all possible fish graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of fish graphs

#' @examples
#' fish(c(1:6))
fish = function(x){
        if(length(x)!=6){
                stop("The number of nodes should be SIX!")
        }
        pair4 = combn(x, 4)
        fish.mat = matrix(NA, ncol = 7)
        for(i in 1:ncol(pair4)){
                pair4.vec = sort(pair4[, i])
                rst = x[! x%in%pair4.vec]
                c4.mat = c4(pair4.vec)
                cnct.mat = cbind(paste(rst[1], '-', pair4.vec, sep = ''),
                                 paste(rst[2], '-', pair4.vec, sep = ''),
                                 rep(deToIn(rst[1], rst[2]), 4))
                c4.mat2 = matrix(rep(c4.mat, each = nrow(cnct.mat)), ncol = ncol(c4.mat), byrow=F)
                cnct.mat2 = rbind(cnct.mat, cnct.mat, cnct.mat)
                fish.temp = cbind(c4.mat2, cnct.mat2)
                fish.mat = rbind(fish.mat, fish.temp)
        }
        return(fish.mat[-1, ])
}
