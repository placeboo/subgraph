#' gem graphs
#' List all possible gem graphs based on given FIVE nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of gem graphs

#' @examples
#' gem(c(1:6))
gem = function(x){
        if(length(x)!=5){
                stop('The number of nodes should be FIVE!')
        }
        x = sort(x)
        pair4 = combn(x, 4)
        gem.mat = matrix(,, ncol = 7)
        for(i in 1: ncol(pair4)){
                pair4.vec = pair4[, i]
                rst = x[!x %in% pair4.vec]
                p4.mat = p4(pair4.vec)
                cnct.vec = paste(rst,'-', pair4.vec,sep = '')
                cnct.mat = matrix(rep(cnct.vec, each = nrow(p4.mat)), ncol = length(cnct.vec), byrow = F)
                gem.temp = cbind(p4.mat, cnct.mat)
                gem.mat = rbind(gem.mat, gem.temp)
        }
        return(gem.mat[-1, ])
}
