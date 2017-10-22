#' x96 graphs
#' List all possible x96 graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of x96 graphs

#' @examples
#' x96(c(1:6))

x96 = function(x){
        if(length(x) != 6){
                stop("The number of nodes should be SIX!")
        }
        pair4 = combn(x, 4)
        x96.mat = matrix(NA, ncol = 7)
        for(i in 1: ncol(pair4)){
                pair4.vec = sort(pair4[,i])
                rst = x[!x%in%pair4.vec]
                diamond.mat = diamond(pair4.vec)
                # have been connected twice
                hub1.mat = findNode(2, diamond.mat)
                # have been connected three times
                hub2.mat = findNode(3, diamond.mat)
                # adding the rest nodes to diamonds,
                # the two nodes are connect to the neighbors.
                cnct.mat = rbind(cbind(rep(paste(rst[1], '-', hub1.mat[, 1], sep = ''), 2),paste(rst[2], '-', hub2.mat, sep = '')),
                                 cbind(rep(paste(rst[1], '-', hub1.mat[, 2], sep = ''), 2),paste(rst[2], '-', hub2.mat, sep = '')),
                                 cbind(rep(paste(rst[1], '-', hub2.mat[, 1], sep = ''), 2),paste(rst[2], '-', hub1.mat, sep = '')),
                                 cbind(rep(paste(rst[1], '-', hub2.mat[, 2], sep = ''), 2),paste(rst[2], '-', hub1.mat, sep = '')))
                diamond.mat2 = do.call(rbind, replicate(8, diamond.mat, simplify=FALSE))
                x96.temp = cbind(diamond.mat2, cnct.mat)
                x96.mat = rbind(x96.mat, x96.temp)
        }
        return(x96.mat[-1, ])
}
