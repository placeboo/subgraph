#' x168 graphs
#' List all possible x168 graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of x168 graphs

#' @examples
#' x168(c(1:6))

x168 = function(x){
        if(length(x) != 6){
                stop("The number of nodes should be SIX!")
        }
        pair4 = combn(x, 4)
        x168.mat = matrix(NA, ncol = 7)
        for(i in 1:ncol(pair4)){
                pair4.vec = sort(pair4[, i])
                rst = x[! x %in% pair4.vec]
                diamond.mat = diamond(pair4.vec)
                # find the hub nodes
                hub = findNode(2, diamond.mat)
                cnct.mat = rbind(t(apply(hub, 1, function(y) c(deToIn(rst[1], y[1]), deToIn(rst[2], y[1])))),
                                 t(apply(hub, 1, function(y) c(deToIn(rst[1], y[2]), deToIn(rst[2], y[2])))))
                diamond.mat2 = rbind(diamond.mat, diamond.mat)
                x168.temp = cbind(diamond.mat2, cnct.mat)
                x168.mat = rbind(x168.mat, x168.temp)
        }
        return(x168.mat[-1, ])
}
