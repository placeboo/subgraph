#' y4 graphs
#' List all possible twin-house.bar graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of y4 graphs

#' @examples
#' y4(c(1:6))


y4 = function(x){
        if(length(x) != 6){
                stop("The number of nodes should be SIX!")
        }
        pair4 = combn(x, 4)
        y4.mat = matrix(NA, ncol = 7)
        for(i in 1:ncol(pair4)){
                pair4.vec = sort(pair4[, i])
                rst = x[! x %in% pair4.vec]
                diamond.mat = diamond(pair4.vec)
                # find the hub nodes
                hub = findNode(3, diamond.mat)
                cnct.mat = rbind(t(apply(hub, 1, function(y) c(deToIn(rst[1], y[1]), deToIn(rst[2], y[2])))),
                                 t(apply(hub, 1, function(y) c(deToIn(rst[1], y[2]), deToIn(rst[2], y[1])))))
                diamond.mat2 = rbind(diamond.mat, diamond.mat)
                y4.temp = cbind(diamond.mat2, cnct.mat)
                y4.mat = rbind(y4.mat, y4.temp)
        }
        return(y4.mat[-1, ])
}
