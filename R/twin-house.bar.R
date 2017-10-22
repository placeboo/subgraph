#' twin-house.bar graphs
#' List all possible twin-house.bar graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of twin-house.bar graphs

#' @examples
#' twin.house.bar(c(1:6))


twin.house.bar = function(x){
        if(length(x) != 6){
                stop("The number of nodes should be SIX!")
        }
        pair4 = combn(x, 4)
        twin.house.bar.mat = matrix(NA, ncol = 7)
        for(i in 1:ncol(pair4)){
                pair4.vec = sort(pair4[, i])
                rst = x[! x %in% pair4.vec]
                diamond.mat = diamond(pair4.vec)
                # find the hub nodes
                hub = findNode(2, diamond.mat)
                cnct.mat = rbind(t(apply(hub, 1, function(y) c(deToIn(rst[1], y[1]), deToIn(rst[2], y[2])))),
                t(apply(hub, 1, function(y) c(deToIn(rst[1], y[2]), deToIn(rst[2], y[1])))))
                diamond.mat2 = rbind(diamond.mat, diamond.mat)
                twin.house.bar.temp = cbind(diamond.mat2, cnct.mat)
                twin.house.bar.mat = rbind(twin.house.bar.mat, twin.house.bar.temp)
        }
        return(twin.house.bar.mat[-1, ])
}
