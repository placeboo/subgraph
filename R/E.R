#' E graphs
#' List all possible E graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of E graphs

#' @examples
#' E(c(1:6))

E = function(x){
        if(length(x) != 6){
                stop("The number of nodes should be SIX")
        }
        x = sort(x)
        pair5 = combn(x, 5)
        E.mat = matrix(,,ncol = 5)
        for(i in 1: ncol(pair5)){
                nodes = pair5[, i]
                rest = x[!x %in% nodes]
                p5.mat = p5(nodes)
                # pick the one connect to the single node
                temp.mat = apply(p5.mat[, c(2,3)], 1, function(y)  as.numeric(unlist(strsplit(y, '-'))))
                # the number appear twice is the hub
                # connect the hub
                hub.vec = apply(temp.mat, 2, function(y) rownames(as.matrix(table(y)))[table(y) == 2])
                E.temp = cbind(p5.mat, paste(rest, "-", hub.vec, sep = ""))
                E.mat = rbind(E.mat, E.temp)
        }
        return(E.mat[-1, ])
}
