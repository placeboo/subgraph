#' x58.bar graphs
#' List all possible x58.bar graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of x58.bar graphs

#' @examples
#' x58.bar(c(1:6))

x58.bar = function(x){
        if(length(x) != 6){
                stop("The number of nodes should be SIX!")
        }
        pair5 = combn(x, 5)
        x58.bar.mat = matrix(NA, ncol = 7)
        for(i in 1:ncol(pair5)){
                pair5.vec = sort(pair5[, i])
                rst = x[!x %in% pair5.vec]
                house.mat = house(pair5.vec)
                hub = findNode(3, house.mat)

                cnct.mat = t(apply(hub, 1, function(y) c(deToIn(y[1], rst), deToIn(y[2], rst))))

                x58.temp = rbind(cbind(house.mat, cnct.mat[, 1]), cbind(house.mat, cnct.mat[, 2]))
                x58.bar.mat = rbind(x58.bar.mat, x58.temp)
        }
        return(x58.bar.mat[-1, ])
}
