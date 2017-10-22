#' fiveFlower graphs
#' List all possible fiveFlower graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of fiveFlower graphs

#' @examples
#' k5_k1.bar(c(1:6))

k5_k1.bar = function(x){
        if(length(x) != 6){
                stop("The number of nodes should be SIX!")
        }
        mat = matrix(,,ncol = 5)
        x = sort(x)
        for(i in 1: length(x)){ # go through the hub
                hub = x[i]
                rst.vec = x[-i]
                temp.mat = c(deToIn(hub, rst.vec[1]), deToIn(hub, rst.vec[2]), deToIn(hub, rst.vec[3]), deToIn(hub, rst.vec[4]), deToIn(hub, rst.vec[5]))
                mat = rbind(mat, temp.mat)
        }
        return(mat[-1, ])
}
