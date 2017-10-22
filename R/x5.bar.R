#' x5.bar graphs
#' List all possible x5.bar graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of x5.bar graphs

#' @examples
#' x5.bar(c(1:6))

x5.bar = function(x){
        if(length(x) != 6){
                stop("The number of nodes should be SIX!")
        }
        pair5 = combn(x, 5)
        x5.bar.mat = matrix(NA, ncol = 7)
        for(i in 1:ncol(pair5)){
                pair5.vec = sort(pair5[, i])
                rst = x[!x %in% pair5.vec]
                house.mat = house(pair5.vec)
                hub = findNode(2, house.mat)
                # only need to find the hubs which are neighbors of each other
                # neighbor index, 1,2; 1,3; 2,3
                neighborIndex.pointer = rbind(c(1,2), c(1,3), c(2,3))
                is.neighbor.mat = matrix(0, ncol = 3, nrow(hub))
                hub.mat = matrix(NA, ncol = 2)
                for(j in 1: nrow(hub)){ # go through each graph
                        # find neighbors in hub.mat
                        neighborIndex = neighborIndex.pointer[c(is.neighbor(hub[j,1], hub[j,2], graph = house.mat[j, ]),
                          is.neighbor(hub[j,1], hub[j,3], graph = house.mat[j, ]),
                          is.neighbor(hub[j,2], hub[j,3], graph = house.mat[j, ])),]
                        hub.vec = hub[j, neighborIndex]
                        hub.mat = rbind(hub.mat, hub.vec)
                }
                hub.mat = hub.mat[-1, ]
                # connect the single node the the hub

                cnct.mat = t(apply(hub.mat, 1, function(y) c(deToIn(rst, y[1]), deToIn(rst, y[2]))))

                x5.temp = rbind(cbind(house.mat, cnct.mat[, 1]), cbind(house.mat, cnct.mat[, 2]))
                x5.bar.mat = rbind(x5.bar.mat, x5.temp)
        }
        return(x5.bar.mat[-1, ])
}
