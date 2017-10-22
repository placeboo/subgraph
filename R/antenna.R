#' antenna graphs
#' List all possible antenna graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of antenna graphs

#' @examples
#' antenna(c(1:6))

antenna = function(x){
        if(length(x) != 6){
                stop("The number of nodes should be SIX!")
        }
        pair5 = combn(x, 5)
        antenna.mat = matrix(NA, ncol = 7)
        for(i in 1:ncol(pair5)){
                pair5.vec = sort(pair5[, i])
                rst = x[!x %in% pair5.vec]
                house.mat = house(pair5.vec)
                hub = findNode(2, house.mat)
                # only need to find the hubs which are neighbors of each other
                # neighbor index, 1,2; 1,3; 2,3
                neighborIndex.pointer = rbind(c(1,2), c(1,3), c(2,3))
                is.neighbor.mat = matrix(0, ncol = 3, nrow(hub))
                hub.vec = c()
                for(j in 1: nrow(hub)){ # go through each graph
                        # find not neighbors in hub.mat
                        neighborIndex = neighborIndex.pointer[c(is.neighbor(hub[j,1], hub[j,2], graph = house.mat[j, ]),
                                                                is.neighbor(hub[j,1], hub[j,3], graph = house.mat[j, ]),
                                                                is.neighbor(hub[j,2], hub[j,3], graph = house.mat[j, ])),]
                        # find not the neighbors
                        hub.temp = hub[j, c(1:3)[-neighborIndex]]
                        hub.vec = rbind(hub.vec, hub.temp)
                }

                # connect the single node the the hub

                cnct.vec = paste(rst, '-', hub.vec, sep = '')

                antenna.temp = cbind(house.mat, cnct.vec)
                antenna.mat = rbind(antenna.mat, antenna.temp)
        }
        return(antenna.mat[-1, ])
}
