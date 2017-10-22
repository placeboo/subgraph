#' cross graphs
#' List all possible cross graphs based on given FIVE nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of cross graphs

#' @examples
#' cross(c(1:6))
cross = function(x){
        if(length(x) !=6 ){
                stop("The number of nodes should be SIX!")
        }else{
                cross.mat = matrix(,,ncol = 5)
                x = sort(x)
                pair5 = combn(x, 5)
                for(i in 1: ncol(pair5)){ # go for each combn of five nodes
                        nodes = pair5[, i]
                        temp.mat = k14(nodes)
                        temp.mat = matrix(rep(temp.mat, each = 4),ncol = 4, byrow = F)
                        rst = x[! x %in% nodes]
                        # connect the left nodes to the others except the "hub"
                        # hub in sequence
                        temp.vec = c()
                        for (j in 1: length(nodes)){ # for through each hub
                                leave = nodes[-j]
                                for(k in 1:length(leave)){
                                        temp.vec = c(temp.vec, deToIn(rst, leave[k]))
                                }

                        }
                        temp.mat = cbind(temp.mat, temp.vec)
                        cross.mat = rbind(cross.mat, temp.mat)
                }
                return(cross.mat[-1, ])
        }

}
