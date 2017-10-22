#' A graphs
#' List all possible A graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of A graphs

#' @examples
#' A(c(1:6))

A = function(x){
        if(length(x) != 6){
                stop("The number of nodes should be SIX!")
        }
        x = sort(x)
        # 1-2-3-4-5-6
        p6.mat = p6(x)
        # connect 2-5

        temp.mat = apply(p6.mat, 1, function(y)as.numeric(unlist(strsplit(y, '-'))))
        # pick out the index of tail and head, which appear only once.
        headTail = apply(temp.mat, 2, function(y) x[table(y) == 1])

        # find which one connect to the head and tail
        connect.mat = matrix(0, ncol = ncol(temp.mat), nrow = 2)
        for(j in 1: ncol(temp.mat)){ # go through each isophism
                # find the index of head and tail in temp.mat
                index.vec = c(1:10)[temp.mat[, j] %in% headTail[, j]]
                for(n in 1: 2){ # go through each entry of index.vec
                        entry = index.vec[n]
                        if(entry%%2 == 0){ #  if the index of tail/head is the even, the one connected to it should be index - 1
                                connect.mat[n, j] = temp.mat[entry-1, j]
                        }else{ # if the index of tail/head is the odds, the one connected to it should be index + 1;
                                connect.mat[n,j] = temp.mat[entry+1, j]
                        }
                }
        }
        # connect the 1-2-3-4-5-6, 2-5
        A.mat = cbind(p6.mat, apply(connect.mat, 2, function(z) deToIn(z[1],z[2])))
        return(A.mat)
}
