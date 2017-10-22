#' x98.bar graphs
#' List all possible R graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of x98.bar graphs

#' @examples
#' x98.bar(c(1:6))
x98.bar = function(x){
        if(length(x) != 6){
                stop('The number of nodes should be SIX!')
        }
        pair3 = combn(x, 3)
        x98.bar.mat = matrix(,, ncol = 6)
        for(i in 1: ncol(pair3)){
                pair3.vec = pair3[, i]
                rest.vec = pair3[, ncol(pair3) + 1 - i]
                rest.vec = sort(rest.vec)
                c3.mat = k3(pair3.vec)
                c3.mat = matrix(rep(c3.mat, each = 6), ncol = ncol(c3.mat), byrow = F)

                p3.mat = p3(rest.vec)
                # find the tail and head of p3
                temp.mat = apply(p3.mat, 1, function(y)  as.numeric(unlist(strsplit(y, '-'))))
                headTail = apply(temp.mat, 2, function(y) rest.vec[table(y) == 1])
                for(j in 1:3){ # go through each nodes of c3
                        c3node = pair3.vec[j]
                        # cnct.vec = c(c3-head-p3, c3-tail-p3)
                        cnct.vec = c(deToIn(c3node, headTail[1,1]), deToIn(c3node, headTail[1,2]), deToIn(c3node, headTail[1, 3]),deToIn(c3node, headTail[2,1]), deToIn(c3node, headTail[2,2]), deToIn(c3node, headTail[2, 3]))
                        cnct_c3.mat = cbind(rbind(p3.mat, p3.mat), cnct.vec)
                        # connect p3 with c3
                        temp.mat = cbind(c3.mat, cnct_c3.mat)
                        x98.bar.mat = rbind(x98.bar.mat, temp.mat)
                }
        }
        return(x98.bar.mat[-1, ])
}

