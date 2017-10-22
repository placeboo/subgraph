#' s3.bar graphs
#' List all possible R graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of s3.bar graphs

#' @examples
#' s3.bar(c(1:6))
s3.bar = function(x){
        if(length(x) != 6){
                stop('The number of nodes should be SIX!')
        }
        x = sort(x)
        pair3 = combn(x, 3)
        s3.bar.mat = matrix(NA, ncol = 6)
        for(i in 1: ncol(pair3)){
                pair3.vec = pair3[, i]
                pair3.vec = sort(pair3.vec)
                rst = x[! x%in%pair3.vec]
                c3.mat = k3(pair3.vec)
                rst1 = rst[1]
                rst2s = rst[-1]
                cnct.mat = matrix(NA, ncol = 3)
                for(j in 1:3){ # go through each nodes in c3
                        c3node = pair3.vec[j]
                        rstC3Node = pair3.vec[-j]
                        # first to connect the one node with c3
                        # second to connect rest two nodes with rest c3's nodes
                        cnct.temp = rbind(c(deToIn(rst1, c3node), paste(sort(rst2s, decreasing = F), '-', rstC3Node,sep = '')),
                        c(deToIn(rst1, c3node), paste(sort(rst2s, decreasing = T), '-', rstC3Node,sep = '')))
                        cnct.mat = rbind(cnct.mat, cnct.temp)
                }
                cnct.mat = cnct.mat[-1, ]
                c3.mat = matrix(rep(c3.mat, each = nrow(cnct.mat)), ncol = 3)
                s3.bar.temp = cbind(c3.mat, cnct.mat)
                s3.bar.mat = rbind(s3.bar.mat, s3.bar.temp)
        }

        return(s3.bar.mat[-1, ])
}
