#' w4_k1.bar graphs
#' List all possible w4_k1.bar graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of w4_k1.bar graphs

#' @examples
#' w4_k1.bar(c(1:6))

w4_k1.bar = function(x){
        if(length(x)!=6){
                stop("The number of nodes should be SIX!")
        }
        pair5 = combn(x, 5)
        w4_k1.mat = matrix(NA, ncol = 7)
        for(i in 1:ncol(pair5)){
                pair5.vec = sort(pair5[, i])
                rst = x[! x %in% pair5.vec]
                butterfly.mat = butterfly(pair5.vec)
                # find the the hub
                temp.mat = apply(butterfly.mat, 1, function(y)  as.numeric(unlist(strsplit(y, '-'))))
                hub = apply(temp.mat, 2, function(y) pair5.vec[table(y) == 4])
                cnct.mat = paste(rst, '-', hub, sep = '')
                w4_k1.temp = cbind(butterfly.mat, cnct.mat)
                w4_k1.mat = rbind(w4_k1.mat, w4_k1.temp)
        }
        return(w4_k1.mat[-1, ])
}
