#' x37 graphs
#' List all possible x37 graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of x37 graphs

#' @examples
#' x37(c(1:6))

x37 = function(x){
        if(length(x)!=6){
                stop("The number of nodes should be SIX!")
        }
        pair5 = combn(x, 5)
        x37.mat = matrix(NA, ncol = 7)
        for(i in 1:ncol(pair5)){
                pair5.vec = sort(pair5[, i])
                rst = x[! x %in% pair5.vec]
                bull.mat = bull(pair5.vec)
                # find the the hub
                temp.mat = apply(bull.mat, 1, function(y)  as.numeric(unlist(strsplit(y, '-'))))
                hub = apply(temp.mat, 2, function(y) pair5.vec[table(y) == 1])
                cnct.mat = t(apply(hub, 2, function(y) paste(rst, '-', y, sep = '')))
                x37.temp = cbind(bull.mat, cnct.mat)
                x37.mat = rbind(x37.mat, x37.temp)
        }
        return(x37.mat[-1, ])
}
