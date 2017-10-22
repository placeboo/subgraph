#' x163 graphs
#' List all possible x163 graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of x163 graphs

#' @examples
#' x163(c(1:6))

x163 = function(x){
        if(length(x) != 6){
                stop("The number of nodes should be SIX!")
        }
        pair5 = combn(x, 5)
        x163.mat = matrix(NA, ncol = 7)

        for(i in 1:ncol(pair5)){
                pair5.vec = sort(pair5[, i])
                rst = x[! x %in% pair5.vec]
                butterfly.mat = butterfly(pair5.vec)
                # find the the hub
                hub = findNode(2, butterfly.mat)
                cnct.mat = paste(rst, '-', hub, sep = '')
                butterfly.mat2 = do.call(rbind, replicate(ncol(hub), butterfly.mat, simplify = F))
                x163.temp = cbind(butterfly.mat2, cnct.mat)
                x163.mat = rbind(x163.mat, x163.temp)
        }
        return(x163.mat[-1, ])
}
