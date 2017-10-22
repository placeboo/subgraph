#' x95 graphs
#' List all possible x95 graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of x95 graphs

#' @examples
#' x95(c(1:6))
x95 = function(x){
        if(length(x) != 6){
                stop('The number of nodes should be SIX!')
        }

        x = sort(x)
        pair4 = combn(x, 4)
        x95.mat = matrix(NA,ncol = 6)
        for(i in 1: ncol(pair4)){
                pair4.vec = pair4[, i]
                pair4.vec = sort(pair4.vec)
                rst = x[! x%in% pair4.vec]
                paw.mat = paw(pair4.vec)
                # find the node who connects the tail
                # find the tail of P.bar
                temp.mat = apply(paw.mat, 1, function(y)  as.numeric(unlist(strsplit(y, '-'))))
                tail = apply(temp.mat, 2, function(y) pair4.vec[table(y) == 1])

                cnct.mat = cbind(paste(rst[1],'-', tail, sep = ''), paste(rst[2],'-', tail, sep = ''))
                x95.temp = cbind(paw.mat, cnct.mat)
                x95.mat = rbind(x95.mat, x95.temp)
        }
        return(x95.mat[-1,])
}
