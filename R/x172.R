#' x172 graphs
#' List all possible x172 graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of x172 graphs

#' @examples
#' x172(c(1:6))

x172 = function(x){
        if(length(x) != 6){
                stop("The number of nodes should be SIX!")
        }
        x172.mat = matrix(,, ncol = 5)
        pair4 = combn(x, 4)
        for(i in 1: ncol(pair4)){
                pair4.temp = pair4[, i]
                rst = x[!x%in%pair4.temp]
                p4.mat = p4(pair4.temp)
                temp.mat = apply(p4.mat, 1, function(y)  as.numeric(unlist(strsplit(y, '-'))))

                # the number appear once is the head and tail
                tailHead = apply(temp.mat, 2, function(y) pair4.temp[table(y) == 1])
                # connect the head
                x172.temp1 = cbind(p4.mat, cbind(paste(rst[1], '-', tailHead[1,], sep = ''), paste(rst[2], '-', tailHead[1,], sep = '')))
                # connect the tial
                x172.temp2 = cbind(p4.mat, cbind(paste(rst[1], '-', tailHead[2,], sep = ''), paste(rst[2], '-', tailHead[2,], sep = '')))
                x172.temp = rbind(x172.temp1, x172.temp2)
                x172.mat = rbind(x172.mat, x172.temp)
        }
        return(x172.mat[-1,  ])

}
