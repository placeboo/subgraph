#' twin-c5 graphs
#' List all possible twin-c5 graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of twin-c5 graphs

#' @examples
#' domtwin.c5ino(c(1:6))


twin.c5 = function(x){
        if(length(x)!=6){
                stop("The number of nodes should be SIX!")
        }
        x = sort(x)
        pair4 = combn(x, 4)
        twin.c5.mat = matrix(NA, ncol = 7)
        for(i in 1:ncol(pair4)){
                pair4.vec = pair4[,i]
                c4.mat = c4(pair4.vec)

                rst = x[! x%in%pair4.vec]
                p2.vec = deToIn(rst[1], rst[2])
                # find the diagonal of the c4
                diag.mat1 = t(apply(c4.mat, 1, function(y) neighbor(pair4.vec[1], y)))
                diag.mat2 = t(apply(diag.mat1, 1, function(y) pair4.vec[!pair4.vec %in% y]))
                diag.mat = rbind(diag.mat1, diag.mat2)
                # concnt p2 to c4
                cnct.mat = rbind(cbind(paste(rst[1], '-', diag.mat[,1], sep = ''),
                      paste(rst[2], '-', diag.mat[,2], sep = '')),
                      cbind(paste(rst[2], '-', diag.mat[,1], sep = ''),
                      paste(rst[1], '-', diag.mat[,2], sep = '')))
                c4.mat2 = rbind(c4.mat, c4.mat, c4.mat, c4.mat)

                twin.c5.temp = cbind(c4.mat2, cnct.mat, p2.vec)
                twin.c5.mat = rbind(twin.c5.mat, twin.c5.temp)
        }
        return(twin.c5.mat[-1, ])
}
