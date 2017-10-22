#' x84 graphs
#' List all possible x84 graphs based on given SIX nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of x84 graphs

#' @examples
#' x84(c(1:6))


x84 = function(x){
        if(length(x) != 6){
                stop('The number of nodes should be SIX!')
        }
        pair4 = combn(x, 4)
        x84.mat = matrix(,,ncol = 6)
        # index of c4, the rest two points should connect to nodes which are face to face in c4
        faceToface.index = matrix(c(1,2,1,3,1,4), ncol = 2, byrow = T)
        for(i in 1: ncol(pair4)){
                pair4.vec = pair4[, i]
                pair4.vec = sort(pair4.vec)
                rst = x[! x%in% pair4.vec]
                # index, face to face: 12, 34; 13, 24; 14, 23
                c4.mat = c4(pair4.vec)
                cnct.mat = matrix(,, ncol = 2)
                for(j in 1:nrow(faceToface.index)){
                        nodesC4 = pair4.vec[faceToface.index[j,]]
                        cnct1 = paste(rst, '-', sort(nodesC4,decreasing = F), sep = '')
                        cnct2 = paste(rst, '-', sort(nodesC4,decreasing = T), sep = '')
                        otherNodesC4 =  pair4.vec[! pair4.vec%in%nodesC4 ]
                        cnct3 = paste(rst, '-', sort(otherNodesC4,decreasing = F), sep = '')
                        cnct4 = paste(rst, '-', sort(otherNodesC4,decreasing = T), sep = '')
                        cnct.temp    = rbind(cnct1, cnct2, cnct3, cnct4)
                        cnct.mat = rbind(cnct.mat, cnct.temp)
                }
                cnct.mat = cnct.mat[-1,]
                c4.mat = matrix(rep(c4.mat, each = 4), ncol = ncol(c4.mat), byrow = F)
                x84.temp = cbind(c4.mat, cnct.mat)
                x84.mat = rbind(x84.mat, x84.temp)
        }
        return(x84.mat[-1, ])
}
