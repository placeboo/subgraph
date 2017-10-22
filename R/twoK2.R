#' 2k2 graphs
#' List all possible 2k2 graphs based on given nodes

#' @param x The vector representing nodes

#' @return A matrix listing edges of 2k2 graphs

#' @examples
#' 2k2(c(1:6))

twoK2 = function(x){
        x = sort(x)
        combn4 = as.matrix(combn(x, 4))
        pair2 = c(paste(combn4[1, ], '-', combn4[2, ], sep = ''), paste(combn4[3, ], '-', combn4[4, ], sep = ''), paste(combn4[1, ], '-', combn4[3, ], sep = ''), paste(combn4[2, ], '-', combn4[4, ], sep = ''), paste(combn4[1, ], '-', combn4[4, ], sep = ''), paste(combn4[2, ], '-', combn4[3, ], sep = ''))
        temp_pair2 = matrix(pair2, nrow = 6, byrow = T)

        pair2_2k2.mtx = t(cbind(temp_pair2[c(1:2),], temp_pair2[c(3:4),], temp_pair2[c(5:6),]))
        return(pair2_2k2.mtx)
}
