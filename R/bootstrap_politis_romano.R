# bootstrap_politis_romano <- function(alternative, n, blockparam, display = TRUE, flag, mat) {
#
#   r <- nrow(alternative)
#   c <- ncol(alternative)
#
#   BtstrpMat <- matrix(data = NA, nrow = n, ncol = c)
#
#   for (i in 1:n) {
#
#     if (display) {
#       cat('Simulations done : ', i, '\n')
#     }
#
#     New <- matrix(data = NA, nrow = r, ncol = c)
#     tel <- 0
#
#     while (tel < r) {
#       tel <- tel + 1
#       p   <- runif(n = 1)
#       if (p < blockparam || tel == 1) {
#         row <- round(runif(n = 1, max = r), 0)
#         # had to add this to avoid row = 0
#         if (row == 0) {
#           row <- row + 1
#         }
#         Xnext <- alternative[row, ]
#       } else {
#         row <- row + 1
#         if (row > r) {
#           row <- row - r
#         }
#         Xnext <- alternative[row, ]
#       }
#       New[tel, ] <- Xnext
#     }
#
#     if (flag == 1) {
#       f <- -(New ^ 2) + mat ^ 2
#     } else if (flag == 2) {
#       f <- log(1 + New) - log(1 + mat)
#     } else if (flag == 3) {
#       f <- -abs(New) + abs(mat)
#     }
#
#     froof <- colMeans(f)
#     BtstrpMat[i, ] <- froof
#
#   }
#
#   return(BtstrpMat)
#
# }
