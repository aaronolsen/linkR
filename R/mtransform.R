mtransform <- function(mat, tmat){
	mat <- t(tmat %*% t(cbind(mat, rep(1, nrow(mat)))))[, 1:3]
}