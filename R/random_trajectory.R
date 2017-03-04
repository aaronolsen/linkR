random_trajectory <- function(n, start = c(0,0,0), mean=c(0,0,0), sd=c(1,1,1), seed = NULL, 
	smooth = FALSE, span = 0.75){

	# Set seed if not null
	if(!is.null(seed)) set.seed(seed)
	
	# Make sure dimensions match
	if(length(mean) < length(start)) mean <- rep(mean, length(start))
	if(length(sd) < length(start)) sd <- rep(sd, length(start))

	# Create matrix to hold final values
	rt_mat <- matrix(NA, nrow=n, ncol=length(start))

	# Run Brownian motion (cumulative sum of normally distributed random variable)
	for(i in 1:length(start)) rt_mat[, i] <- start[i] + cumsum(rnorm(n, mean=mean[i], sd=sd[i]))

	# 
	if(smooth){
	
		for(i in 1:length(start)){

			# Create data frame
			data_frame <- data.frame(t=1:n, y=rt_mat[, i])

			# Smooth - higher span value corresponds to smoother fit
			lowpass.loess <- suppressWarnings(lowpass.loess <- loess(y ~ t, data=data_frame, span=span))

			# Fill matrix
			rt_mat[, i] <- predict(lowpass.loess, data_frame)
		}
	}

	rt_mat
}
