random_trajectory <- function(n, start = 0, mean = 0, sd = 1, seed = NULL, 
	smooth = TRUE, span = NULL, cumulative = TRUE){

#	if(!cumulative) n <- n + 1

	# Set span based on number of points
	if(is.null(span)) span <- 50 / n

	# Set seed if not null
	if(!is.null(seed)) set.seed(seed)
	
	# Make sure dimensions match
	if(!is.null(start)) if(length(start) < length(mean)) start <- rep(start, length(mean))
	if(length(sd) < length(mean)) sd <- rep(sd, length(mean))

	# Create matrix to hold final values
	rt_mat <- matrix(NA, nrow=n, ncol=length(mean))

	# Run Brownian motion (cumulative sum of normally distributed random variable)
	for(i in 1:length(mean)) rt_mat[, i] <- cumsum(rnorm(n, mean=mean[i], sd=sd[i]))
	
	# Set start
	if(!is.null(start)) rt_mat <- rt_mat - matrix((rt_mat[1, ]-start), nrow=nrow(rt_mat), ncol=ncol(rt_mat), byrow=TRUE)

	# Find difference between iterations for non-cumulative output
	if(!cumulative) rt_mat <- apply(rbind(rep(0,ncol(rt_mat)), rt_mat), 2, 'diff')
		
	# Smooth
	if(smooth){
	
		rt_raw <- rt_mat

		for(i in 1:length(mean)){

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
