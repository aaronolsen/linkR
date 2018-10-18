associateShape <- function(mechanism, shape, body){

	associatePoints(mechanism, shape$points, body=body, points.connect=shape$connect)
}
