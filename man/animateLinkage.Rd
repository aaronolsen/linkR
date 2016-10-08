\name{animateLinkage}
\alias{animateLinkage}
\title{Animates a lever or linkage}
\description{
This function animates a lever or linkage mechanism according to specified translational or rotational motion of the input link.
}
\usage{
animateLinkage(linkage, input.param, input.joint=NULL,
               check.inter.joint.dist = TRUE, check.joint.cons = TRUE, 
               check.inter.point.dist = TRUE, print.progress = FALSE)
}

\arguments{
  \item{linkage}{A linkage object. This is a list of class \code{"linkage"} created by \code{\link{defineLinkage}}.}
  \item{input.param}{A list containing the input parameters for the linkage. The number of list elements corresponds to the number of joints for which input motion is specified. Thus, a one degree-of-freedom mechanism should be of length one; a two degree-of-freedom mechanism of length two, etc.}
  \item{input.joint}{A vector specifying the joint(s) at which the input motion is to be applied. Currently, input motion can only be specified at grounded joints.}
  \item{check.inter.joint.dist}{A logical indicating whether a check should be run to verify that the distances among connected joints is constant. A warning message is displayed if the distances are non-constant.}
  \item{check.joint.cons}{A logical indicating whether a check should be run to verify that the joint constraints are maintained through the simulation. This feature is currently in development and does not work for all joint types.}
  \item{check.inter.point.dist}{A logical indicating whether a check should be run to verify that the distances among points on the same body are constant. A warning message is displayed if the distances are non-constant.}
  \item{print.progress}{A logical indicating whether to print progress while running.}
}
\details{
Please see \href{https://aaronolsen.github.io/software/linkr.html}{linkR Resources} for a tutorial on how to use the linkR package and the \href{https://aaronolsen.github.io/software/linkr/examples.html}{linkR example gallery} for several examples (code and animations) of different linkages that can be created using linkR.
}
\value{
a list of class \code{"linkage"} having the same elements as the input \code{linkage} with some exceptions, including:
	\item{joint.coor}{The joint matrix will be converted to an array, where \code{dim(joints)[3]} is equal to the number of animation iterations.}
	\item{points}{If provided in the input, the point matrix will be converted to an array, where \code{dim(points)[3]} is equal to the number of animation iterations.}
}
\seealso{\code{\link{defineLinkage}}}
\author{Aaron Olsen}