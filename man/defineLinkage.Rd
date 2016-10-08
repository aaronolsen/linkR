\name{defineLinkage}
\alias{defineLinkage}
\title{Defines a lever or linkage}
\description{
This function takes the joint positions, types and constraints of a lever or linkage mechanism or a set of minimum parameters and creates a list of class \code{"linkage"} that forms the basic object for model creation and analysis.
}
\usage{
defineLinkage(joint.coor, joint.types, joint.cons, 
              joint.conn = NULL, link.points = NULL, link.assoc = NULL, 
              link.names = NULL, ground.link = NULL, path.connect = NULL, 
              lar.cons = NULL)
}
\arguments{
  \item{joint.coor}{A matrix of 2D or 3D coordinates that are the joints of a lever or linkage mechanism.}
  \item{joint.types}{A vector of single letters indicating the type of constraints acting on each joint. Supported types are "R", "U", "L" and "P". See Details.}
  \item{joint.cons}{A list or matrix of 3D constraint vectors that correspond to each joint.}
  \item{joint.conn}{A two-column matrix having the same number of rows as \code{joint.coor}, specifying the two links that each joint connects.}
  \item{link.points}{A matrix of points associated with one or more of the links in the linkage (optional).}
  \item{link.assoc}{A vector of integers or rownames indicating the link with which each row in \code{points} is associated. Required if \code{points} is non-\code{NULL} but otherwise optional.}
  \item{link.names}{A vector of the link names (optional).}
  \item{ground.link}{A numeric or link name specifying the ground link of the linkage.}
  \item{path.connect}{A list of vectors, each specifying the points to connect (in sequence) with lines when drawing. This parameter is only used when calling the function \code{drawLinkage}.}
  \item{lar.cons}{A list specifying long-axis rotation constraints on links with S-joints on each end. This feature will eventually be added into the \code{joint.conn} input parameter as network connections.}
}
\details{
Please see \href{https://aaronolsen.github.io/software/linkr.html}{linkR Resources} for a tutorial on how to use the linkR package and the \href{https://aaronolsen.github.io/software/linkr/examples.html}{linkR example gallery} for several examples (code and animations) of different linkages that can be created using linkR.
}
\value{
a list of class \code{"linkage"} with the following elements:
	\item{joint.coor}{A matrix of 2D or 3D coordinates that are the joints of a lever or linkage mechanism.}
	\item{joint.cons}{A vector of single letters indicating the type of constraints acting on each joint.}
	\item{joint.types}{A list or matrix of 3D constraint vectors that correspond to each joint.}
	\item{joint.links}{A matrix specifying the joints connecting to each link. Used by the \code{animateLinkage} function.}
	\item{joint.paths}{"Path fragments" along the linkage network, used by the \code{animateLinkage} function to solve for the position of unresolved joints.}
	\item{joint.conn}{A two-column matrix having the same number of rows as \code{joint.coor}, specifying the two links that each joint connects.}
	\item{joint.init}{The initial position of the joints in the linkage.}
	\item{ground.joints}{The ground joints in the linkage.}
	\item{points}{A matrix of points associated with any of the links in the linkage.}
	\item{points.assoc}{The links with which each point is associated.}
	\item{link.assoc}{A vector of integers or rownames indicating the link with which each row, if input.}
	\item{link.names}{A vector of the link names.}
	\item{point.connect}{A list of vectors, each of which specifies linkage-associated points to connect in sequence with a path. This is only used by \code{\link{drawLinkage}} when creating the linkage visualization.}
	\item{link.lcs}{Local coordinate systems assigned to each link which will be transformed with the associated link for kinematic analysis.}
	\item{lar.cons}{Constraints on long-axis rotation.}
	\item{num.links}{The number of links in the linkage.}
	\item{dof}{The number of degrees of freedom of the linkage. Currently, this may not be accurately estimated for all linkages.}
}
\seealso{\code{\link{animateLinkage}}, \code{\link{drawLinkage}}}
\author{Aaron Olsen}