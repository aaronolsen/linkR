\name{drawLinkage}
\alias{drawLinkage}
\title{Draws a lever or linkage}
\description{
This function creates a visualization of a linkage, with options to create a static plot or an interactive, 3D visualization.
}
\usage{
drawLinkage(linkage, method="svgViewR", file=NULL, animate = TRUE, 
    animate.duration = 1, animate.reverse = FALSE, animate.repeat = -1, 
    path.connect=NULL, connect.joints=TRUE, window.title='Linkage Viewer', 
    joint.col.fill="white", joint.col.stroke="black", joint.cex=1.5, 
    joint.lwd=2, point.col.fill="black", point.col.stroke="black", 
    point.cex=1, point.lwd=2, path.col.fill=NA, path.opacity.fill=1, 
    path.opacity.stroke=1, path.col.stroke="black", path.lwd = 1, 
    add = FALSE, ...)
}
\arguments{
  \item{linkage}{A linkage object. This can be the output of \code{\link{defineLinkage}} (to visualize the initial static conformation of the linkage) or \code{\link{animateLinkage}} (to visualize the animated linkage).}
  \item{method}{The method to use in creating the visualization. The default method uses the package \href{https://CRAN.R-project.org/package=svgViewR}{svgViewR} to create an interactive, 3D visualization as an '.html' file that can be opened in any major web browser. The alternative method is "plot", which draws the linkage in an R graphics window (as a static visualization). Note that the "plot" method is still in development and may have limited functionality. The use of the "svgViewR" method (default) is recommended.}
  \item{file}{A filename for the '.html' file if \code{method} is "svgViewR". For method "plot", a filename with the extension of the desired save-as filetype. Supported filetypes for method "plot" are: .bmp, .png, .jpg (or .jpeg), .tiff, .eps.}
  \item{animate}{A logical indicating whether the linkage should be drawn as an animation or as a static visualization with all frames superimposed. This only applies if the input parameter \code{linkage} is an object created by \code{\link{animateLinkage}}. An input parameter \code{linkage} created by \code{\link{defineLinkage}} will only have a single frame (the initial linkage conformation).}
  \item{animate.duration}{The number of seconds during which the entire linkage animation will playback in the viewer. Note that for linkages with a large number of associated points processing speed may reduce the speed with which the animation can play such that the duration will actually be greater than that specified here.}
  \item{animate.reverse}{Whether the animation should play both forward and reverse.}
  \item{animate.repeat}{The number of times the animation should repeat. A value of -1 (default) causes the animation to play non-stop.}
  \item{path.connect}{A list of vectors, each of which specifies linkage-associated points to connect in sequence with a path. If this is specified in \code{\link{defineLinkage}} then it does not have to be specified here as it will be already contained within the \code{linkage} object.}
  \item{connect.joints}{A logical indicating whether the joints should be connected in the visualization. The connections will correspond to those specified by the input parameter \code{joint.conn} to \code{\link{defineLinkage}}.}
  \item{window.title}{For \code{method} "svgViewR", the title that will appear at the top of the '.html' visualization file.}
  \item{joint.col.fill}{The fill color of the points representing linkage joints.}
  \item{joint.col.stroke}{The stroke (outline) color of the points representing linkage joints.}
  \item{joint.cex}{The size of the points representing linkage joints.}
  \item{joint.lwd}{The thickness of the stroke (outline) of the points representing linkage joints.}
  \item{point.col.fill}{The fill color of the linkage-associated points.}
  \item{point.col.stroke}{The stroke (outline) color of the linkage-associated points.}
  \item{point.cex}{The size of the linkage-associated points.}
  \item{point.lwd}{The thickness of the stroke (outline) of the linkage-associated points.}
  \item{path.col.fill}{The fill color of the paths specified by \code{path.connect}. By default, the paths are not filled.}
  \item{path.opacity.fill}{The fill opacity of the paths specified by \code{path.connect}.}
  \item{path.opacity.stroke}{The stroke (outline) opacity of the paths specified by \code{path.connect}.}
  \item{path.col.stroke}{The color of the stroke (outline) opacity of the paths specified by \code{path.connect}.}
  \item{path.lwd}{The thickness of the stroke (outline) opacity of the paths specified by \code{path.connect}.}
  \item{add}{Logical indicating whether linkage should be added to an existing visualization. This option is still in developement.}
  \item{...}{Additional parameters to be passed to plot device opening functions (e.g. 'jpeg', 'png', 'eps') in the case of \code{method} "plot".}
}
\details{
Please see \href{https://aaronolsen.github.io/software/linkr.html}{linkR Resources} for a tutorial on how to use the linkR package and the \href{https://aaronolsen.github.io/software/linkr/examples.html}{linkR example gallery} for several examples (code and animations) of different linkages that can be created using linkR.
}
\value{
NULL
}
\seealso{\code{\link{animateLinkage}}, \code{\link{defineLinkage}}}
\author{Aaron Olsen}