% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hiQuantAdjust.R
\name{hiQuantAdjust}
\alias{hiQuantAdjust}
\title{Relate the intensities in each previously medMad-normalized movie frame to
all other frames}
\usage{
hiQuantAdjust(medMadNormImages, medMadOutput, outDir = ".")
}
\arguments{
\item{medMadNormImages}{A vector of TIFF movie file names, e.g. generated
with list.files(). The input should have been normalized with the
\code{\link{medMadNormSum}}function.}

\item{medMadOutput}{The output from the \code{\link{medMadNormSum}} function
for the run that generated the medMadNormImages.}

\item{outDir}{Directory where the normalized tiff movies will be saved.
Default is to save a new tiff file in the current directory, with the same
name as the old file, but with the extension "adjusted_final".}
}
\value{
The adjusted tiff movies, saved to the outDir.
}
\description{
This function takes a set of image frames, or a set of tiff-movies, that have
previously been normalized with the medMadNormSum function, and uses the
highQuants portion of the medMadNormSum output to relate them to each other.
This is used both to make the individual movie meaningful, but can also be
used to make a set of movies, e.g. where both controls and interesting
samples are present, relate to each other, so that the top intensity in all
the movies dictates the values in all movies.
}
\examples{

#Load example data.
data(testMovie)

#Run medMadNormSumRes with the output in the current directory
medMadNormSumRes <- medMadNormSum(testMovie)

#Now run the final function
hiQuantAdjust("Test_normalized.tif", medMadNormSumRes)
}
