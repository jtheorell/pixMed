#' Normalize each movie frame to its median and MAD and sum the result for all
#' pixels
#'
#' This function is meant to normalize frames of a movie, as there might be
#' slight variations in light intensity etc over the course of a live-cell
#' experiment lasting days.
#'
#' @param imageList A named list of images, as with the
#' example file, or a vector of TIFF file names, e.g. generated with
#' list.files().
#' @param quantThreshFrames The frames in the movie that will be used to set the
#' quantile-based threshold for summation. Default is frame 1:4, as four
#' pictures were taken per hour in the paper.
#' @param quantThreshQuant The quantile above which for the control frames
#' defined above, that the sum of pixel intensity will be calculated for all
#' frames.
#' @param outDir Directory where the normalized tiff movies will be saved.
#' Default is to save a new tiff file in the current directory, with the same
#' name as the old file, but with the extension "normalized".
#' @importFrom tiff readTIFF writeTIFF
#' @importFrom ccaPP fastMAD
#' @importFrom BiocParallel bplapply
#' @seealso \code{\link{hiQuantAdjust}}
#' @return A list of two, both containing lists of length(imageList):
#' \describe{
#'    \item{pixSums}{Pixel intensity sums for each frame in above the
#'    quantile threshold.}
#'    \item{highQuants}{quantiles that should be used as input for the
#'    hiQuantAdjust function.}
#' }
#' In addition, side effect in form of normalized movies, one per imageList
#' item. Worth noting is that the intensities in these movies are flat, i.e.,
#' the top and bottom intensity is identical in all frames. To make these
#' movies useful, use the \code{\link{hiQuantAdjust}} function.
#'
#' @examples
#'
#' #Load example data.
#' data(testMovie)
#'
#' #Run the function with the output in the current directory
#' medMadNormSumRes <- medMadNormSum(testMovie)
#'
#' @export medMadNormSum
medMadNormSum <- function(imageList,
                          quantThreshFrames = 1:4,
                          quantThreshQuant = 0.99,
                          outDir = "."){
    if(inherits(imageList, "list")){
        resultData <- medMadNormCoFunc(imageList, quantThreshFrames,
                                       quantThreshQuant,
                                       there = "Test_normalized.tif")
    } else {
        highQuantList <- lapply(imageList, function(x){
            shortName <- gsub("|\\.tif", "", basename(x))
            message(paste0(shortName, " currently processed"))
            images <- readTIFF(x, all = TRUE)
            there <- paste0(outDir, "/", shortName, "_normalized.tif")
            resultData <- medMadNormCoFunc(images, quantThreshFrames,
                                           quantThreshQuant, there)
            return(resultData)
        })
        pixSums <- lapply(highQuantList, "[[", 1)
        highQuants <- lapply(highQuantList, "[[", 2)
        return(list("pixSums" = pixSums, "highQuants" = highQuants))
    }

}

medMadNormCoFunc <- function(images, quantThreshFrames, quantThreshQuant,
                             there){

    tifQuant <- median(unlist(lapply(quantThreshFrames, function(y){
        localTif <- images[[y]]
        medMad <- fastMAD(localTif)
        normTif <- (localTif-medMad[[1]])/medMad[[2]]
        quant <- quantile(normTif, quantThreshQuant)
    })))

    resultTif <- bplapply(seq_along(images), function(y){
        localTif <- images[[y]]
        medMad <- fastMAD(localTif)
        #Normalization
        normTif <- (localTif-medMad[[1]])/medMad[[2]]
        normTif[which(normTif < tifQuant)] <- 0
        sumTif <- sum(as.vector(normTif))
        highQuant <- quantile(normTif[which(normTif !=0)], 0.95)
        tif01plus <- normTif/highQuant
        tif01plus[which(tif01plus > 1)] <- 1
        return(list(tif01plus, highQuant, sumTif))
    })
    tifList <- lapply(resultTif, "[[", 1)
    highQuants <- unlist(lapply(resultTif, "[[", 2))
    sumTifs <- unlist(lapply(resultTif, "[[", 3))
    nameVec <- seq(0, by = 0.25, length.out = length(sumTifs))
    names(sumTifs) <- nameVec
    names(highQuants) <- nameVec
    writeTIFF(tifList, where = there)
    return(list("pixSums" = sumTifs, "highQuants" = highQuants))
}
