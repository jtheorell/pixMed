#' Normalize each movie frame to its median and MAD and sum the result for all
#' pixels
#'
#' This function is meant to normalize frames of a movie, as there might be
#' slight variations in light intensity etc over the course of a live-cell
#' experiment lasting days.
#'
#' @param imageList A named list of images, as with the
#' example file or a movie, or a vector of TIFF file names, e.g. generated with
#' list.files().¨
#' @param quantThreshFrames The frames in the movie that will be used to set the
#' quantile-based threshold for summation. Default is frame 1:4, as four
#' pictures were taken per hour in the paper.
#' @param outDir If the imageList is a vector of TIFF file names, this will
#' be the place where the filtered movies will be saved. Default is to save
#' a new .rds file in the same directory as the old file, but with the extension
#' "normalized". Can
#' @importFrom tiff readTIFF writeTIFF
#' @importFrom ccaPP fastMAD
#' @return A list of two, both containing lists of length(imageList), containing
#' 1: pixel intensity sums for each frame in above the threshold.
#' 2: quantiles that should be used as input for the normMovie function.
#' In addition, side effect in form of filtered movies, one per imageList item.
#' 890:950,1285:1325
#' @examples
#'
#' #Load example data.
#' data(testImages)
#'
#' #Run the function with the output in the current directory
#' medMadNormIm <- medMadNorm(testImages)
#'
#' @export medMadNormSum
medMadNormSum <- function(imageList, quantThreshFrames = 1:4,
                          outDir = "default"){
    highQuantList <- lapply(imageList, function(x){
        images <- readTIFF(imageList[x], all = TRUE)
        if(outDir == "default"){
            there <- paste0(gsub("|\\.tif", "", imageList[x]), "_normalized.tif")
        } else {
            if(grepl("/", imageList[x])){
                locName <- gsub(".+/|\\.tif", "", imageList[x])
            } else {
                locName <- imageList[x]
            }
            there <- paste0(outDir, "/", locName, "_normalized.tif")
        }
        resultData <- medMadNormCoFunc(images, quantThreshFrames, there)
        return(resultData[[2]])
    })
    return(highQuantList)
}

medMadNormCoFunc <- function(images, quantThreshFrames, there){

    tifQuant <- median(unlist(lapply(quantThreshFrames, function(y){
        localTif <- images[[y]]
        medMad <- fastMAD(localTif)
        normTif <- (localTif-medMad[[1]])/medMad[[2]]
        quant <- quantile(normTif, testQuant)
    })))

    resultTif <- lapply(seq_along(images), function(y){
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
    return(list(sumTifs, highQuants))
}
