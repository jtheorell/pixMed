#' Sum pixel intensity above threshold
#'
#' In this function, we use the input from the quantCutoff function to sum the
#' pixel intensity in the fraction above the threshold for individual images.
#'
#' @param imageList Either a named list of images, as with the
#' example file, or a vector of RGB png file names, e.g. generated with
#' list.files(). The second option is preferred, as this is a more
#' reasonable method from a memory perspective.
#' @param quantCutThreshold A result from the quantCutoff function on a relevant
#' control dataset. NB! The same controls can be included in the imageList too.
#' @importFrom png readPNG writePNG
#'
#' @return A named vector with the sum of pixel intensities above the
#' quantCutThreshold normalized to the total number of pixels in the frame.
#'
#' @examples
#'
#' #Load example data.
#' data(testImages)
#'
#' #Run the function with the output in the current directory
#' pixSum(testImages, quantCutoff(testImages[1], 0.99))
#' #Expected result:
#' #       A33        P79
#' #0.00140716 0.02007561
#' @export pixSum
pixSum <- function(imageList, quantCutThreshold){
    sumList <- lapply(seq_along(imageList), function(x){
        message(paste0("Processing image ", x))
        if(inherits(imageList, "list")){
            images <- imageList[[x]]
        } else {
            images <- readPNG(imageList[x])[,,1]
        }
        sumAbove <- sum(images[which(images > quantCutThreshold)])/
            length(as.vector(images))
    })
    sumVec <- unlist(sumList)
    if(inherits(imageList, "list")){
        names(sumVec) <- names(imageList)
    } else {
        names(sumVec) <- imageList
    }
    return(sumVec)
}
