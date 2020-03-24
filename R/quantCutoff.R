#' Find a quantile-based cutoff to use for images or movies
#'
#' Here, a set of control frames and a quantile is provided, which together
#' generate a cutoff used for downstream analyses.
#' @param ctrlImageList Either a named list, as with the
#' example file, or a vector of RGB png file names, e.g. generated with
#' list.files(). The second option is preferred, as this is a more
#' reasonable method from a memory perspective.
#' @param quant The quantile of pixel intensities in all the images in
#' ctrlImageList from which the cutoff value will be derived.
#' @importFrom png readPNG writePNG
#' @importFrom stats quantile median
#'
#' @return The median value for the quantile specified in quant for all the
#' images in the ctrlImageList.
#'
#' @examples
#'
#' #Load example data.
#' data(testImages)
#'
#' #Run the function with the output in the current directory
#' quantCutoff(testImages[1], 0.99)
#' #Expected result: 0.1098039
#'
#' @export quantCutoff
quantCutoff <- function(ctrlImageList, quant = 0.99){
    thresholdAll <- median(unlist(lapply(seq_along(ctrlImageList), function(x){
        if(inherits(ctrlImageList, "list")){
            images <- ctrlImageList[[x]]
        } else {
            images <- readPNG(ctrlImageList[x])[,,1]
        }
        result <- quantile(images, quant)
    })))
    return(thresholdAll)
}
