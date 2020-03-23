#' Sum pixel intensity above threshold
#'
#' In this function, we use the input from the quantCutoff function to sum the
#' pixel intensity in the fraction above the threshold. This is the
#' central function of the package.
#'
#' @param imageList Either a named list of images, as with the
#' example file or a movie, or a list of RGB png file names, e.g. generated with
#' list.files(). The second option is preferred for images, as this is a more
#' reasonable method from a memory perspective.
#' @param quantCutResult A result from the quantCutoff function on a relevant
#' control dataset. NB! The same controls can be included in the imageList too.
#' @importFrom png readPNG writePNG
#'
#' @return A named vector with the pixel intensity sum values for all frames in
#' imageList.
#'
#' @examples
#'
#' #Load example data.
#' data(testImages)
#'
#' #Run the function with the output in the current directory
#' sumVector <- pixSum(testImages, quantCutoff(testImages[1], 0.99))
#' @export pixSum
pixSum <- function(imageList, quantCutResult){
    sumList <- lapply(seq_along(imageList), function(x){
        if(inherits(imageList, "list")){
            images <- imageList[[x]]
        } else {
            images <- readPNG(imageList[x])[,,1]
        }
        sumAbove <- sum(images[which(images > quantCutResult)])
    })
    sumVec <- unlist(sumList)
    if(inherits(imageList, "list")){
        names(sumVec) <- names(imageList)
    } else {
        names(sumVec) <- imageList
    }
    return(sumVec)
}
