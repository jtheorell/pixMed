#' Neighbor-based median filtering for 2D matrices
#'
#' This function is a simple and straight forward implementation of neighbor-
#' based median filtering for images.
#' @param imageList Either a list of imported images, as with the example file,
#' or more commonly in the user case, a list of file names, e.g. generated with
#' list.files(). The second option is preferred, as the function will handle
#' one file at the time anyway, and this is a more reasonable method from a
#' memory perspective. As this is primarily directed against reproduction of the
#' results from the cited paper by M Ramberger et al, currently only importing
#' of RGB-coloured pngs are implemented, and only the red fraction will be
#' looked at, as this has been used for phRodo analysis.
#' @param outDir The directory where the filtered files will be saved. If FALSE,
#' the full result will be returned as a list, which is not recommended.
#' @importFrom png readPNG writePNG
#' @importFrom BiocParallel bplapply
#' @importFrom ccaPP fastMedian
#'
#' @return The computational time is returned to screen. Filtered plots are
#' saved to outDir. If the outDir is set to FALSE, the result will be returned
#' as a list, which is discouraged for memory reasons.
#'
#' @examples
#'
#' #Load example data.
#' data(testImages)
#'
#' #Run the function with the output in the current directory
#' neighMedFilt(testImages, ".")
#' @export neighMedFilt
neighMedFilt <- function(imageList, outDir = "Median_filtered"){
    if(outDir != FALSE){
        dir.create(outDir, showWarnings = FALSE)
    } else {
        filtimageList <- list()
    }
    for(i in seq_along(imageList)){
        bef <- Sys.time()
        if(inherits(imageList, "list")){
            images <- imageList[[i]]
            locName <- names(imageList)[i]
        } else {
            images <- readPNG(imageList[i])[,,1]
            locName <- imageList[i]
        }
        if(grepl(".+/", locName)){
            imageName <- gsub(".+/|", "\\1", locName)
        } else {
            imageName <- locName
        }
        imagesMedFilt <- do.call(
            "cbind", bplapply(seq(2, ncol(images)-1), function(x){
            rowRes <- vapply(seq(2, nrow(images)-1), function(y){
                locMed <- fastMedian(images[seq(y-1, y+1), seq(x-1, x+1)])
            }, 1)
        }))
        imagesMedFiltFullC <- cbind(images[2:(nrow(images)-1),1], imagesMedFilt, images[2:(nrow(images)-1),ncol(images)])
        imagesMedFiltFull <- rbind(images[1,], imagesMedFiltFullC, images[nrow(images),])
        if(outDir != FALSE){
            writePNG(imagesMedFiltFull, paste0(outDir,"/MedFilt_", imageName, ".png"))
        } else {
            filtimageList[[imageName]] <- imagesMedFiltFull
        }
        message(paste0(imageName, " done in ", round(Sys.time()-bef), " seconds"))
    }
    if(outDir == FALSE){
        return(filtimageList)
    }
}
