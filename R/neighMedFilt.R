#' Neighbor-based median filtering for 2D matrices
#'
#' This function is a simple and straight forward implementation of neighbor-
#' based median filtering for images.
#' @param pngList Either a list of imported pngs, as with the example file, or
#' more commonly in the user case, a list of file names, e.g. generated with
#' list.files(). The second option is preferred, as the function will handle
#' one file at the time anyway, and this is a more reasonable method from a
#' memory perspective.
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
neighMedFilt <- function(pngList, outDir = "Median_filtered"){
    if(outDir != FALSE){
        dir.create(outDir, showWarnings = FALSE)
    } else {
        filtPngList <- list()
    }
    for(i in seq_along(pngList)){
        bef <- Sys.time()
        if(inherits(pngList, "list")){
            pngs <- pngList[[i]]
            locName <- names(pngList)[i]
        } else {
            pngs <- readPNG(pngList[i])[,,1]
            locName <- pngList[i]
        }
        if(grepl(".+/", locName)){
            pngName <- gsub(".+/|", "\\1", locName)
        } else {
            pngName <- locName
        }
        pngsMedFilt <- do.call(
            "cbind", bplapply(seq(2, ncol(pngs)-1), function(x){
            rowRes <- vapply(seq(2, nrow(pngs)-1), function(y){
                locMed <- fastMedian(pngs[seq(y-1, y+1), seq(x-1, x+1)])
            }, 1)
        }))
        pngsMedFiltFullC <- cbind(pngs[2:(nrow(pngs)-1),1], pngsMedFilt, pngs[2:(nrow(pngs)-1),ncol(pngs)])
        pngsMedFiltFull <- rbind(pngs[1,], pngsMedFiltFullC, pngs[nrow(pngs),])
        if(outDir != FALSE){
            writePNG(pngsMedFiltFull, paste0(outDir,"/MedFilt_", pngName, ".png"))
        } else {
            filtPngList[[pngName]] <- pngsMedFiltFull
        }
        message(paste0(pngName, " done in ", round(Sys.time()-bef), " seconds"))
    }
    if(outDir == FALSE){
        return(filtPngList)
    }
}
