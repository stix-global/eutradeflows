#' Products to harvest
#'
#' This is a list of 2 digit product codes
#' to be transferred from the Eurostat Comext repository
#' to a forest products trade flows database.
#' The \code{\link{harvest}} function transfers
#' these 2 digit product codes and all their sub-products
#' from Comext to the database.
#' The variables are:
#'  \itemize{
#'   \item `productcode`, product codes
#'   \item `description`, a very short description, for information purposes, not usualy used by the package.
#'  The official descriptions is provided in a comext text file called CN.txt.
#' }
#' @docType data
#' @name products2harvest
#' @format A data frame
#' @examples
#' products2harvest
NULL
