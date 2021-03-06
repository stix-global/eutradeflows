% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readcomext.R
\name{comparemd5sum}
\alias{comparemd5sum}
\title{Compare the md5sum of files in folder 1 and folder2}
\usage{
comparemd5sum(folder1, folder2)
}
\arguments{
\item{folder1}{character path to a folder}

\item{folder2}{character path to a folder}
}
\value{
a data frame containing file names and a md5equal column
which is TRUE if md5sums are equal for the given file and FALSE otherwise.
}
\description{
Compare the md5sum of files in folder 1 and folder2
}
\examples{
\dontrun{
# This example compares files in 2 folders downloaded at one month interval
# From the  [MOST_RECENT_COMEXT_DATA] folder in the Comext bulk data repository
# Note, since February 2018 the folder doesn't exist anymore on Comext
# use the pattern format(Sys.Date(),"\\[\%Y") instead
dtf <- comparemd5sum("/tmp/comext/201706/data","/tmp/comext/201707/data")
dtf$md5equal
# Comparing a folder with itself obvisouly returns TRUE
obviously <- comparemd5sum("/tmp/comext/201706/data","/tmp/comext/201706/data")
obviously$md5equal
}
}
