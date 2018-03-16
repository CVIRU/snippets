# Project:      CD40 x MAdCAM-1 Duobody
# Sponsor:      Michael Scully
# Author:       Davit Sargsyan
# Created:      11/14/2013 
# Description:  Function: load all scripts from a directory
# Arguments:    path - path to the directory
#               trace - boolean: should the file full names be printed to screen?
#
# Requires:     
# Modified:     03/24/2012    Modified from a Robust IC50 function
##################################################################################################
source.dir <- function(path, trace = TRUE) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    source(file.path(path, nm))
  }
}