## Script for creating and building the dynatop R package
rm(list=ls())
graphics.off()

## path of the package
pacPath <- '.'
devtools::document(pacPath)
devtools::check(pacPath)

## check documentation build
pkgdown::clean_site(pacPath)
pkgdown::build_site(pacPath)
pkgdown::clean_site(pacPath)

## ## mac and windows
## rhub::validate_email() # for first time that session
## pkgName <- sub('\\.tar.gz$', '', basename(tmp)) 
## ## rhub::platforms()[,1] # lists platforms
## mch <- rhub::check(path = tmp,
##                    platform = c("macos-highsierra-release-cran","windows-x86_64-release",
##                                 "windows-x86_64-oldrel"))

## tmp <- paste0(pkgName,".tgz")
## ftmp <- file.path("../..",tmp)
## download.file(file.path(mch$urls()$artifacts[1],tmp),ftmp)
## drat::insertPackage(ftmp,dratPath,action="prune")

## tmp <- paste0(pkgName,".zip")
## ftmp <- file.path("../..",tmp)
## download.file(file.path(mch$urls()$artifacts[3],tmp),ftmp)
## drat::insertPackage(ftmp,dratPath,action="prune")
## download.file(file.path(mch$urls()$artifacts[2],tmp),ftmp)
## drat::insertPackage(ftmp,dratPath,action="prune")

## ## tidy up drat
## drat::pruneRepo(dratPath,pkg="dynatop",remove="git")## this only does source files

rm(list=ls())
devtools::load_all()
w <- wmts$new("./inst/extdata/glofas.nc")
ws <- wmtsServer$new(w,"./build_scripts/template.xml")
ws$start()
