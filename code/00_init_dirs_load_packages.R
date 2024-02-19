############################################################################
### set directories and load packages
############################################################################

# Set user dependent working directories
# add user name and path to working dir to run scripts more easily...

user <- Sys.info()["user"]
path2wd <- switch(user,
                  "sb25gaqy" = "~/Dropbox/1current/microbiome-disturbance/"
)

setwd(path2wd)
   

############################################################################
### Load all needed libraries
############################################################################
# TODO: check if all these are required...
needed_libs <- c("brms",
                 "cowplot",
                 "tidyverse",
                 "devtools",
                 "tidybayes",
                 "ggridges"
)

usePackage <- function(p) {
   if (!is.element(p, installed.packages()[,1])) {   
      if(p == "mobr") {install_github('MoBiodiv/mobr')}  
      install.packages(p, dep = TRUE)
   }
   require(p, character.only = TRUE)
}

sapply(needed_libs, usePackage)
rm(usePackage)

# document setup if needed
#session_info() 
