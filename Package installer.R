install.packages(c("devtools",
                   "BiocManager",
                   "webr", 
                   "shinylive", 
                   "pandoc", 
                   "ggbeeswarm",
                   "git2r"))
library(devtools)
library(BiocManager)
BiocManager::install(c("flowCore",
                       "flowWorkspace",
                       "ggcyto"))
install_github("https://github.com/DavidRach/Luciernaga",
               dependencies = TRUE)