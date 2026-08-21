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

## Generate new github token: https://github.com/settings/tokens
gitcreds::gitcreds_set()

## Generate ssh key: https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent
## Add key to github: https://github.com/settings/keys