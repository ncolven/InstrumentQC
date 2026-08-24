install.packages(c("devtools",
                   "BiocManager",
                   "webr", 
                   "shinylive", 
                   "pandoc", 
                   "ggbeeswarm",
                   "git2r",
                   "lubridate"))

BiocManager::install(c("flowCore",
                       "flowWorkspace",
                       "ggcyto"))
devtools::install_github("https://github.com/DavidRach/Luciernaga",
               dependencies = TRUE)

## Generate new github token: https://github.com/settings/tokens
gitcreds::gitcreds_set()
usethis::edit_r_environ()

## Generate ssh key: https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent
## Add key to github: https://github.com/settings/keys

##Task scheduling:https://www.r-bloggers.com/2018/10/how-to-run-r-from-the-task-scheduler/
## -e "source('')"