# Run in order, enter "yes" for any updates
install.packages(c("devtools",
                   "BiocManager",
                   "webr", 
                   "shinylive", 
                   "pandoc", 
                   "ggbeeswarm",
                   "git2r",
                   "stringr",
                   "purrr",
                   "dplyr",
                   "lubridate"))
# If on a Windows computer
install.packages("taskscheduler")
BiocManager::install(c("flowCore",
                       "flowWorkspace",
                       "ggcyto"))
devtools::install_github("https://github.com/DavidRach/Luciernaga",
               dependencies = TRUE)
devtools::install_local(file.path(getwd(), "FHFCQC"))
## Generate new github token: https://github.com/settings/tokens
gitcreds::gitcreds_set()
usethis::edit_r_environ() # Enter token as GITHUB_PAT="TOKENGOESHERE"

# In R studio terminal
git config --global user.email "github email"
git config --global user.name "github username"
##Task scheduling:https://www.r-bloggers.com/2018/10/how-to-run-r-from-the-task-scheduler/
## -e "source('')"