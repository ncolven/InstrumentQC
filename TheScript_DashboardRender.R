library(git2r)
username <- Sys.info()["user"]
RepositoryPath <- file.path("Users", username, "Documents", "InstrumentQC")
TheRepo <- repository(RepositoryPath)
git2r::pull(TheRepo)

library(quarto)
QuartoProject <- file.path(RepositoryPath, "InstrumentQC.Rproj")
quarto::quarto_render(input=RepositoryPath)

Today <- Sys.time()
Today <- as.Date(Today)

# Stage to Git
add(TheRepo, "*")

TheCommitMessage <- paste0("Updated dashboard on ", Today)
commit(TheRepo, message = TheCommitMessage)
cred <- cred_token(token = "GITHUB_PAT")
push(TheRepo, credentials = cred)
