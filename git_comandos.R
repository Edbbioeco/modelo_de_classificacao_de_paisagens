# Pacotes ----

library(gert)

# Arquivos áptos ----

gert::git_status() |>
  as.data.frame()

# Adicionar arquivos ----

gert::git_add(files = "git_comandos.R") |>
  as.data.frame()

# Commit ----

gert::git_commit(message = "Comandos de Git")

# Push ----

gert::git_push(remote = "origin")

# Pull ----

gert::git_pull(remote = "origin")

# Reset -----

gert::git_reset_soft() |>
  as.data.frame()

gert::git_reset_soft(ref = "HEAD")
