
# Para confirmar que el setup Github es correcto verificar que el siguiente comando devuelve:
usethis::git_sitrep()

# ── Git global (user) 
# • Name: "sebastianf101"
# • Email: "sebastianf101@gmail.com"
# • Global (user-level) gitignore file: C:/Users/sferro/.gitignore
# • Vaccinated: TRUE
# • Default Git protocol: "https"
# • Default initial branch name: "master" and "main"
# 
# ── GitHub user 
# • Default GitHub host: "https://github.com"
# • Personal access token for "https://github.com": <discovered>
#   • GitHub user: "sebastianf101"
# • Token scopes: "gist", "read:project", "repo", "user", "workflow", and "write:packages"
# • Email(s): "sebastianf101@gmail.com (primary)" and "sebastianf101@hotmail.com"
# 
# ── Active usethis project: "C:/Users/sferro/Documents/Trabajo/Projects/SmartModel/SmartModelV2" ──
# 
# ── Git local (project) 
# • Name: "sebastianf101"
# • Email: "sebastianf101@gmail.com"
# • Default branch: "main"
# • Current local branch → remote tracking branch:
#   "main" → "upstream/main"
# 
# ── GitHub project 
# • Type = "fork"
# • Host = "https://github.com"
# • Config supports a pull request = TRUE
# • origin = "sebastianf101/SmartModelV2" (can push) = fork of "sferro-besmart/SmartModelV2"
# • upstream = "sferro-besmart/SmartModelV2" (can push)
# ℹ "origin" is a fork of "sferro-besmart/SmartModelV2", which is configured as the "upstream" remote.
# ℹ Read more about the GitHub remote configurations that usethis supports at:
#   <https://happygitwithr.com/common-remote-setups.html>.

gitcreds::gitcreds_set(url = "https://sferro-besmart@github.com")
gitcreds::gitcreds_set(url = "https://sebastianf101@github.com")
gitcreds::gitcreds_get(use_cache = FALSE)
gh::gh_whoami()
gitcreds::gitcreds_set() # Opción 3 para ver el actual PAT. 

# Guía pull request con usethis
# https://usethis.r-lib.org/articles/pr-functions.html
usethis::pr_init(branch = "feature")
# ✔ Pulling changes from "upstream/main".
# ✔ Creating and switching to local branch "feature".
# ☐ Use usethis::pr_push() to create a PR.

usethis::pr_push()
# Which repo do you want to push to? 
#   
# 1: sebastianf101/SmartModelV2 = "origin" (external PR)
# 2: sferro-besmart/SmartModelV2 = "upstream" (internal PR)
# 
# Selection: 1
# ✔ Pushing "feature" branch to GitHub and setting "origin/feature" as upstream branch.
# ☐ Create PR at link given below.
# ✔ Opening URL <https://github.com/sebastianf101/SmartModelV2/compare/feature>.

usethis::pr_finish()
# ✔ Switching back to default branch ("main").
# ✔ Pulling changes from "upstream/main".
# ✔ Deleting local "feature" branch.
# ✔ PR sferro-besmart/SmartModelV2/#1 has been merged, deleting remote branch "origin/feature".
  

