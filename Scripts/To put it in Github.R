#To push to Github?

install.packages("usethis")
library(usethis)
use_git_config(
  user.name = "eleoperret", 
  user.email = "eleonore.perret@usys.ethz.ch"
)
#Use this in the Console
usethis::create_github_token()
gitcreds::gitcreds_set()
#Set this in the Github bash
#cd setwd("C:/Users/eleop/polybox/phD/PhD/R/Seed_predation/Seed_predation_US_Github/Seed_predation_US_Github2")
#git init
#then come back to R and to this in the console
usethis::use_git()
use_github()
