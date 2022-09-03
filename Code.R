RagnarPath="/home/keitaro2/Uber_Chicago/Uber_Chicago-Git"
LocalPath="C:/Users/Keitaro Ninomiya/Box/Research Notes (keitaro2@illinois.edu)/Uber_Chicago"

#Pull most recent Github version.
library(usethis)
gh_token_help()

create_from_github("https://github.com/Keitaro-Ninomiya/Uber_Chicago.git")
gitcreds::gitcreds_set() 
"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"

#