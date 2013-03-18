#NOTE remember to use stable ordering everywhere
set.seed(20130311)

attempts <- read.csv("attempts.txt")
requirements <- read.csv("requirements.txt")

problems <- unique(sort(c(requirements$problem, requirements$requirement)))
users <- unique(sort(attempts$user))
users.total <- length(users)

# Extract first successful attempt for every user
successes <- attempts[attempts$result != 0,
                      c("problem", "user", "submission.timestamp")]
successes <- successes[order(successes[,"problem"],
                             successes[,"user"],
                             successes[,"submission.timestamp"]), ]
successes <- successes[!duplicated(successes[,c("problem","user")]), ]

# Choose a problem which has enough solvers, but not too many
#  (from between second and third quantiles)
successes.total <- sort(table(successes$problem))
tmp.len <- length(successes.total)
sample.problem <- sample(successes.total[(tmp.len / 2) : (3 * tmp.len / 4)], 1)
sample.problem <- as.integer(names(sample.problem))
cat("Sample problem: #", sample.problem, ", solved by ",
    successes.total[as.character(sample.problem)], " users\n", sep="")

curr.attempts <- attempts[attempts$problem == sample.problem, ]
curr.attempts <- curr.attempts[order(curr.attempts$attempt.timestamp), ]
curr.attempts <- curr.attempts[!duplicated(curr.attempts[c("user",
                                                           "result")]), ]
curr.attempts <- curr.attempts[order(curr.attempts$user), ]

solved.by.fast.users <- c()
solved.by.slow.users <- c()
curr.attempts <- curr.attempts[!duplicated(curr.attempts$user), ]
for (i in 1:nrow(curr.attempts)) {
  tmp.user <- curr.attempts$user[i]
  tmp.ts <- curr.attempts$attempt.timestamp[i]
  tmp.solved <- successes[successes$user == tmp.user &
                            successes$submission.timestamp <= tmp.ts, ]
  if (curr.attempts$result[i] == 0) {
    solved.by.slow.users <- c(solved.by.slow.users, tmp.solved$problem)
  }
  else {
    solved.by.fast.users <- c(solved.by.fast.users, tmp.solved$problem)
  }
}
slow.users.total <- sum(curr.attempts$result == 0)
fast.users.total <- sum(curr.attempts$result == 1)
solved.by.slow.users <- table(solved.by.slow.users)
solved.by.fast.users <- table(solved.by.fast.users)
all.prev.problems <- sort(union(names(solved.by.fast.users),
                                    names(solved.by.slow.users)))
total.prev.problems <- length(all.prev.problems)
deviations <- data.frame(problem=numeric(total.prev.problems),
                         diff=numeric(total.prev.problems),
                         diff.share=numeric(total.prev.problems))
for (i in 1:total.prev.problems)
{
  problem <- all.prev.problems[i]
  deviations$diff[i] <- solved.by.fast.users[problem] -
    solved.by.slow.users[problem]
  deviations$diff.share[i] <- solved.by.fast.users[problem] / fast.users.total -
    solved.by.slow.users[problem] / slow.users.total
  deviations$problem[i] <- problem
}
deviations <- deviations[order(deviations$diff.share), ]
