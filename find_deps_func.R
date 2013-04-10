#TODO(rekolobov): commenting

find.deps <- function(attempts.filename, problems.filename,
                      requirements.filename,
                      min.users, min.share.diff) {
  
  require("igraph")
  
  result <- find.deps.matrices(attempts.filename, problems.filename,
                               requirements.filename)
  
  problems <- read.csv(problems.filename)
  requirements <- read.csv(requirements.filename)
  
  problems <- problems[order(problems$problem),]
  get.problem.name <- function(p) {
    problems$name[problems$problem==p]
  }
  requirements$problem <- as.character(sapply(requirements$problem,
                                              get.problem.name))
  requirements$requirement <- as.character(sapply(requirements$requirement,
                                                  get.problem.name))
  
  all.reqs <- graph.data.frame(requirements)
  all.reqs <- connect.neighborhood(all.reqs, order=length(V(all.reqs)),
                                   mode="out")
  
  for (v in V(all.reqs)$name) {
    indices <- c(v, V(all.reqs)[nei(v, mode="in")]$name)
    result$share.diff[indices, v] <- 0
    result$share.diff[v, indices] <- 0
  }
  result$share.diff[result$fast.users < min.users |
                    result$slow.users < min.users] <- 0
  result$share.diff[result$share.diff < min.share.diff ] <- 0
  result$share.diff[result$share.diff > 1e-10] <- 1
  
  problem.graph(problems.filename, requirements.filename, result$share.diff,
                main=paste("Complex: possible dependencies with users >=",
                           min.users, "and share difference >=",
                           min.share.diff))
  
  ret <- which(result$share.diff == 1, arr.ind=T)
  ret <- data.frame(from=problems$name[ret[,2]], to=problems$name[ret[,1]])
  ret <- ret[order(ret$to), ]
  rownames(ret) <- NULL
  
  return ret
}


find.deps.matrices <- function(attempts.filename, problems.filename,
                               requirements.filename) {
  
  #TODO(rekolobov): Handle input param inconsistencies here
  
  attempts <- read.csv(attempts.filename)
  problems <- read.csv(problems.filename)
  requirements <- read.csv(requirements.filename)
  
  problems <- problems[order(problems$problem),]
  attempts <- attempts[attempts$problem %in% problems$problem, ]
  
  successes <- attempts[attempts$result != 0,
                        c("problem", "user", "submission.timestamp")]
  successes <- successes[order(successes[,"problem"],
                               successes[,"user"],
                               successes[,"submission.timestamp"]), ]
  successes <- successes[!duplicated(successes[,c("problem","user")]), ]
  
  result <- list(share.diff = matrix(0, nrow(problems), nrow(problems)),
                 slow.users = matrix(0, nrow(problems), nrow(problems)),
                 slow.users.share = matrix(0, nrow(problems), nrow(problems)),
                 fast.users = matrix(0, nrow(problems), nrow(problems)),
                 fast.users.share = matrix(0, nrow(problems), nrow(problems)))
  for (el in names(result)) {
    rownames(result[[el]]) <- problems$problem
    colnames(result[[el]]) <- problems$problem
  }
  
  for (problem in problems$problem) {
    cat("Running for problem #", problem, "\n", sep="")
    curr.attempts <- attempts[attempts$problem == problem, ]
    curr.attempts <- curr.attempts[order(curr.attempts$user,
                                         curr.attempts$attempt.timestamp), ]
    
    slow.users.total <- 0
    solved.by.slow.users <- c()
    fast.users.total <- 0
    solved.by.fast.users <- c()
    for (u in unique(curr.attempts$user)) {
      u.attempts <- curr.attempts[curr.attempts$user == u,]
      tmp.ts <- u.attempts$attempt.timestamp[1]
      tmp.solved <- successes[successes$user == u &
                              successes$submission.timestamp <= tmp.ts, ]
      
      
      if (sum(u.attempts$result[1:2], na.rm=T) < 1) {
        slow.users.total = slow.users.total + 1
        solved.by.slow.users <- c(solved.by.slow.users, tmp.solved$problem)
      }
      else {
        fast.users.total = fast.users.total + 1
        solved.by.fast.users <- c(solved.by.fast.users, tmp.solved$problem)
      }
    }
    
    solved.by.slow.users <- table(solved.by.slow.users)
    solved.by.fast.users <- table(solved.by.fast.users)
    all.prev.problems <- sort(union(names(solved.by.fast.users),
                                    names(solved.by.slow.users)))
    
    problem <- as.character(problem)
    
    tmp.result <- solved.by.fast.users[all.prev.problems]
    tmp.result[is.na(tmp.result)] <- 0
    result$fast.users[problem, all.prev.problems] <- tmp.result
    result$fast.users.share[problem,all.prev.problems] <-
      result$fast.users[problem, all.prev.problems] / fast.users.total
    tmp.result <- solved.by.slow.users[all.prev.problems]
    tmp.result[is.na(tmp.result)] <- 0
    result$slow.users[problem, all.prev.problems] <- tmp.result
    result$slow.users.share[problem, all.prev.problems] <-
      result$slow.users[problem, all.prev.problems] / slow.users.total
  }
  
  result$share.diff <- result$fast.users.share - result$slow.users.share
  
  for (el in names(result)) {
    rownames(result[[el]]) <- problems$name
    colnames(result[[el]]) <- problems$name
  }
  
  return(result)
}
