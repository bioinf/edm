#install.packages("cluster")
#install.packages("igraph")
library("cluster")
#library("igraph")

PARAM_CLUSTERS_K = 8
cat("k-means with ", PARAM_CLUSTERS_K, " centers\n", sep="")

#NOTE the seed is for demonstrative purposes only
set.seed(20130311)

attempts <- read.csv("attempts.txt")
requirements <- read.csv("requirements.txt")
all_problems <- unique(sort(c(requirements$problem,
                              requirements$requirement)))

# Extract first successful attempt for every user
successes <- attempts[attempts$result != 0,
                      c("problem", "user", "submission.timestamp")]
successes <- successes[order(successes[,"problem"],
                             successes[,"user"],
                             successes[,"submission.timestamp"]),]
successes <- successes[!duplicated(successes[,c("problem","user")]),]

# Choose a problem which has enough solvers, but not too many
#  (from between second and third quantiles)
tot_successes <- sort(table(successes$problem))
tmp_len <- length(tot_successes)
sample_problem <- sample(tot_successes[(tmp_len / 2) : (3 * tmp_len / 4)], 1)
sample_problem <- as.integer(names(sample_problem))
cat("Sample problem: #", sample_problem, ", solved by ",
    tot_successes[as.character(sample_problem)], " users\n", sep="")

sample_successes <- successes[successes$problem == sample_problem,]
solved_before <- list()
for (i in 1:nrow(sample_successes)) {
  tmp_user <- sample_successes$user[i]
  tmp_ts <- sample_successes$submission.timestamp[i]
  tmp_solved <- successes[successes$user == tmp_user
                          & successes$submission.timestamp <= tmp_ts,]
  solved_before[[as.character(tmp_user)]] <- tmp_solved$problem
}

sample_users <- as.character(sort(sample_successes$user))
tmp_len <- length(sample_users)
dists <- matrix(nrow = tmp_len, ncol = tmp_len)
for (i in 1:tmp_len) {
  for (j in 1:tmp_len) {
    user_i <- sample_users[i]
    user_j <- sample_users[j]
    dists[i,j] = dists[j,i] = length(setdiff(solved_before[[user_i]],
                                             solved_before[[user_j]])) +
                              length(setdiff(solved_before[[user_j]],
                                             solved_before[[user_i]]))
  }
}

result <- pam(as.dist(dists), PARAM_CLUSTERS_K)

clusinfo <- cbind(1:nrow(result$clusinfo), result$clusinfo)
best_cluster <- clusinfo[order(clusinfo[,"size"], decreasing=T),][,1][1]
best_cluster_center <- sample_users[result$medoids[best_cluster]]

cat("Overall info on clusters:\n")
print(result$clusinfo)

cat("Biggest cluster center: user #", best_cluster_center, "\n", sep="")
cat("Problems he solved before sample problem:\n")
print(solved_before[[best_cluster_center]])

cat("Distances between cluster's elements and its center:\n")
print(dists[result$medoids[best_cluster],(1:length(sample_users))[result$clustering==best_cluster]])

#problem_graph <- graph.edgelist(cbind(requirements$requirement,
#                                      requirements$problem))