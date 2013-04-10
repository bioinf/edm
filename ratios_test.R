attempts <- read.csv("dataset/attempts.txt")
problems <- read.csv("dataset/problems.txt")
requirements <- read.csv("dataset/requirements.txt")
users <- read.csv("dataset/users.txt")
log <- read.csv("dataset/log-views-only.txt")

attempts <- attempts[order(attempts$attempt.timestamp),]
log <- log[order(log$timestamp),]
log <- log[log$timestamp >= attempts$attempt.timestamp[1],]
attempts <- attempts[attempts$attempt.timestamp >= log$timestamp[1],]

problems <- problems[order(problems$problem),]
attempts <- attempts[attempts$problem %in% problems$problem,]
log <- log[log$problem %in% problems$problem,]

successes <- attempts[attempts$result != 0,
                      c("problem", "user", "submission.timestamp")]
successes <- successes[order(successes[,"problem"],
                             successes[,"user"],
                             successes[,"submission.timestamp"]), ]
successes <- successes[!duplicated(successes[,c("problem","user")]), ]

views.by.country <- as.matrix(table(merge(log, users, "user")[,c("problem", "country")]))
problems.by.country <- as.matrix(table(merge(successes, users, "user")[,c("problem", "country")]))

# RU and US are only two countries with >= 100 users
views.by.country <- views.by.country[,c("US", "RU")]
problems.by.country <- problems.by.country[,c("US", "RU")]

shares <- sapply(1:nrow(problems.by.country), function(i) {
  problems.by.country[i,] / views.by.country[i,]
})

result <- data.frame(us.views=views.by.country[,"US"],
                     us.solved=problems.by.country[,"US"],
                     us.share=shares["US",],
                     ru.views=views.by.country[,"RU"],
                     ru.solved=problems.by.country[,"RU"],
                     ru.share=shares["RU",])
result$share.diff <- result$us.share - result$ru.share

get.problem.name <- function(p) {
  problems$name[problems$problem==p]
}
rownames(result) <- as.character(sapply(rownames(problems.by.country),
                                        get.problem.name))

result <- result[order(abs(result$share.diff)),]