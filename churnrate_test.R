#NOTE remember to use stable ordering everywhere
CHURNRATE.PERIOD <- 28
FILTERING.SCRIPT <- "churnrate_filter.pl"
FILTERED.FILENAME <- "attempts_filtered.txt"
FILTERED.TEMP.FILENAME <- "attempts_filtered_temp.txt"

attempts <- read.csv("attempts.txt")

users <- unique(sort(attempts$user))
##users.total <- length(users)
users.by.problem <- table(unique(attempts[, c("user", "problem")])$problem)

attempts$submission.timestamp <- NULL
attempts <- attempts[order(attempts$user, attempts$problem,
                           attempts$attempt.timestamp), ]
attempts$date <- as.Date(as.POSIXct(attempts$attempt.timestamp,
                                    origin="1970-01-01"))
attempts$attempt.timestamp <- NULL

write.table(attempts, FILTERED.TEMP.FILENAME, sep="\t",
            row.names=F, quote=F)
system(paste("perl", FILTERING.SCRIPT,
             "<", FILTERED.TEMP.FILENAME,
             ">", FILTERED.FILENAME))
file.remove(FILTERED.TEMP.FILENAME)

last.fails.daily <- read.table(FILTERED.FILENAME, header=T)
last.fails.daily$result <- NULL
last.fails.daily <- last.fails.daily[order(last.fails.daily$date,
                                           last.fails.daily$user), ]
last.fails.daily <- last.fails.daily[!duplicated(last.fails.daily[c("user",
                                                                    "date")],
                                                 fromLast=TRUE), ]
last.fails.daily$date <- as.Date(last.fails.daily$date)

last.date.seen <- last.fails.daily$date[nrow(last.fails.daily)]

churnrate.fails <- data.frame()
#NOTE couldn't avoid using loop here
for (user in users) {
  curr.fails <- rbind(last.fails.daily[last.fails.daily$user == user, ], NA)
  curr.fails$date[nrow(curr.fails)] <- last.date.seen
  churnrate.fails <- rbind(churnrate.fails,
                           curr.fails[c(diff(curr.fails$date) >=
                                          CHURNRATE.PERIOD,
                                        FALSE), ])
}
churnrate.fails <- churnrate.fails[churnrate.fails$last.failed != -1, ]

##NOTE mostly useless
##sort(table(churnrate.fails$user))

#TODO(roman): rewrite R-style
tmp.drops <- table(churnrate.fails$last.failed)
drops.by.problem <- data.frame(problem=character(length(tmp.drops)),
                               users=numeric(length(tmp.drops)),
                               drops=numeric(length(tmp.drops)),
                               drops.share=numeric(length(tmp.drops)))
drops.by.problem$problem <- names(tmp.drops)
drops.by.problem$drops <- as.integer(tmp.drops)
for (row in 1:nrow(drops.by.problem)) {
  problem <- drops.by.problem$problem[row]
  drops.by.problem$drops.share[row] <-
    drops.by.problem$drops[row] / users.by.problem[problem]
  drops.by.problem$users[row] <- as.integer(users.by.problem[problem])
}
drops.by.problem <- drops.by.problem[order(drops.by.problem$drops.share), ]
row.names(drops.by.problem) <- NULL