#TODO(rekolobov): formatting
#TODO(rekolobov): more comments


find.deps.simple <- function(attempts.filename, problems.filename,
                             requirements.filename,
                             min.common.users, min.share) {
  
  problems <- read.csv(problems.filename)
  problems <- problems[order(problems$problem),]
  
  deps <- find.deps.matrices.simple(attempts.filename, problems.filename,
                                    requirements.filename)
  deps$share[deps$abs < min.common.users | deps$share < min.share] <- -1
  deps$share[deps$share != 1] <- 0
  problem.graph(problems.filename, requirements.filename, deps$share,
               main=paste("Possible dependencies with common users >=",
                          min.common.users, "and share >=", min.share))
  
  ret <- which(deps$share == 1, arr.ind=T)
  ret <- data.frame(from=problems$name[ret[,2]], to=problems$name[ret[,1]])
  ret <- ret[order(ret$to), ]
  rownames(ret) <- NULL
  
  return(ret)
}


find.deps.matrices.simple <- function(attempts.filename, problems.filename,
                                      requirements.filename) {
  # Computes two matrices: first containing for each pair of problems number
  #   of users who solved them both, and second containing same data as
  #   first, but with numbers in each row divided by the amount of users who
  #   solved the problem corresponding to that row and numbers in all cells
  #   corresponding to problems with existing relation set to -1.
  #
  # Args:
  #   attempts.filename: File containing the data on attempts.
  #   problems.filename: File containing known problem names.
  #   requirements.filename: File containing data on problem relations.
  #
  # Returns:
  #   List containing two matrices mentioned in description, dubbed as
  #     `abs' and `share', respectively.
  
  require("igraph")
  
  #TODO(rekolobov): Handle input param inconsistencies here
  
  attempts <- read.csv(attempts.filename)
  problems <- read.csv(problems.filename)
  requirements <- read.csv(requirements.filename)
  
  # Compute auxiliary tables
  users <- unique(sort(attempts$user))
  users.total <- length(users)
  
  # Filter and transform attempts data
  problems <- problems[order(problems$problem),]
  
  successes <- attempts[attempts$result != 0, c("problem", "user")]
  successes <- successes[successes$problem %in% problems$problem,]
  successes <- successes[order(successes[,"problem"], successes[,"user"]), ]
  successes <- successes[!duplicated(successes), ]
  rownames(successes) <- NULL
  
  solvers.by.problem <- table(successes$problem)
  solvers.by.problem <- solvers.by.problem[order(problems$problem)]
  
  #TODO(rekolobov): not very effective
  successes.by.pair <- merge(successes, successes, by="user",
                             suffixes=c(".1", ".2"))
  solvers.by.pair <- table(successes.by.pair$problem.1,
                           successes.by.pair$problem.2)
  problems.corr <- t(sapply(1:nrow(solvers.by.pair),
                            function(i) {
                              solvers.by.pair[i,] / solvers.by.problem[i]
                            }))
  solvers.by.pair <- unclass(solvers.by.pair)
  rownames(problems.corr) <- problems$problem
  colnames(problems.corr) <- problems$problem
  
  all.reqs <- graph.data.frame(requirements)
  all.reqs <- connect.neighborhood(all.reqs, order=length(V(all.reqs)),
                                   mode="out")
  
  for (v in V(all.reqs)$name) {
    indices <- c(v, V(all.reqs)[nei(v, mode="in")]$name)
    problems.corr[indices, v] <- -1
    problems.corr[v, indices] <- -1
  }
  
  rownames(solvers.by.pair) <- problems$name
  colnames(solvers.by.pair) <- problems$name
  rownames(problems.corr) <- problems$name
  colnames(problems.corr) <- problems$name
  
  return(list(abs=solvers.by.pair, share=problems.corr))
}


problem.graph <- function(problems.filename, requirements.filename,
                          possible.relations, ...) {
  # Computes and plots the problem graph.
  #
  # Args:
  #   problems.filename: File containing known problem names.
  #   requirements.filename: File containing data on problem relations.
  #   possible.relations: Named 0-1 matrix, conatining ones in cells
  #                       corresponding to problems that could be related.
  #TODO(rekolobov): doc                       Edges: column -> row.
  #                       You usually obtain this matrix by filtering
  #                       return value of a function from find.deps* family.
  #
  # Returns: NULL.
  
  require("igraph")
  require("Rgraphviz")
  
  #TODDO(rekolobov): check whether poss.relations contain the same problems as problems file!
  
  problems <- read.csv(problems.filename)
  problems <- problems[order(problems$problem),]
  requirements <- read.csv(requirements.filename)
  
  get.problem.name <- function(p) {
    problems$name[problems$problem==p]
  }
  requirements$problem <- as.character(sapply(requirements$problem,
                                              get.problem.name))
  requirements$requirement <- as.character(sapply(requirements$requirement,
                                                  get.problem.name))
  
  g0 <- graph.data.frame(cbind(requirements$requirement,
                                     requirements$problem))
  g <- igraph.to.graphNEL(g0)
  
  layers <- numeric(nrow(problems))
  names(layers) <- problems$name
  
  curr.level = 1
  curr.problems = c("DNA", "INI1", "INI")
  repeat {
    curr.dists <- shortest.paths(g0, v=curr.problems, mode="out")
    curr.problems <- colnames(curr.dists)[sapply(1:ncol(curr.dists),
                                                 function(i) {
                                                   any(curr.dists[,i] == 1)
                                                 })]
    layers[curr.problems] = curr.level
    if (length(curr.problems) == 0) {
      break
    }
    curr.level = curr.level + 1
  }
  
  edge.colors <- rep("grey", times=length(edgeNames(g)))
  names(edge.colors) <- edgeNames(g)
  
  possible.relations <- possible.relations[order(rownames(possible.relations)),
                                                 order(colnames(possible.relations))]
  problems <- problems[order(problems$name),]
  found.rels <- which(possible.relations == 1, arr.ind=T)
  found.rels[,1] <- as.character(sapply(found.rels[,1], function(x) {
    problems$name[as.integer(x)]
  }))
  found.rels[,2] <- as.character(sapply(found.rels[,2], function(x) {
    problems$name[as.integer(x)]
  }))
  g <- addEdge(found.rels[,2], found.rels[,1], g)
  new.edges.colors <- rep("red", nrow(found.rels))
  names(new.edges.colors) <- sapply(1:nrow(found.rels), function(i) {
    paste(found.rels[i,2], found.rels[i,1], sep="~")
  })
  
  subgraphs <- lapply(unique(layers), function(i) {
    list(graph=subGraph(names(layers)[layers == i], g),
         cluster=FALSE)
  })
  plot(g, attrs=list(graph=list(layout="dot"),
                     node=list(shape="ellipse", fixedsize=F, fontsize=500)),
       edgeAttrs=list(color=c(edge.colors, new.edges.colors)),
       subGList=subgraphs,
       ...)
}
