groups <- function(groups, n.groups = NULL, n) {
  ng <- length(groups)
  if (is.null(n.groups) | !is.list(n.groups)) {
    n.groups <- list(n.groups)
  }
  n.groups <- rep(n.groups, ng)
  
  for (i in 1:ng) {
    lg <- length(groups[[i]])
    if (!is.null(n.groups[[i]]))
      n.groups[[i]] <- rep(n.groups[[i]], length = lg)
    if (is.null(n.groups[[i]])) {
      n.groups[[i]] <- rep(floor(n/lg), lg)
    }
    if (sum(n.groups[[i]]) != n) {
      n.groups[[i]][length(n.groups[[i]])] <- n.groups[[i]][length(n.groups[[i]])] + n - sum(n.groups[[i]])
    }
  }
  results <- list(groups, n.groups)
  results
}

ngroups <- function(group, n.group = NULL, n) {
  ng <- length(group)
  pos.group <- c(1, 1 + cumsum(n.group))[1:ng]
  data.frame(group, pos.group, n.group, stringsAsFactors = FALSE)
}
