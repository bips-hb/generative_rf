myloglik <- function(mod, dat) {
  rf <- mod$.__enclos_env__$private$rf
  params <- mod$.__enclos_env__$private$params
  node_probs <- mod$.__enclos_env__$private$node_probs
  td_long <- melt(data.table(obs = 1:nrow(dat), dat), id.vars = "obs")
  
  pred <- predict(rf, dat, type = "terminalNodes")
  preds <- rbindlist(lapply(1:ncol(pred$predictions), function(i) {
    data.table(tree = i, obs = 1:nrow(pred$predictions), nodeid = pred$predictions[, i])
  }))
  
  np <- rbindlist(lapply(1:ncol(node_probs), function(i) {
    data.table(tree = i, nodeid = 1:nrow(node_probs), prob = node_probs[, i])
  }))
  
  #aa <- merge(params, np, by = c("tree", "nodeid"))
  aa <- merge(params[!is.na(sd), ], np, by = c("tree", "nodeid"))
  bb <- merge(aa, preds, by = c("tree", "nodeid"), allow.cartesian = TRUE)
  cc <- merge(bb, td_long, by = c("obs", "variable"))
  cc[, lik := dnorm(value, mean = mean, sd = sd)]
  
  #cc[, lik := max(lik, 1e-100)]
  
  # Normalization constant
  zz <- unique(cc[, .(tree, nodeid, prob)])
  nc <- zz[, sum(prob), by = tree]
  colnames(nc)[2] <- "normconst"
  
  dd <- cc[, prod(lik), by = .(obs, tree, nodeid, prob)]
  
  ee <- merge(dd, nc, by = "tree")
  
  ee[, lik := prob/normconst * V1]
  #ee[, lik := prob * V1]
  #ee[, lik := V1]
  
  ff <- ee[, sum(lik), by = .(obs, tree)]
  
  (ff[, log(mean(V1)), by = obs]$V1)
}