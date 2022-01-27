out <- lapply(unique(treesgo$plot), function(i) {
  treesg <- dplyr::filter(treesgo, plot == i)
  treelength <- nrow(treesg)

  cutyr <- genoud(objective,
                  nvars = treelength,
                  max = TRUE,
                  pop.size = 50,
                  max.generations = 50,
                  wait.generations = 6,
                  hard.generation.limit = TRUE,
                  Domains = matrix(c(rep(0, treelength),
                                     rep(paramsg$endyr / paramsg$steplength,
                                         treelength)),
                                   ncol = 2),
                  solution.tolerance = 2,
                  boundary.enforcement = 2,
                  data.type.int = TRUE,
                  print.level = 1,
                  trees = treesg,
                  params = paramsg)$par

  return(data.frame(tree = treesg$tree, cutyr = cutyr))
})

cutyrs <- do.call(rbind, out)
treesdone <- dplyr::left_join(trees, cutyrs, by = "tree") %>%
  mutate(cutyr = cutyr * paramsg$steplength)
