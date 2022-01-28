# try a bunch of forester compensation packages to see how foresters mark
trees_sub <- treesgo[treesgo$plot == treesgo$plot[1],]
comp_grid <- expand.grid(gamma = c(0, 2), lambda = c(0, .1),
                         rho = c(0, 15), theta = c(0, 10), phi = c(0, .05, 1))
marking_exploration <-
  lapply(1:nrow(comp_grid), function(i) {
    print(i)
    for_opt(trees_sub, paramsg, gamma = comp_grid$gamma[i],
            lambda = comp_grid$lambda[i], rho = comp_grid$rho[i],
            theta = comp_grid$theta[i], phi = comp_grid$phi[i])
  })
marking_exploration <- as_tibble(do.call(rbind, marking_exploration))
