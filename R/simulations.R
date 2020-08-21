### SIMULATIONS ###

# i = numéro de la simulation
one.simu <- function(i, X,
                     n = 10^2,
                     sigma = 1,
                     beta = c(0,1))
{
  ### STOP ###
  if(length(beta) != 2){stop('erreur de dimension')}
  if(ncol(X) != 2){stop('erreur de dimension')}
  if(nrow(X) != n){stop('erreur de dimension')}

  # Noise
  epsilon <- rnorm(n = n, sd = sigma)

  # Output
  Y <- X %*% beta + epsilon

  # Création du dataset
  dataset <- data.frame(Y, X)

  # Multiple linear regression model (without intercept)
  linear_regression_model <- lm(Y ~ X1 + X2 - 1, data = dataset)

  # Estimators
  tmp <- nnls2d::pcls2(X = X, Y = Y)
  beta_nnls <- tmp$beta
  beta_lm <- as.list(linear_regression_model$coefficients)

  # Results
  df <- data.frame(n, sigma, beta[1], beta[2],
                   beta_nnls[1], beta_nnls[2],
                   beta_lm[1], beta_lm[2])

  colnames(df) <- c("n", "sigma", "beta_1", "beta_2",
                    "beta_nnls_1", "beta_nnls_2",
                    "beta_lm_1", "beta_lm_2")
  return(df)
}









simulations <- function(cores, liste_n, liste_sigma, beta = c(0,1),
                        nb_one_simu = 250, classes_hist = 50, alpha = 0.05,
                        FUN = one.simu)
{



  ### Calcul parallèle ###



  registerDoParallel(cores = cores)



  ### Initialisation des listes ###



  taille <- length(liste_n)*length(liste_sigma)

  # Contient n, sigma, beta_1, beta_2, beta_nnls_1, beta_nnls_2,
  # beta_lm_1 et beta_lm_2 pour chacune des simulations
  liste_final_df <- vector("list", taille)


  # Histogrammes de beta_nnls_1 et beta_nnls_2
  liste_plot_nnls_1 <- vector("list", taille)
  liste_plot_nnls_2 <- vector("list", taille)

  # Histogrammes de beta_lm_1 et beta_lm_2
  liste_plot_lm_1 <- vector("list", taille)
  liste_plot_lm_2 <- vector("list", taille)


  # Regroupe beta_nnls_1 et beta_lm_1
  # pour construire les histogrammes comparatifs
  liste_comparison_1 <- vector("list", taille)
  # Sans les valeurs nulles pour nnls
  liste_comparison_1_without_0 <- vector("list", taille)



  # Regroupe beta_nnls_2 et beta_lm_2
  # pour construire les histogrammes comparatifs
  liste_comparison_2 <- vector("list", taille)
  # Sans les valeurs nulles pour nnls
  liste_comparison_2_without_0 <- vector("list", taille)

  # Histogrammes comparatifs
  liste_plot_comparison_1 <- vector("list", taille)
  liste_plot_comparison_2 <- vector("list", taille)
  # Sans le pic en 0 pour nnls
  liste_plot_comparison_1_without_0 <- vector("list", taille)
  liste_plot_comparison_2_without_0 <- vector("list", taille)

  # Contient biais, variance et MSE pour chacune des simulations
  liste_resultats_df <- vector("list", taille)
  liste_comments_resultats_df <- vector("list", taille)

  # Contient biais, variance et MSE théoriques pour chacune des simulations*
  liste_resultats_theoriques_df <- vector("list", taille)
  liste_comments_resultats_theoriques_df <- vector("list", taille)



  # Rapport d'amplitude des IC
  # pour la première composante de chaque estimateur
  liste_rapports_1 <- vector("list", taille)
  liste_comments_rapports_1 <- vector("list", taille)

  # Rapport d'amplitude des IC
  # pour la deuxième composante de chaque estimateur
  liste_rapports_2 <- vector("list", taille)
  liste_comments_rapports_2 <- vector("list", taille)

  # IC au niveau de confiance 1 - alpha
  liste_IC_df <- vector("list", taille)
  liste_comments_IC_df <- vector("list", taille)



  for(n in 1:length(liste_n))
  {
    X <- matrix(runif(liste_n[n]*2), nrow = liste_n[n], ncol = 2)
    for(sigma in 1:length(liste_sigma))
    {

      finaldf <- foreach(i = 1:nb_one_simu, .combine = rbind) %dopar%
        {
          tempdf <- FUN(i = i, X = X, n = liste_n[n],
                        sigma = liste_sigma[sigma], beta = beta)
          tempdf
        }

      # Indice pour insérer dans les listes
      indice <- (n - 1) * length(liste_sigma) + sigma

      # Pour centrer les titres
      theme_update(plot.title = element_text(hjust = 0.5))

      # Pour les titres des histogrammes
      #tmp0_nnls <- paste(
      #  "Distribution de beta_nnls_1 pour la simulation no.", indice)
      #tmp1_nnls <- paste(
      #  "Distribution de beta_nnls_2 pour la simulation no.", indice)
#
      #tmp0_lm <- paste(
      #  "Distribution de beta_lm_1 pour la simulation no.", indice)
      #tmp1_lm <- paste(
      #  "Distribution de beta_lm_2 pour la simulation no.", indice)

      tmp0_nnls <- "Distribution de beta_nnls_1"
      tmp1_nnls <- "Distribution de beta_nnls_2"
      
      tmp0_lm <- "Distribution de beta_lm_1"
      tmp1_lm <- "Distribution de beta_lm_2"
      
      #tmp_comparison <- paste(
      #  "Comparaison des distributions de")
      #tmp0_comparison <- paste(
      #  "beta_nnls_1 et de beta_lm_1 pour la simulation no.", indice)
      #tmp1_comparison <-
      #  paste(
      #    "beta_nnls_2 et de beta_lm_2 pour la simulation no.", indice)
#
      #tmp0_comparison_without_0 <-
      #  paste("beta_nnls_1 (valeurs > 0) et de beta_lm_1 pour la simulation no.",
      #        indice)
      #tmp1_comparison_without_0 <-
      #  paste("beta_nnls_2 (valeurs > 0) et de beta_lm_2 pour la simulation no.",
      #        indice)

      tmp_comparison <- "Comparaison des distributions de"
      tmp0_comparison <- "beta_nnls_1 et de beta_lm_1"
      tmp1_comparison <- "beta_nnls_2 et de beta_lm_2"
      
      tmp0_comparison_without_0 <- "beta_nnls_1 (valeurs > 0) et de beta_lm_1"
      tmp1_comparison_without_0 <- "beta_nnls_2 (valeurs > 0) et de beta_lm_2"
      
      tmp2 <- paste("nombre de répétitions = ", nb_one_simu)
      tmp3 <- paste("n = " , liste_n[n])
      tmp4 <- paste("sigma = ", liste_sigma[sigma])
      tmp5 <- paste("nombre de classes = ", classes_hist)
      tmp6 <- paste("beta_1 = ", beta[1])
      tmp7 <- paste("beta_2 = ", beta[2])
      
      

      tmp5_comparison <-
        paste("nombre de classes = ", floor(classes_hist/2))


      # On insère les résultats
      # dans une liste préalablement initialisée
      liste_final_df[[indice]] <- finaldf



      ### Histogrammes ###


      # nnls_1
      liste_plot_nnls_1[[indice]] <- ggplot(data = finaldf,
                                            aes(x = beta_nnls_1,
                                                y = ..density..)) +
        geom_histogram(bins = classes_hist, col = "black",
                       fill = "blue") + xlab("beta_nnls_1") +
        ggtitle(paste0(tmp0_nnls, "\n", tmp2, ", ",
                       tmp3, ", ", tmp4, "\n", tmp5, "\n", tmp6, ", ", tmp7))

      # nnls_2
      liste_plot_nnls_2[[indice]] <- ggplot(data = finaldf,
                                            aes(x = beta_nnls_2,
                                                y = ..density..)) +
        geom_histogram(bins = classes_hist, col = "black",
                       fill = "purple") + xlab("beta_nnls_2") +
        ggtitle(paste0(tmp1_nnls, "\n", tmp2, ", ",
                       tmp3, ", ", tmp4, "\n", tmp5, "\n", tmp6, ", ", tmp7))

      # lm_1
      liste_plot_lm_1[[indice]] <- ggplot(data = finaldf,
                                          aes(x = beta_lm_1,
                                              y = ..density..)) +
        geom_histogram(bins = classes_hist, col = "black",
                       fill = "red") + xlab("beta_lm_1") +
        ggtitle(paste0(tmp0_lm, "\n", tmp2, ", ",
                       tmp3, ", ", tmp4, "\n", tmp5, "\n", tmp6, ", ", tmp7))

      # lm_2
      liste_plot_lm_2[[indice]] <- ggplot(data = finaldf,
                                          aes(x = beta_lm_2,
                                              y = ..density..)) +
        geom_histogram(bins = classes_hist, col = "black",
                       fill = "green") + xlab("beta_lm_2") +
        ggtitle(paste0(tmp1_lm, "\n", tmp2, ", ",
                       tmp3, ", ", tmp4, "\n", tmp5, "\n", tmp6, ", ", tmp7))



      # Comparaison entre nnls et lm
      liste_comparison_1[[indice]] <-
        data.frame(beta_1 = c(finaldf$beta_nnls_1, finaldf$beta_lm_1),
                   model = c(rep("nnls", length(finaldf$beta_nnls_1)),
                             rep("lm", length(finaldf$beta_lm_1))))

      liste_comparison_2[[indice]] <-
        data.frame(beta_2 = c(finaldf$beta_nnls_2, finaldf$beta_lm_2),
                   model = c(rep("nnls", length(finaldf$beta_nnls_2)),
                             rep("lm", length(finaldf$beta_lm_2))))


      # Histogrammes comparatifs
      liste_plot_comparison_1[[indice]] <-
        ggplot(liste_comparison_1[[indice]],
               aes(x = beta_1, y = ..density..)) +
        geom_histogram(data =
                         subset(liste_comparison_1[[indice]],
                                model == "nnls"),
                       aes(fill = model),
                       bins = floor(classes_hist/2), alpha = 0.2) +
        geom_histogram(data =
                         subset(liste_comparison_1[[indice]],
                                model == "lm"),
                       aes(fill = model),
                       bins = floor(classes_hist/2), alpha = 0.2) +
        scale_fill_manual(name = "model", values =
                            c("red","blue")) +
        ggtitle(paste0(tmp_comparison, " ",
                       tmp0_comparison, "\n",
                       tmp2, ", ", tmp3, ", ",
                       tmp4, "\n", tmp5_comparison, "\n", tmp6, ", ", tmp7))




      liste_plot_comparison_2[[indice]] <-
        ggplot(liste_comparison_2[[indice]],
               aes(x = beta_2, y = ..density..)) +
        geom_histogram(data =
                         subset(liste_comparison_2[[indice]],
                                model == "nnls"),
                       aes(fill = model),
                       bins = floor(classes_hist/2), alpha = 0.2) +
        geom_histogram(data =
                         subset(liste_comparison_2[[indice]],
                                model == "lm"),
                       aes(fill = model),
                       bins = floor(classes_hist/2), alpha = 0.2) +
        scale_fill_manual(name = "model", values =
                            c("green","purple")) +
        ggtitle(paste0(tmp_comparison, " ",
                       tmp1_comparison, "\n",
                       tmp2, ", ", tmp3, ", ",
                       tmp4, "\n", tmp5_comparison, "\n", tmp6, ", ", tmp7))


      # Histogrammes comparatifs sans le pic en 0

      # beta_nnls sans les valeurs nulles
      beta_nnls_1_without_0 <-
        finaldf$beta_nnls_1[finaldf$beta_nnls_1 > 0]
      beta_nnls_2_without_0 <-
        finaldf$beta_nnls_2[finaldf$beta_nnls_2 > 0]

      # Comparaison entre nnls (sans les valeurs nulles) et lm
      liste_comparison_1_without_0[[indice]] <-
        data.frame(beta_1 = c(beta_nnls_1_without_0,
                              finaldf$beta_lm_1),
                   model = c(rep("nnls",
                                 length(beta_nnls_1_without_0)),
                             rep("lm", length(finaldf$beta_lm_1))))

      liste_comparison_2_without_0[[indice]] <-
        data.frame(beta_2 = c(beta_nnls_2_without_0,
                              finaldf$beta_lm_2),
                   model = c(rep("nnls",
                                 length(beta_nnls_2_without_0)),
                             rep("lm", length(finaldf$beta_lm_2))))

      # Histogrammes
      liste_plot_comparison_1_without_0[[indice]] <-
        ggplot(liste_comparison_1_without_0[[indice]],
               aes(x = beta_1, y = ..density..)) +
        geom_histogram(
          data =
            subset(liste_comparison_1_without_0[[indice]],
                   model == "nnls"),
          aes(fill = model), bins = classes_hist, alpha = 0.2) +
        geom_histogram(
          data =
            subset(liste_comparison_1_without_0[[indice]],
                   model == "lm"),
          aes(fill = model), bins = classes_hist, alpha = 0.2) +
        scale_fill_manual(name = "model",
                          values = c("red","blue")) +
        ggtitle(paste0(tmp_comparison, "\n",
                       tmp0_comparison_without_0, "\n",
                       tmp2, ", ", tmp3, ", ",
                       tmp4, "\n", tmp5, "\n", tmp6, ", ", tmp7))

      liste_plot_comparison_2_without_0[[indice]] <-
        ggplot(liste_comparison_2_without_0[[indice]],
               aes(x = beta_2, y = ..density..)) +
        geom_histogram(
          data =
            subset(liste_comparison_2_without_0[[indice]],
                   model == "nnls"),
          aes(fill = model), bins = classes_hist, alpha = 0.2) +
        geom_histogram(
          data =
            subset(liste_comparison_2_without_0[[indice]],
                   model == "lm"),
          aes(fill = model), bins = classes_hist, alpha = 0.2) +
        scale_fill_manual(name = "model",
                          values = c("green","purple")) +
        ggtitle(paste0(tmp_comparison, "\n",
                       tmp1_comparison_without_0, "\n",
                       tmp2, ", ", tmp3, ", ",
                       tmp4, "\n", tmp5, "\n", tmp6, ", ", tmp7))




      ### Biais ###




      # Calcul du biais (nnls)
      biais_nnls_1 <- mean(finaldf$beta_nnls_1) - beta[1]
      biais_nnls_2 <- mean(finaldf$beta_nnls_2) - beta[2]

      # Calcul du biais (lm)
      biais_lm_1 <- mean(finaldf$beta_lm_1) - beta[1]
      biais_lm_2 <- mean(finaldf$beta_lm_2) - beta[2]

      # On rassemble les résultats dans un dataframe
      biais_list <- c(biais_nnls_1, biais_lm_1, biais_nnls_2, biais_lm_2)
      biais_df <- data.frame(biais = biais_list)
      row.names(biais_df) <- c("nnls_1", "lm_1", "nnls_2", "lm_2")



      ### Variance ###



      # Calcul de la variance (nnls)
      var_nnls_1 <- var(finaldf$beta_nnls_1)
      var_nnls_2 <- var(finaldf$beta_nnls_2)

      # Calcul de la variance (lm)
      var_lm_1 <- var(finaldf$beta_lm_1)
      var_lm_2 <- var(finaldf$beta_lm_2)

      # On rassemble les résultats dans un dataframe
      var_list <- c(var_nnls_1, var_lm_1, var_nnls_2, var_lm_2)
      var_df <- data.frame(variance = var_list)
      row.names(var_df) <- c("nnls_1", "lm_1", "nnls_2", "lm_2")



      ### MSE ###



      # Calcul de l'EQM, i.e. la MSE (nnls)
      MSE_nnls_1 <- var_nnls_1 + biais_nnls_1^2
      MSE_nnls_2 <- var_nnls_2 + biais_nnls_2^2

      # Calcul de l'EQM, i.e. la MSE (lm)
      MSE_lm_1 <- var_lm_1 + biais_lm_1^2
      MSE_lm_2 <- var_lm_2 + biais_lm_2^2

      # On rassemble les résultats dans un dataframe
      MSE_list <- c(MSE_nnls_1, MSE_lm_1, MSE_nnls_2, MSE_lm_2)
      MSE_df <- data.frame(MSE = MSE_list)
      row.names(MSE_df) <- c("nnls_1", "lm_1", "nnls_2", "lm_2")



      ### Biais, variance et MSE dans un unique dataframe ###



      biais_var_MSE_df_tmp <- cbind(biais_df, var_df, MSE_df)

      # On insère ce dataframe dans une liste préalablement initialisée
      liste_resultats_df[[indice]] <- biais_var_MSE_df_tmp
      liste_resultats_theoriques_df[[indice]] <-
        pcls2_bias_variance_MSE(X = X, beta = beta, sigma = liste_sigma[[sigma]])

      # Légende
      comment(liste_resultats_df[[indice]]) <-
        paste0("Simulation no.", indice,
               "\n", "Nombre de répétitions = ", nb_one_simu,
               "\n", "n = ", liste_n[n],
               "\n", "sigma = ", liste_sigma[sigma])

      comment(liste_resultats_theoriques_df[[indice]]) <-
        paste0("Résultats théoriques attendus pour la simulation no.", indice,
               "\n", "n = ", liste_n[n],
               "\n", "sigma = ", liste_sigma[sigma])

      # On stocke la légende dans une liste préalablement initialisée
      liste_comments_resultats_df[[indice]] <-
        comment(liste_resultats_df[[indice]])

      liste_comments_resultats_theoriques_df[[indice]] <-
        comment(liste_resultats_theoriques_df[[indice]])





      ### Confidence intervals ###



      # Calculs intermédiaires
      borne_inf <- alpha/2
      borne_sup <- 1 - alpha/2
      bornes <- c(borne_inf, borne_sup)

      # Calcul des bornes de l'IC (nnls)
      quantiles_nnls_1 <- quantile(finaldf$beta_nnls_1, probs = bornes)
      quantiles_nnls_2 <- quantile(finaldf$beta_nnls_2, probs = bornes)

      # Calcul des bornes de l'IC (lm)
      quantiles_lm_1 <- quantile(finaldf$beta_lm_1, probs = bornes)
      quantiles_lm_2 <- quantile(finaldf$beta_lm_2, probs = bornes)

      # IC (nnls)
      IC_nnls_1 <- data.frame(
        c(quantiles_nnls_1[[1]], quantiles_nnls_1[[2]]))
      IC_nnls_2 <- data.frame(
        c(quantiles_nnls_2[[1]], quantiles_nnls_2[[2]]))

      # IC (lm)
      IC_lm_1 <- data.frame(c(quantiles_lm_1[[1]], quantiles_lm_1[[2]]))
      IC_lm_2 <- data.frame(c(quantiles_lm_2[[1]], quantiles_lm_2[[2]]))

      # Pour comparer plus facilement les IC obtenus
      IC_df_tmp <- cbind(IC_nnls_1, IC_lm_1, IC_nnls_2, IC_lm_2)
      colnames(IC_df_tmp) <-
        c("IC_nnls_1", "IC_lm_1", "IC_nnls_2", "IC_lm_2")
      row.names(IC_df_tmp) <- c("Borne inf", "Borne sup")

      # On stocke l'IC calculé dans une liste préalablement initialisée
      liste_IC_df[[indice]] <- IC_df_tmp

      # Légende
      comment(liste_IC_df[[indice]]) <-
        paste0("Simulation no.", indice,
               "\n", "Nombre de répétitions = ", nb_one_simu,
               "\n", "n = ", liste_n[n],
               "\n", "sigma = ", liste_sigma[sigma])

      # On stocke la légende dans une liste préalablement initialisée
      liste_comments_IC_df[[indice]] <-
        comment(liste_IC_df[[indice]])



      ### Rapports d'amplitude ###



      # Calcul
      rapport_1 <- diff(IC_df_tmp$IC_nnls_1)/
        diff(IC_df_tmp$IC_lm_1)

      rapport_2 <- diff(IC_df_tmp$IC_nnls_2)/
        diff(IC_df_tmp$IC_lm_2)

      # On récapitule les résultats dans un dataframe
      # qu'on stocke dans une liste préalablement initialisée
      liste_rapports_1[[indice]] <- rapport_1
      liste_rapports_2[[indice]] <- rapport_2

      # Légende
      comment(liste_rapports_1[[indice]]) <-
        paste0("Simulation no.", indice,
               "\n", "Nombre de répétitions = ", nb_one_simu,
               "\n", "n = ", liste_n[n],
               "\n", "sigma = ", liste_sigma[sigma])

      comment(liste_rapports_2[[indice]]) <-
        paste0("Simulation no.", indice,
               "\n", "Nombre de répétitions = ", nb_one_simu,
               "\n", "n = ", liste_n[n],
               "\n", "sigma = ", liste_sigma[sigma])

      # On stocke la légende dans une liste préalablement initialisée
      liste_comments_rapports_1[[indice]] <-
        comment(liste_rapports_1[[indice]])

      liste_comments_rapports_2[[indice]] <-
        comment(liste_rapports_2[[indice]])
    }
  }
  # Stop cluster
  stopImplicitCluster()



  return(
    list(
      liste_final_df = liste_final_df,
      liste_plot_nnls_1 = liste_plot_nnls_1,
      liste_plot_nnls_2 = liste_plot_nnls_2,
      liste_plot_lm_1 = liste_plot_lm_1,
      liste_plot_lm_2 = liste_plot_lm_2,
      liste_comparison_1 = liste_comparison_1,
      liste_comparison_2 = liste_comparison_2,
      liste_plot_comparison_1 = liste_plot_comparison_1,
      liste_plot_comparison_2 = liste_plot_comparison_2,
      liste_comparison_1_without_0 =
        liste_comparison_1_without_0,
      liste_comparison_2_without_0 =
        liste_comparison_2_without_0,
      liste_plot_comparison_1_without_0 =
        liste_plot_comparison_1_without_0,
      liste_plot_comparison_2_without_0 =
        liste_plot_comparison_2_without_0,
      liste_resultats_df = liste_resultats_df,
      liste_comments_resultats_df = liste_comments_resultats_df,
      liste_resultats_theoriques_df =
        liste_resultats_theoriques_df,
      liste_comments_resultats_theoriques_df =
        liste_comments_resultats_theoriques_df,
      liste_IC_df = liste_IC_df,
      liste_comments_IC_df = liste_comments_IC_df,
      liste_rapports_1 = liste_rapports_1,
      liste_comments_rapports_1 = liste_comments_rapports_1,
      liste_rapports_2 = liste_rapports_2,
      liste_comments_rapports_2 = liste_comments_rapports_2
    )
  )
}






### BOOTSTRAP ###



one.sample <- function(Y, X, beta = c(0,0), sigma = 1,
                       prop = 0.7, replace = TRUE)
{
  ### STOP ###
  if(ncol(X) != 2){stop('erreur de dimension')}
  if(nrow(X) != nrow(Y)){stop('erreur de dimension')}

  # Création du jeu de données initial
  df <- data.frame(Y, X)
  colnames(df) <- c("Y","X1", "X2")

  # Création du jeu de données rééchantillonné
  sample_size  <- ceiling(nrow(X) * prop)
  dataset <- df %>% sample_n(replace = replace, size = sample_size)

  # On demande des data.frame dans pcls2
  # De plus, ncol(Y) = NULL si c'est une liste
  Y_sample <- as.data.frame(dataset$Y)
  X1_sample <- dataset$X1
  X2_sample <- dataset$X2
  X_sample <- as.data.frame(cbind(X1_sample, X2_sample))

  # nnls
  tmp <- nnls2d::pcls2(X = X_sample, Y = Y_sample)
  beta_nnls_sample <- tmp$beta

  # Multiple linear regression
  linear_regression_model_sample <- lm(Y ~ X1 + X2 - 1, data = dataset)
  beta_lm_sample <- as.list(linear_regression_model_sample$coefficients)

  # Results
  df <- data.frame(nrow(X), sigma, beta[1], beta[2],
                   beta_nnls_sample[1], beta_nnls_sample[2],
                   beta_lm_sample[1], beta_lm_sample[2])
  colnames(df) <- c("n", "sigma", "beta_1", "beta_2",
                    "beta_nnls_sample_1", "beta_nnls_sample_2",
                    "beta_lm_sample_1", "beta_lm_sample_2" )

  return(list(results = df, model = linear_regression_model_sample))
}









bootstrap <- function(n = 10^3, sigma = 1, beta = c(0,0), prop = 0.7,
                      replace = TRUE, B = 10)
{
  # Création du jeu de données
  tmp <- nnls2d::dataGenerator(n = n, sigma = sigma, beta = beta)

  # On renomme pour plus de lisibilité
  Y <- tmp$Y
  X <- tmp$X
  epsilon <- tmp$epsilon

  # Bootstrap : on répète le rééchantillonnage B fois
  results <- replicate(B, one.sample(Y = Y, X = X, prop = prop,
                                     replace = replace, sigma = sigma))
  return(results)
}









beta_list <- function(n = 10^3, sigma = 1, beta = c(0,0), prop = 0.7,
                      replace = TRUE, B = 10, alpha = 0.05)
{
  ### Initialisation ###


  list_beta_nnls_1 <- vector("list", B)
  list_beta_nnls_2 <- vector("list", B)
  list_beta_lm_1 <- vector("list", B)
  list_beta_lm_2 <- vector("list", B)



  ### Bootstrap ###



  tmp <- bootstrap(n = n, sigma = sigma, beta = beta, prop = prop,
                   replace = replace, B = B)

  # Stockage des résultats dans des listes
  for(k in 1:B)
  {
    list_beta_nnls_1[[k]] <- tmp[,k]$results[["beta_nnls_sample_1"]]
    list_beta_nnls_2[[k]] <- tmp[,k]$results[["beta_nnls_sample_2"]]

    list_beta_lm_1[[k]] <- tmp[,k]$results[["beta_lm_sample_1"]]
    list_beta_lm_2[[k]] <- tmp[,k]$results[["beta_lm_sample_2"]]

    # Autre écriture possible
    #list_beta_nnls_1[[k]] <- tmp[,k]$results[,5]
    #list_beta_nnls_2[[k]] <- tmp[,k]$results[,6]

    #list_beta_lm_1[[k]] <- tmp[,k]$results[,7]
    #list_beta_lm_2[[k]] <- tmp[,k]$results[,8]
  }

  # Pour obtenir une "vraie" liste et pas une "liste de listes"
  list_beta_nnls_1 <- unlist(list_beta_nnls_1)
  list_beta_nnls_2 <- unlist(list_beta_nnls_2)
  list_beta_lm_1 <- unlist(list_beta_lm_1)
  list_beta_lm_2 <- unlist(list_beta_lm_2)

  # On rassemble les beta obtenus par nnls dans un dataframe
  beta_nnls <- as.data.frame(cbind(list_beta_nnls_1, list_beta_nnls_2))
  colnames(beta_nnls) <- c("beta_nnls_1", "beta_nnls_2")

  # Idem avec lm
  beta_lm <- as.data.frame(cbind(list_beta_lm_1, list_beta_lm_2))
  colnames(beta_lm) <- c("beta_lm_1", "beta_lm_2")

  # Comparaison entre nnls et lm
  comparison_1 <-
    data.frame(beta_1 = c(beta_nnls$beta_nnls_1, beta_lm$beta_lm_1),
               model=c(rep("nnls", length(beta_nnls$beta_nnls_1)),
                       rep("lm", length(beta_lm$beta_lm_1))))

  comparison_2 <-
    data.frame(beta_2 = c(beta_nnls$beta_nnls_2, beta_lm$beta_lm_2),
               model = c(rep("nnls", length(beta_nnls$beta_nnls_2)),
                         rep("lm", length(beta_lm$beta_lm_2))))




  ### Biais ###



  # Calcul du biais (nnls)
  biais_nnls_1 <- mean(beta_nnls$beta_nnls_1) - beta[1]
  biais_nnls_2 <- mean(beta_nnls$beta_nnls_2) - beta[2]

  # Calcul du biais (lm)
  biais_lm_1 <- mean(beta_lm$beta_lm_1) - beta[1]
  biais_lm_2 <- mean(beta_lm$beta_lm_2) - beta[2]

  # On rassemble les résultats dans un dataframe
  biais_list <- c(biais_nnls_1, biais_lm_1, biais_nnls_2, biais_lm_2)
  biais_df <- data.frame(biais = biais_list)
  row.names(biais_df) <- c("nnls_1", "lm_1", "nnls_2", "lm_2")



  ### Variance ###



  # Calcul de la variance (nnls)
  var_nnls_1 <- var(beta_nnls$beta_nnls_1)
  var_nnls_2 <- var(beta_nnls$beta_nnls_2)

  # Calcul de la variance (lm)
  var_lm_1 <- var(beta_lm$beta_lm_1)
  var_lm_2 <- var(beta_lm$beta_lm_2)

  # On rassemble les résultats dans un dataframe
  var_list <- c(var_nnls_1, var_lm_1, var_nnls_2, var_lm_2)
  var_df <- data.frame(variance = var_list)
  row.names(var_df) <- c("nnls_1", "lm_1", "nnls_2", "lm_2")



  ### MSE ###



  # Calcul de l'EQM, i.e. la MSE (nnls)
  MSE_nnls_1 <- var_nnls_1 + biais_nnls_1^2
  MSE_nnls_2 <- var_nnls_2 + biais_nnls_2^2

  # Calcul de l'EQM, i.e. la MSE (lm)
  MSE_lm_1 <- var_lm_1 + biais_lm_1^2
  MSE_lm_2 <- var_lm_2 + biais_lm_2^2

  # On rassemble les résultats dans un dataframe
  MSE_list <- c(MSE_nnls_1, MSE_lm_1, MSE_nnls_2, MSE_lm_2)
  MSE_df <- data.frame(MSE = MSE_list)
  row.names(MSE_df) <- c("nnls_1", "lm_1", "nnls_2", "lm_2")



  ### Confidence intervals ###



  # Calculs intermédiaires
  borne_inf <- alpha/2
  borne_sup <- 1 - alpha/2
  bornes <- c(borne_inf, borne_sup)

  # Calcul des bornes de l'IC (nnls)
  quantiles_nnls_1 <- quantile(beta_nnls$beta_nnls_1, probs = bornes)
  quantiles_nnls_2 <- quantile(beta_nnls$beta_nnls_2, probs = bornes)

  # Calcul des bornes de l'IC (lm)
  quantiles_lm_1 <- quantile(beta_lm$beta_lm_1, probs = bornes)
  quantiles_lm_2 <- quantile(beta_lm$beta_lm_2, probs = bornes)

  # IC (nnls)
  IC_nnls_1 <- data.frame(c(quantiles_nnls_1[[1]],quantiles_nnls_1[[2]]))
  IC_nnls_2 <- data.frame(c(quantiles_nnls_2[[1]], quantiles_nnls_2[[2]]))

  # IC (lm)
  IC_lm_1 <- data.frame(c(quantiles_lm_1[[1]], quantiles_lm_1[[2]]))
  IC_lm_2 <- data.frame(c(quantiles_lm_2[[1]], quantiles_lm_2[[2]]))

  # Pour comparer plus facilement les IC obtenus
  IC_df <- cbind(IC_nnls_1, IC_lm_1, IC_nnls_2, IC_lm_2)
  colnames(IC_df) <- c("IC_nnls_1", "IC_lm_1", "IC_nnls_2", "IC_lm_2")
  row.names(IC_df) <- c("Borne inf", "Borne sup")

  return(list(beta_nnls = beta_nnls, beta_lm = beta_lm,
              comparison_1 = comparison_1, comparison_2 = comparison_2,
              biais_df = biais_df, var_df = var_df,
              MSE_df = MSE_df, IC_df = IC_df))
}









distribution_beta <- function(n = 10^3, sigma = 1, beta = c(0,0),
                              prop = 0.7, replace = TRUE, B = 10,
                              alpha = 0.05, nb_classes = 50)
{
  ### Bootstrap et calcul des beta ###



  beta_list <- beta_list(n = n, sigma =  sigma, beta = beta, prop = prop,
                         replace = replace, B = B, alpha = alpha)

  # On renomme les variables pour plus de lisibilité
  beta_nnls <- beta_list$beta_nnls
  beta_lm <- beta_list$beta_lm

  comparison_1 <- beta_list$comparison_1
  comparison_2 <- beta_list$comparison_2

  biais_df <- beta_list$biais_df
  var_df <- beta_list$var_df

  MSE_df <- beta_list$MSE_df
  IC_df <- beta_list$IC_df



  ### Histogrammes ###


  # Pour les titres des histogrammes
  tmp0_nnls <- paste(
    "Distribution de beta_nnls_1")
  tmp1_nnls <- paste(
    "Distribution de beta_nnls_2")

  tmp0_lm <- paste(
    "Distribution de beta_lm_1")
  tmp1_lm <- paste(
    "Distribution de beta_lm_2")

  tmp_comparison <- paste(
    "Comparaison des distributions de")
  tmp0_comparison <- paste(
    "beta_nnls_1 et de beta_lm_1")
  tmp1_comparison <-
    paste(
      "beta_nnls_2 et de beta_lm_2 ")

  tmp0_comparison_without_0 <-
    paste("beta_nnls_1 (valeurs > 0) et de beta_lm_1")
  tmp1_comparison_without_0 <-
    paste("beta_nnls_2 (valeurs > 0) et de beta_lm_2")


  tmp2 <- paste("nombre de rééchantillonnages = ", B)
  tmp6 <- paste("proportion des données utilisée = ", prop)
  tmp3 <- paste("n = " , n)
  tmp4 <- paste("sigma = ", sigma)
  tmp5 <- paste("nombre de classes = ", nb_classes)


  tmp5_comparison <-
    paste("nombre de classes = ", floor(nb_classes/2))



  # Histogrammes nnls
  beta_nnls_1_hist <- ggplot(data = beta_nnls, aes(beta_nnls_1)) +
    geom_histogram(aes(y = ..density..), col = "black", fill = "blue",
                   bins = nb_classes) + xlab("beta_nnls_1") +
    ggtitle(paste0(tmp0_nnls, "\n", tmp2,
                   ", ", tmp6, "\n",
                   tmp3, ", ", tmp4, ", ", tmp5))

  beta_nnls_2_hist <- ggplot(data = beta_nnls, aes(beta_nnls_2)) +
    geom_histogram(aes(y = ..density..), col = "black", fill = "purple",
                   bins = nb_classes) + xlab("beta_nnls_2") +
    ggtitle(paste0(tmp1_nnls, "\n", tmp2,
                   ", ", tmp6, "\n",
                   tmp3, ", ", tmp4, ", ", tmp5))

  # Histogrammes lm
  beta_lm_1_hist <- ggplot(data = beta_lm, aes(beta_lm_1)) +
    geom_histogram(aes(y = ..density..), col = "black", fill="red",
                   bins = nb_classes) + xlab("beta_lm_1") +
    ggtitle(paste0(tmp0_lm, "\n", tmp2,
                   ", ", tmp6, "\n",
                   tmp3, ", ", tmp4, ", ", tmp5))

  beta_lm_2_hist <- ggplot(data = beta_lm, aes(beta_lm_2)) +
    geom_histogram(aes(y = ..density..), col = "black", fill = "green",
                   bins = nb_classes) + xlab("beta_lm_2") +
    ggtitle(paste0(tmp1_lm, "\n", tmp2,
                   ", ", tmp6, "\n",
                   tmp3, ", ", tmp4, ", ", tmp5))


  # Pour centrer les titres
  # theme_update(plot.title = element_text(hjust = 0.5))



  ### Histogrammes superposés ###



  comparison_1_hist <- ggplot(comparison_1,
                              aes(x = beta_1, y = ..density..)) +
    geom_histogram(data = subset(comparison_1, model == "nnls"),
                   aes(fill = model),
                   bins = floor(nb_classes/2), alpha = 0.2) +
    geom_histogram(data = subset(comparison_1, model == "lm"),
                   aes(fill = model),
                   bins = floor(nb_classes/2), alpha = 0.2) +
    scale_fill_manual(name = "model", values = c("red", "blue")) +
    ggtitle(paste0(tmp_comparison, " ",
                   tmp0_comparison, "\n",
                   tmp2, ", ", tmp6, "\n",
                   tmp3, ", ", tmp4, ", ", tmp5_comparison))

  comparison_2_hist <- ggplot(comparison_2,
                              aes(x = beta_2, y = ..density..)) +
    geom_histogram(data = subset(comparison_2, model == "nnls"),
                   aes(fill = model),
                   bins = floor(nb_classes/2), alpha = 0.2) +
    geom_histogram(data = subset(comparison_2, model == "lm"),
                   aes(fill = model),
                   bins = floor(nb_classes/2), alpha = 0.2) +
    scale_fill_manual(name = "model", values = c("green","purple")) +
    ggtitle(paste0(tmp_comparison, " ",
                   tmp1_comparison, "\n",
                   tmp2, ", ", tmp6, "\n",
                   tmp3, ", ", tmp4, ", ", tmp5_comparison))



  ### Histogrammes superposés en enlevant le pic en 0 pour nnls ###



  # beta_nnls sans les valeurs nulles
  beta_nnls_1_without_0 <- beta_nnls$beta_nnls_1[beta_nnls$beta_nnls_1 > 0]
  beta_nnls_2_without_0 <- beta_nnls$beta_nnls_2[beta_nnls$beta_nnls_2 > 0]

  # Comparaison entre nnls (sans les valeurs nulles) et lm
  comparison_1_without_0 <-
    data.frame(beta_1 = c(beta_nnls_1_without_0,
                          beta_lm$beta_lm_1),
               model = c(rep("nnls",
                             length(beta_nnls_1_without_0)),
                         rep("lm", length(beta_lm$beta_lm_1))))

  comparison_2_without_0 <-
    data.frame(beta_2 = c(beta_nnls_2_without_0,
                          beta_lm$beta_lm_2),
               model = c(rep("nnls",
                             length(beta_nnls_2_without_0)),
                         rep("lm", length(beta_lm$beta_lm_2))))

  # Histogrammes
  comparison_1_hist_without_0 <- ggplot(comparison_1_without_0,
                                        aes(x = beta_1, y = ..density..)) +
    geom_histogram(data = subset(comparison_1_without_0, model == "nnls"),
                   aes(fill = model), bins = nb_classes, alpha = 0.2) +
    geom_histogram(data = subset(comparison_1_without_0, model == "lm"),
                   aes(fill = model), bins = nb_classes, alpha = 0.2) +
    scale_fill_manual(name = "model", values = c("red","blue")) +
    ggtitle(paste0(tmp_comparison, " ",
                   tmp0_comparison_without_0, "\n",
                   tmp2, ", ", tmp6, "\n",
                   tmp3, ", ", tmp4, ", ", tmp5))


  comparison_2_hist_without_0 <- ggplot(comparison_2_without_0,
                                        aes(x = beta_2, y = ..density..)) +
    geom_histogram(data = subset(comparison_2_without_0, model == "nnls"),
                   aes(fill = model), bins = nb_classes, alpha = 0.2) +
    geom_histogram(data = subset(comparison_2_without_0, model == "lm"),
                   aes(fill = model), bins = nb_classes, alpha = 0.2) +
    scale_fill_manual(name = "model", values = c("green","purple")) +
    ggtitle(paste0(tmp_comparison, " ",
                   tmp1_comparison_without_0, "\n",
                   tmp2, ", ", tmp6, "\n",
                   tmp3, ", ", tmp4, ", ", tmp5))

  return(list(beta_nnls_1_hist = beta_nnls_1_hist,
              beta_nnls_2_hist = beta_nnls_2_hist,
              beta_lm_1_hist = beta_lm_1_hist,
              beta_lm_2_hist = beta_lm_2_hist,
              comparison_1_hist = comparison_1_hist,
              comparison_1_hist_without_0 = comparison_1_hist_without_0,
              comparison_2_hist = comparison_2_hist,
              comparison_2_hist_without_0 = comparison_2_hist_without_0,
              biais_df = biais_df, var_df = var_df, MSE_df = MSE_df,
              IC_df = IC_df))
}









IC_beta <- function(list_of_beta, n = 10^3, sigma = 1, prop = 0.7,
                    replace = TRUE, B = 10, alpha = 0.05, nb_classes = 50)
{

  ### INITIALISATION ###
  IC_beta_list <- vector("list", length = length(list_of_beta))
  noms_IC <- vector("list", length = length(list_of_beta))


  ### Création des noms ###
  for (i in 1:length(noms_IC))
  {
    noms_IC[[i]] <- paste0("beta = (", list_of_beta[[i]][1],
                           ", ", list_of_beta[[i]][2], ")")
  }

  ### Calcul des IC ###
  tmp <- rapply(list_of_beta, distribution_beta, n = n, sigma = sigma,
                          prop = prop, replace = replace, B = B,
                          alpha = alpha, nb_classes = nb_classes,
                how = "list")

  ### Stockage des résultats ###
  for (i in 1:length(IC_beta_list))
  {
    IC_beta_list[[i]] <- tmp[[i]]$IC_df
  }

  ### On nomme chaque élément de la liste des IC ###
  names(IC_beta_list) <- noms_IC

  return(IC_beta_list)
}








# Test : compter le nombre de fois où 0 n'est pas dans IC
# But : vérifier que ya 5% d'erreur

# On sait que dans 95% des cas, la vraie valeur de beta
# sera dans l'IC obtenu.
# Si 0 n'est pas dans l'IC pour \hat\beta_i,
# on rejette l'hypothèse que beta_i = 0
# On va compter le nombre de rejet de l'hypothèse
test_beta <- function(nb_repetitions = 100, n = 10^3, sigma = 1,
                      beta = c(0, 0), prop = 0.7, replace = TRUE,
                      B = 10, alpha = 0.05, nb_classes = 50)
{



  ### INITIALISATION ###



  nb_rejet_nnls_1 <- 0
  nb_rejet_nnls_2 <- 0
  nb_rejet_lm_1 <- 0
  nb_rejet_lm_2 <- 0



  ### Calcul des IC ###



  tmp <- replicate(nb_repetitions,
                   IC_beta(list_of_beta = list(beta), n = n,
                           sigma = sigma, prop = prop,
                           replace = replace, B = B, alpha = alpha,
                           nb_classes = nb_classes))



  ### Comptage du nombre de rejets ###



  for (i in 1:nb_repetitions)
  {

    if(tmp[[i]]$IC_nnls_1[2] < 0 | tmp[[i]]$IC_nnls_1[1] > 0)
    {
      nb_rejet_nnls_1 = nb_rejet_nnls_1 + 1
    }

    if(tmp[[i]]$IC_nnls_2[2] < 0 | tmp[[i]]$IC_nnls_2[1] > 0)
    {
      nb_rejet_nnls_2 = nb_rejet_nnls_2 + 1
    }

    if(tmp[[i]]$IC_lm_1[2] < 0 | tmp[[i]]$IC_lm_1[1] > 0)
    {
      nb_rejet_lm_1 = nb_rejet_lm_1 + 1
    }

    if(tmp[[i]]$IC_lm_2[2] < 0 | tmp[[i]]$IC_lm_2[1] > 0)
    {
      nb_rejet_lm_2 = nb_rejet_lm_2 + 1
    }
  }



  ### Calcul de la fréquence de rejet ###



  #(on est censé trouver 5% par construction)
  freq_rejet_nnls_1 <- nb_rejet_nnls_1/nb_repetitions
  freq_rejet_nnls_2 <- nb_rejet_nnls_2/nb_repetitions
  freq_rejet_lm_1 <- nb_rejet_lm_1/nb_repetitions
  freq_rejet_lm_2 <- nb_rejet_lm_2/nb_repetitions

  freq_df <- data.frame(freq_rejet_nnls_1 = freq_rejet_nnls_1,
                        freq_rejet_nnls_2 = freq_rejet_nnls_2,
                        freq_rejet_lm_1 = freq_rejet_lm_1,
                        freq_rejet_lm_2 = freq_rejet_lm_2)

  return(freq_df)
}









test_beta_grid <- function(beta_grid, nb_repetitions = 100, n = 10^3,
                           sigma = 1, prop = 0.7, replace = TRUE,
                           B = 10, alpha = 0.05, nb_classes = 50)
{

  results_test_beta_grid <- rapply(beta_grid, test_beta,
                                   nb_repetitions = nb_repetitions,
                                   n = n, sigma = sigma, prop = prop,
                                   replace = replace, B = B, alpha = alpha,
                                   nb_classes = nb_classes, how = "list")

  noms_results <- vector("list", length = length(beta_grid))

  for (i in 1:length(noms_results))
  {
    noms_results[[i]] <- paste0("beta = (", beta_grid[[i]][1],
                                ", ", beta_grid[[i]][2], ")")
  }

  names(results_test_beta_grid) <- noms_results

  return(results_test_beta_grid)
}

