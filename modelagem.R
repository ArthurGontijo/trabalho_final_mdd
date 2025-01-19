require(VLMC)
require(ggraph)
require(tidyverse)

alienista <- readRDS("./dados/alienista_UPOS.rds")

multinomial_model <- function(data) {
  alphabet_size <- data$UPOS |> unique() |> length()
  parameters_to_estimate <- alphabet_size - 1
  sample_size <- data$UPOS |> length()
  
  n <- data$UPOS |> table()
  p <- n |> prop.table()
  
  max_log_lik <- 0
  for(i in 1:alphabet_size) {
    max_log_lik <- max_log_lik + n[i] * log(p[i])
  }
  
  BIC <- 2 * max_log_lik - parameters_to_estimate * log(sample_size)
  
  # Só para tornar o output mais limpo
  names(max_log_lik) <- NULL
  names(BIC) <- NULL
  
  output <- list(
    "n" = n,
    "p" = p |> round(4),
    "max_log_lik" = max_log_lik,
    "BIC" = BIC
  )
  
  return(output)
}

markov_model <- function(data, order = 1) {
  alphabet <- data$UPOS |> unique() 
  alphabet_size <- alphabet |> length()
  parameters_to_estimate <- alphabet_size^order * (alphabet_size - 1)
  sample_size <- data$UPOS |> length()
  
  upos_sequence <- data$UPOS
  
  all_combinations <- expand.grid(rep(list(alphabet), order))
  all_ngrams <- apply(all_combinations, 1, function(row) paste(row, collapse = "|"))
  
  ngrams <- sapply(1:(sample_size - order), function(i) {
    paste(upos_sequence[i:(i + order - 1)], collapse = "|")
  })
  
  n <- table(ngrams, upos_sequence[(order + 1):sample_size])
  
  missing_ngrams <- setdiff(all_ngrams, rownames(n))
  if (length(missing_ngrams) > 0) {
    n <- rbind(n, matrix(0, nrow = length(missing_ngrams), ncol = ncol(n), dimnames = list(missing_ngrams, colnames(n))))
  }
  
  n <- n[order(rownames(n)), ]
  p <- prop.table(n, 1)
  p[is.na(p)] <- 0
  
  max_log_lik <- 0
  for(i in 1:(alphabet_size ^ order)) {
    for(j in 1:alphabet_size) {
      if(p[i, j] != 0) {
        max_log_lik <- max_log_lik + n[i, j] * log(p[i, j]) 
      }
    }
  }
  
  BIC <- 2 * max_log_lik - parameters_to_estimate * log(sample_size)
  
  output <- list(
    "order" = order, 
    "n_params" = parameters_to_estimate,
    "n" = n,
    "p" = p |> round(4),
    "max_log_lik" = max_log_lik,
    "BIC" = BIC
  )
  
  return(output)
}

variable_length_model <- function(data) {
  alphabet <- data$UPOS |> unique()
  alphabet_size <- alphabet |> length()
  N = data$UPOS |> length()
  vl_model <- vlmc(data$UPOS, alpha.c = 0.001)
  
  # Calculando o bic do modelo
  context = as.numeric(vl_model$size[2])
  BIC = as.numeric(2*logLik(vl_model)-(alphabet_size-1)*log(N)*context)
  
  # Plot
  dvlmc <- as.dendrogram(vl_model, hang = -1)
  
  output = list(
    "model" = vl_model,
    "BIC" = BIC, 
    "plot" = dvlmc
  )
  
  return(output)
}

# === Modelo multinomial ===
multinomial_model(alienista)
# Teste de independência
chisq.test(table(alienista$UPOS))


# === Modelo de Markov com Ordem Fixa ===
m1 <- markov_model(alienista)
m2 <- markov_model(alienista, order = 2)
m3 <- markov_model(alienista, order = 3)


# === Modelo de Markov com Ordem Variável ===
vl_model <- variable_length_model(alienista)
plot(vl_model$plot, type ="tr", nodePar = list(pch=c(1,20)), center=TRUE,
     yaxt = "n")








