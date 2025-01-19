require(udpipe)
require(tidyverse)

ud_model <- udpipe_load_model("./udpipe/portuguese-bosque-ud-2.5-191206.udpipe")

# Para traduzir cada UPOS para português:
upos_translation <- list(
  "ADJ" = "Adjetivo",
  "ADP" = "Preposição",
  "ADV" = "Advérbio",
  "AUX" = "Verbo Auxiliar",
  "CONJ" = "Conjunção",
  "CCONJ" = "Conjunção",
  "DET" = "Artigo",
  "INTJ" = "Interjeição",
  "NOUN" = "Substantivo",
  "NUM" = "Numeral",
  "PART" = "Partícula",
  "PRON" = "Pronome",
  "PROPN" = "Substantivo Próprio",
  "PUNCT" = "Pontuação",
  "SCONJ" = "Conjunção",
  "SYM" = "Símbolo",
  "VERB" = "Verbo",
  "X" = "Outro"
)


create_upos_file <- function(text, ud_model, output_name) {
  annotations <- udpipe_annotate(ud_model, x = text)
  
  conllu_data <- annotations$conllu
  lines <- unlist(strsplit(conllu_data, "\n"))
  lines <- lines[!grepl("^#", lines) & lines != ""]
  
  # Apenas as colunas 2 e 4, que contêm a forma e o UPOS
  columns <- lapply(strsplit(lines, "\t"), function(x) c(x[2], x[4]))
  
  upos_form_df <- do.call(rbind, lapply(columns, function(x) {
    as.data.frame(t(x), stringsAsFactors = FALSE)
  }))
  
  colnames(upos_form_df) <- c("Form", "UPOS")
  
  upos_form_df <- upos_form_df |>
    filter(UPOS != "PUNCT") |>
    filter(UPOS != "_") |>
    filter(UPOS != "SYM") |>
    mutate(UPOS = ifelse(UPOS == "PROPN", "NOUN", UPOS)) |>
    mutate(UPOS = sapply(UPOS, function(upos) upos_translation[[upos]]))
  
  
  saveRDS(upos_form_df, paste0("./dados/", output_name, "_UPOS.rds"))
}

alienista <- readLines("./dados/alienista.txt")
create_upos_file(alienista, ud_model, "alienista")





ggplot(alienista, aes(x = UPOS)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Frequência de Cada Morfema no Texto", x = "Morfema", y = "Frequência") +
  theme_minimal()

