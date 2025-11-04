# =====================================================================
# PIPELINE REPRODUTÍVEL {targets} - DELINEAMENTO ALPHA-LATTICE
# ---------------------------------------------------------------------
# Autora: Jennifer Luz Lopes
# Descrição: Automação do fluxo experimental (BLUE, BLUP, herdabilidade,
# agrupamento) com coleta via API GitHub.
# =====================================================================

# ---------------------------------------------------------------------
# 1. PACOTES -----------------------------------------------------------
# ---------------------------------------------------------------------
library(targets)
library(tarchetypes)
library(tidyverse)
library(lme4)
library(emmeans)
library(broom.mixed)
library(glue)
library(metan)
library(ggplot2)
library(ggpubr)
library(writexl)
library(httr2)
library(base64enc)
library(readxl)

# ---------------------------------------------------------------------
# 2. OPÇÕES GERAIS DO PIPELINE ----------------------------------------
# ---------------------------------------------------------------------
tar_option_set(
  packages = c(
    "tidyverse", "lme4", "emmeans", "broom.mixed", "metan",
    "ggplot2", "ggpubr", "writexl", "glue", "httr2", "base64enc", "readxl"
  ),
  format = "rds"
)

# ---------------------------------------------------------------------
# 3. FUNÇÕES AUXILIARES -----------------------------------------------
# ---------------------------------------------------------------------
source("meu_projeto/funcoes/coleta_dados_github.R")

# ---------------------------------------------------------------------
# 4. DEFINIÇÃO DO PIPELINE {targets} ----------------------------------
# ---------------------------------------------------------------------
list(
  # Etapa 1: Coleta via API GitHub ------------------------------------
  tar_target(
    dados_brutos,
    coleta_dados_github(
      repo = "JenniferLopes/portfolio_experimentacao_agricola",
      path = "meu_projeto/dados/alpha_lattice.xlsx"
    )
  ),
  
  # Etapa 2: Ajuste de variáveis e estrutura experimental -------------
  tar_target(
    dados,
    dados_brutos %>%
      mutate(
        gen = as.factor(gen),
        rep = as.factor(rep),
        inc.bloco = as.factor(inc.bloco)
      )
  ),
  
  # Etapa 3: Croqui de campo ------------------------------------------
  tar_target(
    croqui_campo,
    ggplot(dados, aes(x = col, y = row, fill = inc.bloco)) +
      geom_tile(color = "black") +
      geom_text(aes(label = gen), size = 3) +
      facet_wrap(~rep, scales = "free_x") +
      theme_bw(base_size = 12) +
      labs(
        title = "Croqui de Campo - Delineamento Alpha-Lattice",
        x = "Colunas", y = "Linhas"
      )
  ),
  
  # Etapa 4: Análise descritiva ---------------------------------------
  tar_target(
    analise_desc,
    metan::desc_stat(dados$prod, hist = FALSE, stats = "main")
  ),
  
  # Etapa 5: Modelos mistos -------------------------------------------
  tar_target(
    modelo_BLUE,
    lmer(prod ~ gen + rep + (1 | rep:inc.bloco), data = dados)
  ),
  tar_target(
    modelo_BLUP,
    lmer(prod ~ rep + (1 | gen) + (1 | rep:inc.bloco), data = dados)
  ),
  
  # Etapa 6: Comparação AIC / logLik ----------------------------------
  tar_target(
    comparacao_modelos,
    tibble(
      Modelo = c("Efeito Fixo (BLUE)", "Efeito Aleatório (BLUP)"),
      AIC = c(AIC(modelo_BLUE), AIC(modelo_BLUP)),
      logLik = c(logLik(modelo_BLUE), logLik(modelo_BLUP))
    )
  ),
  
  # Etapa 7: Estimativas BLUE e BLUP ----------------------------------
  tar_target(
    estimativas,
    {
      BLUEs <- emmeans(modelo_BLUE, ~gen) %>%
        as.data.frame() %>%
        transmute(
          gen,
          BLUE = emmean,
          IC_inferior_BLUE = lower.CL,
          IC_superior_BLUE = upper.CL
        )
      
      mu_manual <- fixef(modelo_BLUP)[1]
      BLUPs <- broom.mixed::augment(ranef(modelo_BLUP)) %>%
        filter(grp == "gen") %>%
        transmute(
          gen = level,
          BLUP = mu_manual + estimate,
          IC_inferior_BLUP = BLUP - 1.96 * std.error,
          IC_superior_BLUP = BLUP + 1.96 * std.error
        )
      
      full_join(BLUEs, BLUPs, by = "gen")
    }
  ),
  
  # Etapa 8: Comparação visual BLUE x BLUP -----------------------------
  tar_target(
    grafico_comparacao,
    {
      estimativas_long <- estimativas %>%
        pivot_longer(
          cols = c(BLUE, BLUP),
          names_to = "Tipo",
          values_to = "Estimativa"
        )
      
      ggplot(estimativas_long, aes(x = gen, y = Estimativa, fill = Tipo)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
        geom_errorbar(
          aes(
            ymin = ifelse(Tipo == "BLUE", IC_inferior_BLUE, IC_inferior_BLUP),
            ymax = ifelse(Tipo == "BLUE", IC_superior_BLUE, IC_superior_BLUP)
          ),
          position = position_dodge(width = 0.8), width = 0.2
        ) +
        labs(
          title = "Comparação entre BLUEs e BLUPs por Genótipo",
          x = "Genótipos", y = "Estimativas de Produção (kg/ha)",
          fill = "Modelo"
        ) +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  ),
  
  # Etapa 9: Herdabilidade --------------------------------------------
  tar_target(
    herdabilidade,
    {
      vcomps <- as.data.frame(VarCorr(modelo_BLUP))
      vc.g <- vcomps[vcomps$grp == "gen", "vcov"]
      vc.e <- vcomps[vcomps$grp == "Residual", "vcov"]
      nreps <- length(unique(dados$rep))
      hc <- vc.g / (vc.g + vc.e / nreps)
      hc
    }
  ),
  
  # Etapa 10: Agrupamento UPGMA ---------------------------------------
  tar_target(
    agrupamento_UPGMA,
    {
      blup_values <- estimativas$BLUP
      names(blup_values) <- estimativas$gen
      hc_tree <- hclust(dist(blup_values), method = "average")
      hc_tree
    }
  ),
  
  # Etapa 11: Exportações ---------------------------------------------
  tar_target(
    exportar_outputs,
    {
      dir.create("meu_projeto/output", showWarnings = FALSE)
      
      writexl::write_xlsx(
        list(
          "Analise_Descritiva" = analise_desc,
          "Comparacao_Modelos" = comparacao_modelos,
          "Estimativas_BLUE_BLUP" = estimativas,
          "Herdabilidade" = tibble(H2 = herdabilidade)
        ),
        path = "meu_projeto/output/resultados_experimentais.xlsx"
      )
      
      ggsave(
        filename = "meu_projeto/output/grafico_comparacao_BLUE_BLUP.png",
        plot = grafico_comparacao,
        width = 8, height = 6, dpi = 300
      )
      
      message("✅ Resultados exportados em 'meu_projeto/output/'.")
    },
    cue = tar_cue(mode = "always")
  )
)
