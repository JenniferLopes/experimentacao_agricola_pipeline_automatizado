# =====================================================================
# PIPELINE REPRODUTÍVEL {targets} - EXPERIMENTAÇÃO AGRÍCOLA
# ---------------------------------------------------------------------
# Autora: Jennifer Luz Lopes
# Descrição: Pipeline reprodutível para modelagem experimental
# com coleta de dados via API GitHub e análises BLUE/BLUP
# =====================================================================

# ---------------------------------------------------------------------
# 1. PACOTES -----------------------------------------------------------
# ---------------------------------------------------------------------
library(targets)        
library(tarchetypes)    
library(tidyverse)      
library(lme4)           
library(emmeans)        
library(metan)          
library(ggplot2)        
library(glue)           
library(readxl)         
library(broom.mixed)    
library(httr2)          
library(base64enc)      
library(ggpubr)         
library(writexl)        

# ---------------------------------------------------------------------
# 2. OPÇÕES DO PIPELINE ---------------------------------------
# ---------------------------------------------------------------------
tar_option_set(
  packages = c(
    "tidyverse", "lme4", "emmeans", "metan", "ggplot2",
    "readxl", "broom.mixed", "glue", "httr2", 
    "base64enc", "ggpubr", "writexl"
  ),
  format = "rds")

# ---------------------------------------------------------------------
# 3. FUNÇÕES AUXILIARES ------------------------------------------------
# ---------------------------------------------------------------------
source("meu_projeto/funcoes/coleta_dados_github.R")

# ---------------------------------------------------------------------
# 4. DEFINIÇÃO DOS TARGETS ---------------------------------------------
# ---------------------------------------------------------------------
list(
  
  # Etapa 1: Coleta de dados via API GitHub ----------------------------
  tar_target(
    dados_brutos,
    coleta_dados_github(
      repo = "JenniferLopes/portfolio_experimentacao_agricola",
      path = "meu_projeto/dados/alpha_lattice.xlsx"
    )
  ),
  
  # Etapa 2: Ajuste de variáveis ---------------------------------------
  tar_target(
    dados,
    dados_brutos %>%
      mutate(
        gen = as.factor(gen),
        rep = as.factor(rep),
        inc.bloco = as.factor(inc.bloco)
      )
  ),
  
  # Etapa 3: Modelos BLUE e BLUP ---------------------------------------
  tar_target(
    modelo_BLUE,
    lmer(prod ~ gen + rep + (1 | rep:inc.bloco), data = dados)
  ),
  tar_target(
    modelo_BLUP,
    lmer(prod ~ rep + (1 | gen) + (1 | rep:inc.bloco), data = dados)
  ),
  
  # Etapa 4: Estimativas BLUEs e BLUPs ---------------------------------
  tar_target(
    estimativas,
    {
      BLUEs <- emmeans(modelo_BLUE, ~ gen) %>%
        as.data.frame() %>%
        transmute(gen, BLUE = emmean)
      
      mu_manual <- fixef(modelo_BLUP)[1]
      BLUPs <- augment(ranef(modelo_BLUP)) %>%
        filter(grp == "gen") %>%
        transmute(gen = level, BLUP = mu_manual + estimate)
      
      merge(BLUEs, BLUPs, by = "gen")
    }
  ),
  
  # Etapa 5: Herdabilidade ---------------------------------------------
  tar_target(
    herdabilidade,
    {
      vcomps <- as.data.frame(VarCorr(modelo_BLUP))
      vc.g <- vcomps[vcomps$grp == "gen", "vcov"]
      vc.e <- vcomps[vcomps$grp == "Residual", "vcov"]
      nreps <- 3
      hc <- vc.g / (vc.g + vc.e / nreps)
      hc
    }
  ),
  
  # Etapa 6: Agrupamento UPGMA -----------------------------------------
  tar_target(
    agrupamento,
    {
      blup_values <- estimativas$BLUP
      names(blup_values) <- estimativas$gen
      hclust(dist(blup_values), method = "average")
    }
  ),
  
  # Etapa 7: Gráfico BLUE vs BLUP --------------------------------------
  tar_target(
    grafico_BLUE_BLUP,
    {
      stopifnot(all(c("BLUE", "BLUP") %in% names(estimativas)))
      
      ggplot(estimativas, aes(x = BLUE, y = BLUP)) +
        geom_point(size = 3, alpha = 0.6, color = "#224573") +
        geom_smooth(method = "lm", color = "#E85D04", se = FALSE) +
        ggpubr::stat_regline_equation(label.y = max(estimativas$BLUP, na.rm = TRUE)) +
        theme_bw() +
        labs(
          title = "Correlação entre BLUEs e BLUPs dos Genótipos",
          subtitle = "Estimativas ajustadas do modelo misto (REML)",
          x = "BLUE (Efeito Fixo)",
          y = "BLUP (Efeito Aleatório)",
          caption = "SIGM | Jennifer Luz Lopes"
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 13),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    }
  ),
  
  # Etapa 8: Resumo final ----------------------------------------------
  tar_target(
    resumo_final,
    {
      tibble(
        Herdabilidade = round(herdabilidade, 3),
        Cor_BLUE_BLUP = cor(estimativas$BLUE, estimativas$BLUP)
      )
    }
  ),
  
  # Etapa 9: Exportação automática -------------------------------------
  tar_target(
    exportar_resultados,
    {
      dir.create("meu_projeto/output", showWarnings = FALSE)
      
      # Exporta tabela
      writexl::write_xlsx(
        list(
          "Estimativas_BLUE_BLUP" = estimativas,
          "Resumo_Final" = resumo_final
        ),
        path = "meu_projeto/output/resultados_experimentais.xlsx"
      )
      
      # Exporta gráfico
      ggsave(
        filename = "meu_projeto/output/grafico_BLUE_BLUP.png",
        plot = grafico_BLUE_BLUP,
        width = 7,
        height = 5,
        dpi = 300
      )
      
      message("✅ Resultados exportados para 'meu_projeto/output/'.")
    },
    cue = tar_cue(mode = "always")
  )
)


