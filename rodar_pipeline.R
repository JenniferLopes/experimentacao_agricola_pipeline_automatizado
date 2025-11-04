# =====================================================================
# PIPELINE REPRODUTÍVEL {targets} - EXPERIMENTAÇÃO AGRÍCOLA
# Objetivo: Acompanhar a execução, verificar dependências e 
# depurar etapas do pipeline
# ---------------------------------------------------------------------
# Autora: Jennifer Luz Lopes
# Descrição: Pipeline reprodutível para modelagem experimental
# com coleta de dados via API GitHub e análises BLUE/BLUP
# =====================================================================

# 1. Carregar o pacote principal ----------------------------------------------
# O {targets} gerencia pipelines reprodutíveis e automáticos no R.
library(targets)

# 2. Executar o pipeline completo ---------------------------------------------
# Reexecuta apenas as etapas (targets) que foram alteradas.
tar_make()

# 3. Ver manifesto do pipeline -------------------------------------------------
# Exibe o nome de cada target e o comando usado para gerá-lo.
tar_manifest(fields = command)

# 4. Visualizar a rede de dependências ----------------------------------------
# Mostra graficamente como os targets se conectam (interativo).
tar_visnetwork()

# 5. Ler o resultado de um target ---------------------------------------------
# Retorna o conteúdo direto do target (sem carregar no ambiente).
resultado <- tar_read(estimativas)
head(resultado)

# 6. Carregar target no ambiente do RStudio -----------------------------------
# Disponibiliza o objeto para uso manual, como se tivesse sido criado no script.
tar_load(modelo_BLUE)

# 7. Carregar múltiplos targets de uma só vez ---------------------------------
# Útil quando você quer analisar diferentes etapas ao mesmo tempo.
tar_load(c(resumo_final, estimativas))

# 8. Verificar avisos (warnings) dos targets ----------------------------------
# Mostra quais etapas geraram mensagens de aviso durante a execução.
targets::tar_meta(fields = warnings, complete_only = TRUE)

# 9. Acessar metadados dos targets, como tempo de execução, tamanho, avisos ----
tar_meta()

# 10. Resumo do progresso -------------------------------------------------------
# Mostra o andamento de execução de cada target (status e tempo total).
tar_progress()

# 11. Resumo em texto do progresso da execução
# Atualiza continuamente um resumo em texto do progresso da execução no console do R. 
# Execute-o em uma nova sessão do R no diretório raiz do projeto. 
tar_poll()

# 12. Resumo geral do pipeline -------------------------------------------------
# Exibe uma visão resumida com número de targets, tempo total e status geral.
tar_progress_summary()
