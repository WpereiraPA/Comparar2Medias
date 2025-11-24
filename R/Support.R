# ==============================================================================
# PACOTE: ComparMedia
# FUNCAO PRINCIPAL: teste_t_pareado_interativo()
# VERSAO 100% ASCII - SEM ACENTOS E CARACTERES ESPECIAIS
# ==============================================================================

#' Teste t Pareado Interativo
#'
#' Esta funcao realiza um teste t pareado de forma interativa, solicitando
#' os dados das duas variaveis e o nivel de significancia desejado.
#'
#' @return Um objeto de classe "htest" com os resultados do teste t pareado
#' @export
#' @examples
#' \dontrun{
#' teste_t_pareado_interativo()
#' }
teste_t_pareado_interativo <- function() {

  cat("\n")
  cat("===============================================================\n")
  cat("           TESTE t PAREADO - MODO INTERATIVO\n")
  cat("===============================================================\n\n")

  # ========== COLETA DA PRIMEIRA VARIAVEL ==========
  cat("ETAPA 1: Entrada de dados da primeira variavel\n")
  cat("---------------------------------------------------------------\n")
  cat("Digite os valores separados por virgula (use ponto decimal).\n")
  cat("Exemplo: 4.05, 3.69, 4.10, 4.70\n\n")

  entrada1 <- readline(prompt = "Primeira variavel: ")

  # Processar entrada
  variavel1 <- tryCatch({
    valores <- as.numeric(unlist(strsplit(entrada1, ",")))
    if (any(is.na(valores))) {
      stop("Valores invalidos detectados na primeira variavel.")
    }
    valores
  }, error = function(e) {
    stop("Erro ao processar primeira variavel: ", e$message)
  })

  cat("\n[OK] Primeira variavel registrada: ", length(variavel1), " observacoes\n")
  cat("  Valores: ", paste(round(variavel1, 2), collapse = ", "), "\n\n")

  # ========== COLETA DA SEGUNDA VARIAVEL ==========
  cat("ETAPA 2: Entrada de dados da segunda variavel\n")
  cat("---------------------------------------------------------------\n")
  cat("Digite os valores separados por virgula (use ponto decimal).\n")
  cat("IMPORTANTE: Deve ter o mesmo numero de observacoes (", length(variavel1), ")\n\n")

  entrada2 <- readline(prompt = "Segunda variavel: ")

  # Processar entrada
  variavel2 <- tryCatch({
    valores <- as.numeric(unlist(strsplit(entrada2, ",")))
    if (any(is.na(valores))) {
      stop("Valores invalidos detectados na segunda variavel.")
    }
    valores
  }, error = function(e) {
    stop("Erro ao processar segunda variavel: ", e$message)
  })

  cat("\n[OK] Segunda variavel registrada: ", length(variavel2), " observacoes\n")
  cat("  Valores: ", paste(round(variavel2, 2), collapse = ", "), "\n\n")

  # Verificar compatibilidade
  if (length(variavel1) != length(variavel2)) {
    stop("ERRO: As variaveis devem ter o mesmo numero de observacoes!\n",
         "  Variavel 1: ", length(variavel1), " observacoes\n",
         "  Variavel 2: ", length(variavel2), " observacoes")
  }

  # ========== COLETA DO NIVEL DE SIGNIFICANCIA ==========
  cat("ETAPA 3: Definicao do nivel de significancia\n")
  cat("---------------------------------------------------------------\n")
  cat("Para 95% de confianca, digite: 0.05\n")
  cat("Para 99% de confianca, digite: 0.01\n")
  cat("Para 90% de confianca, digite: 0.10\n\n")

  entrada_alpha <- readline(prompt = "Nivel de significancia (alpha): ")

  alpha <- tryCatch({
    valor <- as.numeric(entrada_alpha)
    if (is.na(valor) || valor <= 0 || valor >= 1) {
      stop("O valor deve estar entre 0 e 1.")
    }
    valor
  }, error = function(e) {
    stop("Erro ao processar nivel de significancia: ", e$message)
  })

  confianca <- (1 - alpha) * 100
  cat("\n[OK] Nivel de confianca: ", confianca, "%\n")
  cat("[OK] Nivel de significancia (alpha): ", alpha, "\n\n")

  # ========== EXECUTAR TESTE t PAREADO ==========
  cat("===============================================================\n")
  cat("                    EXECUTANDO ANALISE...\n")
  cat("===============================================================\n\n")

  resultado <- stats::t.test(variavel1, variavel2,
                             paired = TRUE,
                             conf.level = 1 - alpha)

  # ========== APRESENTAR RESULTADOS ==========
  cat("========== RESULTADO DO TESTE t PAREADO ==========\n\n")

  cat("ESTATISTICAS DESCRITIVAS:\n")
  cat("  Media Variavel 1: ", round(mean(variavel1), 4), "\n")
  cat("  Media Variavel 2: ", round(mean(variavel2), 4), "\n")
  cat("  Diferenca media: ", round(mean(variavel1 - variavel2), 4), "\n\n")

  cat("RESULTADO DO TESTE:\n")
  cat("  Estatistica t = ", round(resultado$statistic, 4), "\n")
  cat("  Graus de liberdade = ", resultado$parameter, "\n")
  cat("  Valor-p = ", round(resultado$p.value, 4), "\n\n")

  cat("INTERVALO DE CONFIANCA (", confianca, "%):\n", sep = "")
  cat("  [", round(resultado$conf.int[1], 4), " ; ",
      round(resultado$conf.int[2], 4), "]\n\n", sep = "")

  # ========== INTERPRETACAO ==========
  cat("INTERPRETACAO:\n")
  if (resultado$p.value >= alpha) {
    cat("  [ACEITA H0] Nao ha diferenca estatistica significativa\n")
    cat("  --> As medias das duas variaveis sao estatisticamente iguais\n")
    cat("      (p = ", round(resultado$p.value, 4),
        " >= alpha = ", alpha, ").\n", sep = "")
  } else {
    cat("  [REJEITA H0] Ha diferenca estatistica significativa\n")
    cat("  --> As medias das duas variaveis sao estatisticamente diferentes\n")
    cat("      (p = ", round(resultado$p.value, 4),
        " < alpha = ", alpha, ").\n", sep = "")
  }

  cat("\n==================================================\n\n")

  # Retornar resultado invisivel para analise posterior
  invisible(resultado)
}


# ==============================================================================
# DOCUMENTACAO DO PACOTE
# ==============================================================================

#' ComparMedia: Teste t Pareado Interativo
#'
#' Este pacote fornece uma funcao interativa para realizar testes t pareados,
#' facilitando a analise estatistica de dados pareados atraves de uma interface
#' amigavel no console R.
#'
#' @docType package
#' @name ComparMedia
"_PACKAGE"
