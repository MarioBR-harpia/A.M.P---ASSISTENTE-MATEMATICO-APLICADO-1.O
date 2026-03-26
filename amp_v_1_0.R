library(lpSolve)
library(ggplot2)
library(deSolve)
library(Benchmarking)
library(readxl)

menu_matematico = function() {
  opcao = "" 
  
  while(opcao != "0") {
    
    cat("\014") 
    cat("=================================================================\n")
    cat("      A.M.P. | ASSISTENTE MATEMÁTICO APLICADO - v1.0             \n")
    cat("      Interface de Modelagem, Otimização e Inteligência          \n")
    cat("=================================================================\n\n")
    
    cat(">>> MÓDULOS DE OTIMIZAÇÃO (Pesquisa Operacional)\n")
    cat("-----------------------------------------------------------------\n")
    cat(" [ 1 ] PROGRAMAÇÃO LINEAR (Maximização de Lucros / Minimização de Custos)\n")
    cat(" [ 2 ] ANÁLISE ENVOLTÓRIA DE DADOS - DEA (Eficiência e Benchmarking)\n\n")
    
    cat(">>> MÓDULOS DE DINÂMICA E CONTROLE\n")
    cat("-----------------------------------------------------------------\n")
    cat(" [ 3 ] SISTEMAS DINÂMICOS (Modelagem de Caos e Estabilidade)\n")
    cat(" [ 4 ] CONTROLE ÓTIMO NÃO-LINEAR (Estabilização de Sistemas) [Em Breve]\n\n")
    
    cat(">>> MÓDULOS DE INTELIGÊNCIA ARTIFICIAL E DADOS\n")
    cat("-----------------------------------------------------------------\n")
    cat(" [ 5 ] MACHINE LEARNING (Modelos Preditivos e Regressão) [Em Breve]\n")
    cat(" [ 6 ] ALGORITMOS GENÉTICOS (Otimização Heurística) [Em Breve]\n\n")
    
    cat(">>> SISTEMA\n")
    cat("-----------------------------------------------------------------\n")
    cat(" [ 0 ] ENCERRAR SESSÃO\n")
    cat("=================================================================\n")
    
    
    opcao = readline(">> Selecione o módulo desejado: ")
    if (opcao == "1") {
      executa_pl() 
      cat("\n[INFO] Carregando módulo de Programação Linear...\n")
      Sys.sleep(1.5)
      
    } else if (opcao == "2") {
      executa_dea() 
      
    } else if (opcao == "3") {
      
      executa_sistemas_dinamicos()
      cat("\n[INFO] Carregando módulo de Sistemas Dinâmicos...\n")
      Sys.sleep(1.5)
      
    } else if (opcao %in% c("4", "5", "6")) {
      cat("\n[AVISO] Este módulo está em desenvolvimento para as próximas versões.\n")
      Sys.sleep(2)
      
    } else if (opcao == "0") {
      cat("\n[SISTEMA] Encerrando o A.M.P... Até a próxima análise!\n")
      Sys.sleep(1)
      
    } else {
      cat("\n[ERRO] Comando não reconhecido. Tente novamente.\n")
      Sys.sleep(1.5)
    }
  }
}



executa_pl = function() {
  cat("\014") 
  texto = paste(
    "================================================================\n",
    "        SISTEMA DE APOIO À DECISÃO E OTIMIZAÇÃO DE RECURSOS     \n",
    "================================================================\n",
    "Este módulo utiliza a Programação Linear para encontrar a melhor\n",
    "decisão possível (Matematicamente Ótima) para o seu negócio,\n",
    "respeitando as restrições físicas e financeiras da realidade.\n\n",
    "Conceitos Aplicados:\n",
    "1. Função Objetivo: A meta financeira (Maximizar ou Minimizar).\n",
    "2. Variáveis de Decisão: O que podemos controlar (ex: quanto produzir).\n",
    "3. Restrições: Nossos limites (ex: horas, orçamento, estoque).\n",
    "================================================================\n"
  )
  
  cat(texto)
  readline("Pressione [ENTER] para configurar o modelo de otimização...")
  
  cat("\014") 
  cat("\n================================================================")
  cat("\n>>> DEFINIÇÃO DA META ESTRATÉGICA <<<\n")
  cat("----------------------------------------------------------------\n")
  cat("Qual é o objetivo principal do cálculo?\n")
  cat(" 1. MAXIMIZAÇÃO (Ex: Maior Lucro, Receita ou Produtividade)\n")
  cat(" 2. MINIMIZAÇÃO (Ex: Menor Custo, Tempo ou Desperdício)\n")
  cat("----------------------------------------------------------------\n")
  
  escolha = as.integer(readline("Selecione a direção (1 ou 2): "))
  direcao = ifelse(escolha == 1, "max", "min")
  
  if (!(escolha %in% c(1, 2))) {
    cat("\n[ERRO] Operação cancelada. Seleção inválida.")
    Sys.sleep(1.5)
    return()
  }
  
  ler_input = function(msg) {
    entrada = readline(msg)
    partes = unlist(strsplit(entrada, "[ ,]+"))
    vetor = sapply(partes, function(x) eval(parse(text = x)))
    return(as.numeric(vetor))
  }
  
  cat("\014") 
  cat("================================================================")
  cat(paste("\n>>> ENTRADA DE DADOS: MODELO DE", toupper(direcao), "<<<\n"))
  cat("================================================================\n")
  
  # 1. Função Objetivo
  cat("[1] FUNÇÃO OBJETIVO (Vetor C):\n")
  f.obj = ler_input("Qual o Lucro ou Custo de cada item? (ex: 50, 120): ")
  
  # 2. Sinais das Restrições
  cat("\n[2] DIREÇÃO DOS LIMITES:\n")
  cat("Use '<=' para teto máximo ou '>=' para meta mínima.\n")
  dir_str = readline("Sinais separados por espaço (ex: <= <=): ")
  f.dir = unlist(strsplit(dir_str, "[ ,]+"))
  
  # 3. Disponibilidade
  cat("\n[3] DISPONIBILIDADE TOTAL (Termos Independentes / RHS):\n")
  f.rhs = ler_input("Qual o limite total de cada recurso? (ex: 40h, 1000 reais): ")
  
  # 4. Matriz Técnica
  cat("\n[4] CONSUMO DE RECURSOS (Matriz de Coeficientes):\n")
  cat("Quanto cada item consome de cada recurso?\n")
  mat_vec = ler_input("Digite todos os valores, linha por linha: ")
  
  f.con = matrix(mat_vec, nrow = length(f.dir), ncol = length(f.obj), byrow = TRUE)
  
  cat("\n[SISTEMA] Algoritmo Simplex em execução... Calculando cenário ideal.\n")
  Sys.sleep(1.5)
  
  solucao = lp(direction = direcao, 
               objective.in = f.obj, 
               const.mat = f.con, 
               const.dir = f.dir, 
               const.rhs = f.rhs,
               compute.sens = TRUE)
  
  cat("\014") 
  cat("================================================================\n")
  if (solucao$status == 0) {
    cat(">>> RELATÓRIO DE OTIMIZAÇÃO CONCLUÍDO <<<\n")
    cat("----------------------------------------------------------------\n")
    cat("RESULTADO FINANCEIRO ÓTIMO (Z*):", format(solucao$objval, nsmall=2), "\n\n")
    
    cat("PLANO DE AÇÃO IDEAL (Solução):\n")
    for(i in 1:length(solucao$solution)) {
      cat(paste("  -> Produzir/Alocar item X", i, ": ", round(solucao$solution[i], 2), " unidades\n", sep=""))
    }
    
    cat("\nANÁLISE DE GARGALOS (Preços-Sombra):\n")
    precos_sombra = solucao$duals[1:length(f.rhs)]
    for(i in 1:length(precos_sombra)) {
      if(precos_sombra[i] > 0) {
        cat(paste("  ! Dica: O Recurso ", i, " está esgotado. (+1 unidade gera +", 
                  round(precos_sombra[i], 2), " no resultado final)\n", sep=""))
      }
    }
  } else {
    cat(">>> ERRO: CENÁRIO MATEMATICAMENTE INVIÁVEL <<<\n")
    cat("As restrições fornecidas entram em conflito e não possuem solução.\n")
  }
  cat("================================================================\n")
  
  readline("\nPressione [ENTER] para retornar ao menu principal.")
}

executa_sistemas_dinamicos = function() {
  cat("\014") 
  texto = paste(
    "================================================================\n",
    "Sistemas Dinâmicos estudam como estados evoluem sob regras\n",
    "matemáticas determinísticas. No mundo real, essa evolução é\n",
    "frequentemente NÃO-LINEAR, o que dá origem a fenômenos como:\n\n",
    "1. CAOS: Extrema sensibilidade às condições iniciais (Efeito Borboleta).\n",
    "2. ATRATORES: Estados ou ciclos para os quais o sistema converge.\n",
    "3. ESTABILIDADE: A capacidade de um sistema retornar ao equilíbrio\n",
    "   após uma perturbação — foco central da Teoria de Controle Ótimo.\n\n",
    "Modelar esses sistemas permite prever crises financeiras, projetar\n",
    "estabilizadores na engenharia e entender o equilíbrio biológico.\n",
    "================================================================\n"
  )
  
  cat(texto)
  
  readline("Pressione [ENTER] para configurar os modelos...")
  
  cat("\014") 
  cat("\n================================================================")
  cat("\n>>> MÓDULO DE SISTEMAS DINÂMICOS <<<\n")
  cat("Escolha o modelo matemático para simular:\n")
  cat(" 1. Financeiro (Ma-Chen - Caos e Estabilidade Econômica)\n")
  cat(" 2. Engenharia (Pêndulo Não-Linear com Atrito)\n")
  cat(" 3. Biologia (Lotka-Volterra - Predador e Presa)\n")
  cat("----------------------------------------------------------------\n")
  
  modelo = as.integer(readline("Digite o número do modelo: "))
  
  tempos = seq(0, 50, by = 0.05) 
  
  if (modelo == 1) {
    
    print("Para Teste, use (X,Y,Z) = (2, 1 ,3) e (a, b, c) = (0.2, 0.9, 1.2)")
    
    cat("\n--- MODELO FINANCEIRO (MA-CHEN) ---\n")
    X_val = as.numeric(readline("Taxa de juros inicial (X): "))
    Y_val = as.numeric(readline("Demanda por investimento (Y): "))
    Z_val = as.numeric(readline("Índice de inflação (Z): "))
    
    a_val = as.numeric(readline("Parâmetro de poupança (a): "))
    b_val = as.numeric(readline("Custo de investimento (b): "))
    c_val = as.numeric(readline("Elasticidade da demanda (c): "))
    
    estado = c(X = X_val, Y = Y_val, Z = Z_val)
    parametros = c(a = a_val, b = b_val, c = c_val)
    
    dinamica = function(t, estado, parametros) {
      with(as.list(c(estado, parametros)), {
        dX = Z + (Y - a) * X
        dY = 1 - b * Y - X^2
        dZ = -X - c * Z
        return(list(c(dX, dY, dZ)))
      })  
    }
    
  } else if (modelo == 2) {
    cat("\n--- MODELO DE ENGENHARIA (PÊNDULO NÃO-LINEAR) ---\n")
    theta_val = as.numeric(readline("Ângulo inicial (em radianos, ex: 3.14): ")) 
    omega_val = as.numeric(readline("Velocidade angular inicial: "))
    
    g_val = as.numeric(readline("Gravidade (g): "))
    L_val = as.numeric(readline("Comprimento da haste (L): "))
    b_val = as.numeric(readline("Coeficiente de atrito (b): "))
    
    estado = c(Theta = theta_val, Omega = omega_val)
    parametros = c(g = g_val, L = L_val, b = b_val)
    
    dinamica = function(t, estado, parametros) {
      with(as.list(c(estado, parametros)), {
        dTheta = Omega
        dOmega = -(g / L) * sin(Theta) - b * Omega
        return(list(c(dTheta, dOmega)))
      })  
    }
    
  } else if (modelo == 3) {
    cat("\n--- MODELO BIOLÓGICO (LOTKA-VOLTERRA) ---\n")
    x_val = as.numeric(readline("População inicial de Presas (x): "))
    y_val = as.numeric(readline("População inicial de Predadores (y): "))
    
    alpha_val = as.numeric(readline("Taxa de crescimento da presa (alpha): "))
    beta_val  = as.numeric(readline("Taxa de caça (beta): "))
    delta_val = as.numeric(readline("Taxa de reprodução do predador (delta): "))
    gamma_val = as.numeric(readline("Taxa de mortalidade do predador (gamma): "))
    
    estado = c(Presa = x_val, Predador = y_val)
    parametros = c(alpha = alpha_val, beta = beta_val, delta = delta_val, gamma = gamma_val)
    
    dinamica = function(t, estado, parametros) {
      with(as.list(c(estado, parametros)), {
        dx = alpha * Presa - beta * Presa * Predador
        dy = delta * Presa * Predador - gamma * Predador
        return(list(c(dx, dy)))
      })  
    }
    
  } else {
    cat("\nOpção inválida! Voltando ao menu principal...\n")
    return() 
  }
  

  cat("\n[INFO] Resolvendo o sistema...\n")
  
  saida = ode(y = estado, times = tempos, func = dinamica, parms = parametros)
  df_saida = as.data.frame(saida)
  
  # 1. Gráfico Automático de Evolução Temporal (Todas as variáveis vs Tempo)
  plot(saida, main = "Evolução Temporal do Sistema")
  var1_nome = names(df_saida)[2]
  var2_nome = names(df_saida)[3]
  
  plot(df_saida[, 2], df_saida[, 3], type = "l", col = "firebrick", lwd = 2,
       main = "Espaço de Fase", xlab = var1_nome, ylab = var2_nome)
  
  cat("\n[SUCESSO] Gráficos gerados com sucesso!\n")
}


executa_dea = function() {
  # --- TELA 1: INTRODUÇÃO ---
  cat("\014") # Limpa a tela
  texto = paste(
    "================================================================\n",
    "       MÓDULO DE EFICIÊNCIA: ANÁLISE ENVOLTÓRIA DE DADOS (DEA)  \n",
    "================================================================\n",
    "A DEA é uma técnica avançada de Programação Linear usada para\n",
    "medir a eficiência relativa de múltiplas unidades (DMUs) que\n",
    "utilizam vários recursos (Inputs) para gerar resultados (Outputs).\n\n",
    "Conceitos Fundamentais:\n",
    "1. DMU (Unidade de Decisão): A entidade que será avaliada\n",
    "   (ex: Universidades Públicas, Hospitais, Lojas de Varejo).\n",
    "2. FRONTEIRA DE EFICIÊNCIA: O limite de desempenho ótimo. Quem\n",
    "   atinge 100% (Score = 1) compõe a fronteira e dita o mercado.\n",
    "3. PEERS (Benchmarking): As unidades eficientes que servem de\n",
    "   modelo e 'alvo' para que as unidades ineficientes melhorem.\n\n",
    "----------------------------------------------------------------\n",
    ">>> APLICAÇÃO PRÁTICA (Consultoria & Pesquisa):\n",
    "----------------------------------------------------------------\n",
    "A DEA não diz apenas QUEM é ineficiente, mas revela EXATAMENTE\n",
    "quanto o gestor precisa reduzir de custo ou aumentar de produção\n",
    "para se igualar aos melhores do seu setor.\n",
    "================================================================\n"
  )
  
  cat(texto)
  readline("Pressione [ENTER] para iniciar a avaliação de eficiência... ")
  
  # --- TELA 2: IMPORTAÇÃO DE DADOS ---
  cat("\014") # Limpa a tela
  cat("================================================================\n")
  cat(">>> IMPORTAÇÃO DE DADOS EXTERNOS <<<\n")
  cat("----------------------------------------------------------------\n")
  cat("O sistema está operando na pasta:\n", getwd(), "\n") 
  cat("Certifique-se de que o arquivo está nesta pasta.\n")
  cat("Use (Ctrl + Shift + h) para escolher o diretorio!\n")
  cat("----------------------------------------------------------------\n")
  cat("Nessa Versão estamos apenas lendo Planilhas Excel (.xlsx)\n")
  cat("Digite nesse formato: (nome_do_arquivo.xlsx)\n")
  
  readline("\nPressione [ENTER] para Continuar ou Escolha o diretorio usando Ctrl + Shift + h: \n")
  
  dados_excel = NULL 
  
  while (is.null(dados_excel)) {
    
    nome_excel = readline("Nome do Arquivo: ")
    
    if (!grepl("\\.xlsx$", nome_excel)) {
      nome_excel = paste0(nome_excel, ".xlsx")
    }
    
    if (file.exists(nome_excel)) {
      dados_excel = readxl::read_excel(nome_excel)
      cat("\n[SUCESSO] Planilha carregada!\n")
      
    } 
    
    else {
      cat("\n[ERRO] O arquivo '", nome_excel, "' não foi encontrado nesta pasta.\n", sep="")
      cat("Verifique se o nome está correto e tente novamente.\n\n")
    }
  }
  
  View(dados_excel)
  
  
  cat("\n[SUCESSO] Planilha carregada!\n")
  Sys.sleep(1)
  
  # --- TELA 3: MAPEAMENTO - PASSO 1 ---
  cat("\014") 
  cat("================================================================\n")
  cat(">>> ETAPA 1/3: IDENTIFICAÇÃO DAS UNIDADES (DMUs) <<<\n")
  cat("----------------------------------------------------------------\n")
  defina_dmu = readline("Defina a Coluna de DMU (Nome da Coluna): ")
  dmu_nomes = dados_excel[[defina_dmu]]
  
  # --- TELA 4: MAPEAMENTO - PASSO 2 ---
  cat("\014") 
  cat("================================================================\n")
  cat(">>> ETAPA 2/3: MAPEAMENTO DE RECURSOS (INPUTS) <<<\n")
  cat("----------------------------------------------------------------\n")
  cat("Dica: O usuário pode digitar uma ou mais colunas separadas por vírgula\n\n")
  input_cols = readline("Defina as Colunas de INPUT (ex: Custo, Horas): ")
  input_nomes = unlist(strsplit(input_cols, "[ ,]+"))
  X = as.matrix(dados_excel[, input_nomes])
  
  # --- TELA 5: MAPEAMENTO - PASSO 3 ---
  cat("\014") 
  cat("================================================================\n")
  cat(">>> ETAPA 3/3: MAPEAMENTO DE RESULTADOS (OUTPUTS) <<<\n")
  cat("----------------------------------------------------------------\n")
  output_cols = readline("Defina as Colunas de OUTPUT (ex: Producao, Receita): ")
  output_nomes = unlist(strsplit(output_cols, "[ ,]+"))
  Y = as.matrix(dados_excel[, output_nomes])
  
  cat("\n[INFO] Matrizes de eficiência construídas com sucesso!\n")
  Sys.sleep(1.5) 
  
  # --- TELA 6: TEORIA DE ESCALA ---
  cat("\014") 
  texto = paste(
    "================================================================\n",
    "       MODELAGEM DE FRONTEIRAS: CRS vs VRS (ESCALA)             \n",
    "================================================================\n",
    "Para medir a Eficiência Técnica, devemos definir o comportamento\n",
    "da escala de produção do sistema analisado:\n\n",
    "1. CRS (Constant Returns to Scale):\n",
    "   Assume que insumos e produtos variam proporcionalmente.\n",
    "   Ideal para quando não há barreiras de entrada ou limitações\n",
    "   físicas de crescimento. (Modelo Global)\n\n",
    "2. VRS (Variable Returns to Scale):\n",
    "   Flexibiliza a premissa anterior. Reconhece que aumentos nos\n",
    "   insumos não geram aumentos exatamente proporcionais.\n",
    "   Considera ganhos ou perdas de escala. (Modelo Local/Realista)\n\n",
    "----------------------------------------------------------------\n",
    ">>> INSIGHT TÉCNICO:\n",
    "O modelo VRS é geralmente preferido em consultorias, pois isola\n",
    "a Eficiência Técnica da Eficiência de Escala, sendo mais justo\n",
    "ao comparar empresas de portes distintos.\n",
    "================================================================\n"
  )
  cat(texto)
  readline("Pressione [ENTER] para selecionar o modelo e prosseguir... ")
  
  cat("\014") 
  cat("================================================================\n")
  cat(">>> CONFIGURAÇÃO DO MOTOR DEA <<<\n")
  cat("----------------------------------------------------------------\n")
  
  modelo_opt = as.integer(readline("Escolha o Modelo:\n 1 - VRS (Retornos Variáveis)\n 2 - CRS (Retornos Constantes)\n Opção: "))
  rts_escolhido = ifelse(modelo_opt == 1, "vrs", "crs")
  
  cat("\n")
  orient_opt = as.integer(readline("Orientação:\n 1 - Input (Minimizar Recursos)\n 2 - Output (Maximizar Resultados)\n Opção: "))
  orient_escolhida = ifelse(orient_opt == 1, "in", "out")
  
  
  cat("\014") 
  cat(paste("\n[SISTEMA] Processando DEA (", toupper(rts_escolhido), "-", toupper(orient_escolhida), ")...\n"))
  Sys.sleep(1.5)
  
  resultado_final = dea(X, Y, RTS = rts_escolhido, ORIENTATION = orient_escolhida)
  scores = eff(resultado_final)
  
  cat("\n================================================================\n")
  cat(">>> RELATÓRIO DE PERFORMANCE FINAL <<<\n")
  cat("----------------------------------------------------------------\n")
  
  for(i in 1:length(dmu_nomes)) {
    porcentagem = round(scores[i] * 100, 2)
    cat(paste("DMU:", dmu_nomes[i], "| Eficiência:", porcentagem, "%\n"))
  }
  cat("================================================================\n")
  
  readline("\nPressione [ENTER] para concluir a consultoria e voltar ao menu inicial.")
}


menu_matematico()
