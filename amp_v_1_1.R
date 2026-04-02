library(lpSolve)
library(ggplot2)
library(deSolve)
library(Benchmarking)
library(readxl)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(tidyr)
library(CVXR)




menu_matematico = function() {
  opcao = "" 
  while(opcao != "0") {
    
    cat("\014") 
    cat("=================================================================\n")
    cat("      A.M.P. | ASSISTENTE MATEMÁTICO APLICADO - v1.1             \n")
    cat("=================================================================\n\n")
    
    cat(" [ 1 ] OTIMIZAÇÃO (PL, DEA, Algoritmos Genéticos)\n")
    cat(" [ 2 ] DINÂMICA E MODELAGEM (EDOs, Caos, Controle Ótimo)\n")
    cat(" [ 3 ] FINANÇAS E ATUÁRIA (Markowitz, Amortização, Risco Atuarial)\n")
    cat(" [ 4 ] ESTATÍSTICA E PROBABILIDADE (Inferência, Regressão, Markov)\n")
    cat(" [ 5 ] MATEMÁTICA COMPUTACIONAL (Análise Numérica e Grafos)\n")
    cat(" [ 6 ] TEORIA DOS JOGOS (Simulação de Estratégias e Conflitos)\n")
    cat(" [ 0 ] ENCERRA O A.M.P\n")
    
    opcao = readline(">> Selecione o módulo desejado: ")
    
    
    if (opcao == "1") {
      cat("\n[ACESSO] Entrando no domínio de OTIMIZAÇÃO...\n")
      Sys.sleep(1)
      submenu_otimizacao()
      
    } 
    
    else if (opcao == "2") {
      cat("\n[ACESSO] Entrando no domínio de DINÂMICA E MODELAGEM...\n")
      Sys.sleep(1)
      submenu_dinamica()
      
    } 
    
    else if (opcao == "3") {
      cat("\n[ACESSO] Entrando no domínio de FINANÇAS E ATUÁRIA...\n")
      Sys.sleep(1)
      submenu_financas()
      
    }
    
    else if (opcao == "4") {
      cat("\n[ACESSO] Entrando no domínio de ESTATISTICA E PROBABILIDADE...\n")
      Sys.sleep(1)
      
    }
    
    else if (opcao == "5") {
      cat("\n[ACESSO] Entrando no domínio de MATEMÁTICA COMPUTACIONAL...\n")
      readline("EM DESENVOLVIMENTO... APERTE [ENTER] PARA VOLTAR PARA O MENU PRINCIPAL.")
      Sys.sleep(1)
     
    }
    
    else if (opcao == "6") {
      cat("\n[ACESSO] Entrando no domínio de TEORIA DOS JOGOS...\n")
      readline("EM DESENVOLVIMENTO... APERTE [ENTER] PARA VOLTAR PARA O MENU PRINCIPAL.")
      Sys.sleep(1)
    }
    
    else if (opcao == "0") {
      cat("\n[SISTEMA] Encerrando o A.M.P... Até a próxima análise!\n")
      Sys.sleep(1)
    }
  }
}


submenu_otimizacao = function() {
  sub_opcao = ""
  
  while(sub_opcao != "0") {
    cat("\014")
    cat("=================================================================\n")
    cat("          A.M.P. | MÓDULO 1: OTIMIZAÇÃO E PESQUISA OP.           \n")
    cat("=================================================================\n")
    cat("A Pesquisa Operacional (P.O.) utiliza modelos matemáticos para\n")
    cat("maximizar a eficiência produtiva e otimizar a alocação de recursos\n")
    cat("escassos, eliminando gargalos operacionais e reduzindo custos.\n")
    cat("-----------------------------------------------------------------\n\n")
    
    cat(" Selecione a metodologia de análise:\n\n")
    cat(" [ 1 ] PROGRAMAÇÃO LINEAR (Simplex)\n")
    cat("       - Foco: Mix de produção, logística e custos.\n\n")
    
    cat(" [ 2 ] ANÁLISE ENVOLTÓRIA DE DADOS (DEA)\n")
    cat("       - Foco: Benchmarking e ranking de eficiência relativa.\n\n")
    
    cat(" [ 3 ] ALGORITMOS GENÉTICOS (Heurísticas)\n")
    cat("       - Foco: Problemas complexos e roteamento (NP-Difícil).\n\n")
    
    cat(" [ 0 ] VOLTAR AO MENU PRINCIPAL\n")
    cat("=================================================================\n")
    
    sub_opcao = readline(">> Selecione a ferramenta: ")
    
    if (sub_opcao == "1") {
      executa_pl()
    } else if (sub_opcao == "2") {
      executa_dea()
    } else if (sub_opcao == "3") {
      cat("\n[AVISO] Módulo de Algoritmos Genéticos em desenvolvimento... \n")
      Sys.sleep(2)
    }
  }
}

submenu_dinamica = function() {
  sub_opcao = ""
  while(sub_opcao != "0") {
    cat("\014")
    cat("=================================================================\n")
    cat("          A.M.P. | MÓDULO 2: DINÂMICA E MODELAGEM               \n")
    cat("=================================================================\n")
    cat("Sistemas Dinâmicos analisam a evolução de estados sob regras\n")
    cat("determinísticas. Este módulo foca na sensibilidade às condições\n")
    cat("iniciais (Caos), estabilidade de equilíbrios e o controle de\n")
    cat("trajetórias em sistemas complexos e não-lineares.\n")
    cat("-----------------------------------------------------------------\n\n")
    
    cat(" Selecione a metodologia de análise:\n\n")
    cat(" [ 1 ] SIMULAÇÃO DE SISTEMAS (EDOs)\n")
    cat("       - Foco: Modelos de Caos (Ma-Chen) e Biologia (Predador-Presa).\n\n")
    
    cat(" [ 2 ] ANÁLISE DE ESTABILIDADE E FASE\n")
    cat("       - Foco: Atratores, Retratos de Fase e Pontos de Equilíbrio.\n\n")
    
    cat(" [ 3 ] CONTROLE ÓTIMO NÃO-LINEAR\n")
    cat("       - Foco: Estabilização de Sistemas Caóticos (PIBIC/FAPEAM).\n\n")
    
    cat(" [ 0 ] VOLTAR AO MENU PRINCIPAL\n")
    cat("=================================================================\n")
    
    sub_opcao = readline(">> Selecione a ferramenta: ")
    
    if (sub_opcao == "1") {
      cat("\n[INFO] Sistemas Caóticos (EDOS)...\n")
      Sys.sleep(2)
      executa_sistemas_dinamicos()
    } 
    
    else if (sub_opcao == "2") {
      cat("\n[INFO] Gerando Retrato de Fase e Análise de Estabilidade...\n")
      Sys.sleep(2)
      
    } 
    
    else if (sub_opcao == "3") {
      cat("\n[AVISO] Módulo de Controle Ótimo (Tema PIBIC) em implementação...\n")
      Sys.sleep(2)
    }
  }
}

submenu_financas = function() {
  sub_opcao = ""
  
  while(sub_opcao != "0") {
    cat("\014")
    cat("=================================================================\n")
    cat("          A.M.P. | MÓDULO 3: FINANÇAS E ATUÁRIA                 \n")
    cat("=================================================================\n")
    cat("A Engenharia Financeira utiliza a matemática para quantificar o\n")
    cat("risco e maximizar o retorno. Este módulo aplica a Teoria Moderna\n")
    cat("do Portfólio (Markowitz), análise de fluxos de caixa e modelos\n")
    cat("atuariais para garantir a sustentabilidade de ativos e passivos.\n")
    cat("-----------------------------------------------------------------\n\n")
    
    cat(" Selecione a ferramenta financeira:\n\n")
    cat(" [ 1 ] OTIMIZAÇÃO DE CARTEIRA (Markowitz)\n")
    cat("       - Foco: Fronteira Eficiente, Risco (Sigma) e Retorno.\n\n")
    
    cat(" [ 2 ] MATEMÁTICA FINANCEIRA (Amortização)\n")
    cat("       - Foco: Sistemas SAC e PRICE, VPL e TIR.\n\n")
    
    cat(" [ 3 ] MATEMÁTICA ATUARIAL E RISCO\n")
    cat("       - Foco: Tábuas de Mortalidade e Probabilidade de Sobrevivência.\n\n")
    
    cat(" [ 0 ] VOLTAR AO MENU PRINCIPAL\n")
    cat("=================================================================\n")
    
    sub_opcao = readline(">> Selecione a ferramenta: ")
    
    if (sub_opcao == "1") {
      cat("\n[INFO] Instanciando modelo de Markowitz... \n")
      Sys.sleep(1.5)
      executa_markowitz()
    } 
    
    else if (sub_opcao == "2") {
      cat("\n[INFO] Abrindo calculador de sistemas de amortização...\n")
      Sys.sleep(1.5)
      # executa_amortizacao()
    } 
    
    else if (sub_opcao == "3") {
      cat("\n[AVISO] Módulo de Atuária em fase de implementação técnica.\n")
      Sys.sleep(2)
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
  } 
  
  
  else if (modelo == 2) {
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
    
  } 
  
  
  else if (modelo == 3) {
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
    
  } 
  
  else {
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
  
  readline("Voltando para o Menu Principal... Aperte [ENTER]")
  
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
  
  
  cat("\014") 
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

executa_markowitz = function() {
  cat("\014")
  cat("=================================================================\n")
  cat("          A.M.P. | CARTEIRA DE MARKOWITZ (Média-Variância)       \n")
  cat("=================================================================\n")
  cat("Este módulo utiliza a Programação Quadrática para encontrar a    \n")
  cat("combinação ideal de ativos que minimiza o risco (volatilidade)   \n")
  cat("para um determinado nível de retorno esperado.                   \n\n")
  
  cat("Conceitos Matemáticos Aplicados:\n")
  cat("1. Função Objetivo: Minimizar a Variância da Carteira (Risco).\n")
  cat("2. Matriz de Covariância: Mede a interdependência entre os ativos.\n")
  cat("3. Fronteira Eficiente: O lugar geométrico das carteiras ótimas.\n")
  cat("=================================================================\n")
  
  
  readline("Pressione [ENTER] para continuar...")
  
  cat("\014") 
  cat("================================================================\n")
  cat(">>> IMPORTAÇÃO DE DADOS EXTERNOS <<<\n")
  cat("----------------------------------------------------------------\n")
  cat("O sistema está operando na pasta:\n", getwd(), "\n\n") 
  cat("Certifique-se de que o arquivo está nesta pasta.\n")
  cat("Use (Ctrl + Shift + h) para escolher o diretorio!\n")
  cat("----------------------------------------------------------------\n")
  cat("Nessa Versão estamos apenas lendo Planilhas Excel (.xlsx)\n")
  cat("Digite nesse formato: (nome_do_arquivo.xlsx)\n")
  
  readline("Pressione [ENTER] para Continuar ou Escolha o diretorio usando Ctrl + Shift + H:")
  
  repeat {
    arquivo = readline(">> Digite o nome do arquivo (ex: dados.xlsx) ou '0' para cancelar: ")
    
    if (arquivo == "0") {
      cat("\n[SISTEMA] Operação cancelada. Retornando...\n")
      Sys.sleep(1)
      return() 
    }
    
    if (file.exists(arquivo)) {
      cat("\n[SUCESSO] Arquivo detectado com êxito!\n")
      cat("[INFO] Processando matriz de covariância e retornos...\n")
      
      tryCatch({
        library(readxl)
        dados_brutos = read_excel(arquivo)
        
        cat("[OK] Dados carregados para a memória.\n")
        Sys.sleep(1.5)
        break
        
      }, error = function(e) {
        cat("\n[ERRO CRÍTICO] O arquivo existe, mas o R não conseguiu lê-lo.\n")
        cat("Verifique se ele é um Excel válido e não está corrompido.\n")
      })
      
    } 
    
    
    else {
      cat("\n[ERRO] O arquivo '", arquivo, "' não foi encontrado na pasta:\n", sep="")
      cat(getwd(), "\n")
      cat("Verifique a ortografia e a extensão (.xlsx)!\n\n")
      Sys.sleep(2)
    }
  }
  
  cat("\n[SISTEMA] Estrutura da Planilha Identificada:\n")
  nomes_colunas = colnames(dados_brutos)
  
  for(i in 1:length(nomes_colunas)) {
    cat(sprintf(" [ %d ] %s\n", i, nomes_colunas[i]))
  }
  cat("-----------------------------------------------------------------\n")
  col_data = as.integer(readline(">> Digite o NÚMERO da coluna que contém as DATAS: "))
  
  if (is.na(col_data) || col_data < 1 || col_data > length(nomes_colunas)) {
    cat("[ERRO] Coluna inválida. Retornando ao menu...\n")
    return()
  }
  
  cat("\n[INFO] Configurando", nomes_colunas[col_data], "como eixo de tempo...\n")
  
  datas = as.Date(dados_brutos[[col_data]])
  
  retornos_puros = dados_brutos[ , -col_data] 
  
  retornos_puros = as.data.frame(lapply(retornos_puros, as.numeric))
  
  retornos_matriz = as.matrix(retornos_puros)
  retornos_xts = xts(retornos_matriz, order.by = datas)
  
  
  port_spec = portfolio.spec(assets = colnames(retornos_xts))
  
  port_spec = add.constraint(portfolio = port_spec, type = "full_investment")
  
  port_spec = add.constraint(portfolio = port_spec, type = "long_only")

  port_spec = add.objective(portfolio = port_spec, type = "risk", name = "StdDev")
  
  otimizacao = optimize.portfolio(R = retornos_xts, 
                                  portfolio = port_spec, 
                                  optimize_method = "ROI", 
                                  trace = TRUE)
  
  pesos_otimos = extractWeights(otimizacao)
  risco_carteira = extractStats(otimizacao)["StdDev"]
  Sys.sleep(2)
  
  
  readline("[SISTEMA] Calculos sendo EXECUTADOS, Pressione [ENTER] para continuar...")
  
  
  
  cat("\n=================================================================\n")
  cat("          RESULTADO DA OTIMIZAÇÃO - MÉDIA-VARIÂNCIA             \n")
  cat("=================================================================\n")
  cat(sprintf("Risco Diário da Carteira (Desvio Padrão): %.4f%%\n", risco_carteira * 100))
  cat("-----------------------------------------------------------------\n")
  cat("ALOCAÇÃO SUGERIDA:\n\n")
  
  for(i in 1:length(pesos_otimos)) {
    nome_ativo = names(pesos_otimos)[i]
    percentual = pesos_otimos[i] * 100
    barra = paste(rep("=", as.integer(percentual/5)), collapse = "")
    cat(sprintf(" [%-8s] : [%-20s] %.2f%%\n", nome_ativo, barra, percentual))
  }
  cat("=================================================================\n")
  chart.Weights(otimizacao, main = "Alocação Ótima de Ativos - A.M.P.")
  
  
  readline("Precione [ENTER] para voltar para o MENU do MODULO 3")
  
  
}

menu_matematico()
