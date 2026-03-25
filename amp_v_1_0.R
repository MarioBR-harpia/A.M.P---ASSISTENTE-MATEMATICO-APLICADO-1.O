library(lpSolve)





# 1. Função Objetivo (MaxZ = 40x_1 + 70x_2)
func_obj = c(40, 70)

# 2. Matriz de Restrições (Coeficientes)
restricao = matrix(c(1, 2,  # Linha 1: Restrição de Placas (1x_1 + 2x_2)
                      2, 1), # Linha 2: Restrição de Horas (2x_1 + 1x_2)
                    nrow = 2, 
                    byrow = TRUE)

# 3. Sinais das Restrições
direcao = c("<=", "<=")

# 4. Limites Disponíveis (Lado Direito da Equação)
limites = c(300, 240) # 300 placas e 240 horas

# 5. Otimizando (O "Motor" da Programação Linear)
solucao = lp(direction = "max", 
              objective.in = func_obj, 
              const.mat = restricao, 
              const.dir = direcao, 
              const.rhs = limites)

solucao_final = solucao$solution
print(solucao_final) #Retorna 60 e 120, ou seja, MaxZ = 40 * 60 + 70 * 120 => MaxZ = 2400 + 

library(ggplot2)

# 1. Definir os vértices da Região Factível (Polígono de possibilidades)
# Os pontos de limite são: Origem(0,0), Eixo Y(0, 120), Interseção(120, 60), Eixo X(150, 0)
regiao_factivel = data.frame(
  x = c(0, 0, 120, 150),
  y = c(0, 120, 60, 0)
)

# 2. Plotar o Gráfico Visual
ggplot() +
  # Linhas das restrições (Desenhando os limites)
  geom_abline(intercept = 300, slope = -2, color = "orange", linetype = "dashed", size = 1) +
  geom_abline(intercept = 120, slope = -0.5, color = "blue", linetype = "dashed", size = 1) +
  
  # Pintar a Região Factível (Onde a fábrica pode operar)
  geom_polygon(data = regiao_factivel, aes(x = x, y = y), fill = "seagreen", alpha = 0.3) +
  
  # Ponto Ótimo encontrado pelo lpSolve
  geom_point(aes(x = 120, y = 60), color = "red", size = 4) +
  geom_text(aes(x = 120, y = 60, label = "Lucro Máximo\n(120, 60)"), 
            vjust = -1.5, color = "red", fontface = "bold") +
  
  # Rótulos e Títulos
  labs(title = "Otimização de Mix de Produção com Programação Linear",
       subtitle = "Área verde: Combinações possíveis respeitando os limites de estoque e horas.\nLinhas tracejadas: Restrições de produção.",
       x = "Quantidade do Produto 1 (x_1)",
       y = "Quantidade do Produto 2 (x_2)") +
  
  # Estética e limites do gráfico
  theme_minimal() +
  coord_cartesian(xlim = c(0, 200), ylim = c(0, 150))

