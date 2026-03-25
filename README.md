# 🧮 A.M.P. | Assistente Matemático Aplicado (v1.0)

O **A.M.P.** é um ecossistema de funções desenvolvido em **R** focado em transformar modelos teóricos de Matemática Aplicada em ferramentas práticas de **Apoio à Decisão (Decision Science)**. Através de uma interface modular de terminal (CLI), o sistema permite a execução de algoritmos de otimização, simulação de sistemas complexos e inteligência de dados.

---

## 🏗️ Arquitetura e Módulos

O projeto foi estruturado para separar a **Lógica de Interface** dos **Motores de Cálculo**, garantindo escalabilidade para novos domínios matemáticos.

### 1. Otimização e Pesquisa Operacional
* **Programação Linear (Simplex):** Implementação via `lpSolve` para maximização de lucros e minimização de custos com análise de **Preços-Sombra (Shadow Prices)** e gargalos operacionais.
* **Análise Envoltória de Dados (DEA):** Avaliação de eficiência técnica relativa entre unidades (DMUs) utilizando modelos **VRS** (Variable Returns to Scale) e **CRS** (Constant Returns to Scale).

### 2. Dinâmica de Sistemas e Controle
* **Simulação de EDOs:** Resolução de equações diferenciais ordinárias via `deSolve`.
* **Modelagem Não-Linear:** Aplicação em modelos financeiros (Caos de Ma-Chen), biológicos (Lotka-Volterra) e engenharia (Pêndulo com Atrito).
* **Espaço de Fase:** Geração de visualizações gráficas da evolução dos estados do sistema.

### 3. Inteligência Artificial e Finanças (Roadmap 🚀)

Este módulo está sendo desenvolvido como parte da especialização técnica em **IA pelo CETAM**, unindo modelos estocásticos e aprendizado de máquina.

* **Machine Learning Preditivo:** * **Algoritmos:** Regressão Linear Múltipla e Random Forest.
    * **Aplicação:** Forecasting de demanda e predição de scores de eficiência para novas unidades de negócio (Data-Driven Discovery).
* **Engenharia Financeira (Markowitz):** * **Algoritmos:** Otimização Quadrática e Matrizes de Covariância.
    * **Aplicação:** Construção de carteiras de investimento com minimização de risco (volatilidade) para um retorno esperado fixo.
* **Algoritmos Genéticos (AG):** * **Aplicação:** Resolução de problemas de otimização combinatória (como o Problema do Caixeiro Viajante) onde métodos exatos do Simplex são computacionalmente inviáveis.
---

## 🛠️ Stack Tecnológica
* **Linguagem:** R
* **Bibliotecas Principais:** `lpSolve`, `Benchmarking`, `deSolve`, `readxl`, `ggplot2`
* **Interface:** Command Line Interface (CLI) modular.

## 🚀 Como Executar
1. Clone o repositório:
   ```bash
   git clone [https://github.com/SEU_USUARIO/NOME_DO_REPOSITORIO.git](https://github.com/SEU_USUARIO/NOME_DO_REPOSITORIO.git)
