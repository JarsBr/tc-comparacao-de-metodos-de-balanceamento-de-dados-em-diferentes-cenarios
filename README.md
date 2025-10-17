# 🧠 Projeto TC — Pipeline de Análise de Bases Desbalanceadas

Este repositório contém o projeto do Trabalho de Conclusão (TC), cujo objetivo é **avaliar diferentes técnicas de balanceamento de dados** e seu impacto em **modelos de classificação supervisionada**.  
O fluxo completo foi implementado em **R**, dividido em etapas modulares para garantir reprodutibilidade e clareza.

---

## ⚙️ Estrutura do Projeto

```text
projeto_tc/
│
├── data/
│   ├── raw/          # Bases originais (cada base em uma subpasta)
│   ├── processed/    # Dados pré-processados e resultados intermediários
│
├── resultados/
│   ├── metricas/     # Tabelas de métricas (.csv)
│   ├── graficos/     # Visualizações geradas (.png)
│
├── scripts/
│   ├── 01_preprocessamento.R  # Leitura, limpeza e padronização das bases
│   ├── 02_balanceamentos.R    # Aplicação de SMOTE, ROSE e SDA
│   ├── 03_modelos.R           # Treinamento e avaliação de modelos
│   ├── 04_analises.R          # Consolidação e geração de gráficos
│
└── main.R                     # Script principal que executa todo o pipeline
```

---

## 🔄 Fluxo do Pipeline

O projeto foi estruturado em **etapas sequenciais**, controladas pelo script `main.R`.  

### 1️⃣ Pré-processamento
Executado por [`01_preprocessamento.R`](scripts/01_preprocessamento.R)  
- Lê todas as bases de `data/raw/`  
- Faz limpeza, padronização de nomes e variáveis  
- Detecta variável-alvo (`Class`)  
- Salva a lista consolidada em `data/processed/lista_bases_raw.RData`

---

### 2️⃣ Balanceamento de Dados
Executado por [`02_balanceamentos.R`](scripts/02_balanceamentos.R)  
- Gera versões balanceadas das bases usando:
  - **SMOTE** (`smotefamily`)
  - **ROSE** (`ROSE`)
  - **Subamostragem Aleatória (SDA)** (`dplyr`)
- Salva em:
  - `lista_bases_smote.RData`
  - `lista_bases_rose.RData`
  - `lista_bases_sda.RData`

---

### 3️⃣ Modelagem
Executado por [`03_modelos.R`](scripts/03_modelos.R)  
- Treina e avalia três algoritmos:
  - Regressão Logística (`glm`)
  - Árvore de Decisão (`rpart`)
  - Random Forest (`randomForest`)
- Usa particionamento treino/teste (70/30)
- Calcula métricas com `caret::confusionMatrix()`:
  - **Acurácia**, **Precisão**, **Recall** e **F1-Score**
- Salva os resultados em:
  - `resultados_raw.RData`
  - `resultados_smote.RData`
  - `resultados_rose.RData`
  - `resultados_sda.RData`

---

### 4️⃣ Análise e Visualização
Executado por [`04_analises.R`](scripts/04_analises.R)  
- Consolida todas as métricas em um único dataset  
- Gera tabelas e gráficos comparando desempenho entre:
  - Modelos
  - Técnicas de balanceamento  
- Salva:
  - **CSV**: `metricas_resumo.csv`, `metricas_por_base.csv`
  - **Gráficos**:
    - `comparacao_F1.png`
    - `boxplot_Acuracia.png`
    - `heatmap_F1.png`

---

## ▶️ Execução Completa

Para executar o pipeline completo:

Execute o main.r
