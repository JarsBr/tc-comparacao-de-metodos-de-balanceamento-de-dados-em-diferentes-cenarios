# Passo a passo para execução

1. Instalar as dependências

- Instale todos os pacotes comentados no arquivo Analise.R
- Execute `Modelos.R` (este script não gera saída visível).

2. Em seguida, execute os scripts de balanceamento (também sem saída visível):
- `BalancementoComRose.R`
- `BalancementoComSMOTE.R`
- `BalancementoSimplesDA.R`

3. Após rodar os scripts acima, execute as análises específicas para cada balanceamento:
- `AnalisesRawData.R`
- `AnalisesRose.R`
- `AnalisesSmote.R`
- `AnaliseOversamplingSimples.R`
  
> [!IMPORTANT]
> A execução dos códigos pode demorar e os resultados estão sendo exibidos apenas no console.

> [!NOTE]
> Cada script de análises corresponde a um método balanceamento diferente. Para cada método, treinamos os seguintes modelos:
> 
> ***Random Forest***
> > package("randomForest")
> 
> ***LightGBM***
> > package("lightgbm")
> 
> ***SVM-RFE***
> > package("caret"), package("e1071")

# Observações importantes

O arquivo Analise.R não está mais em uso (com erro: object 'dados_treino' not found).

Os gráficos são gerados no arquivo `Graficos.R`, mas atualmente os resultados são inseridos manualmente no código, devido ao longo tempo de execução de cada modelo. É necessário automatizar esse processo.
