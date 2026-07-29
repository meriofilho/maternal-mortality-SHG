# Óbitos Maternos por Síndromes Hipertensivas na Gestação (SHG) — Piauí, 2019–2024

Análise descritiva do perfil epidemiológico dos óbitos maternos por
Síndromes Hipertensivas na Gestação (SHG) no Estado do Piauí, com
comparação à Região de Saúde dos Cocais, no período de 2019 a 2024.

CID-10 considerados: **O10, O11, O13, O14, O15 e O16**.

## Autores

- **Susana Silva Lima**
- Rosa Maria do Rego Lima
- **Romério de Oliveira Lima Filho** — responsável pela análise
- Mara Regina Pereira Viana Damasceno Feitosa

## Fonte dos dados

MS/SVSA/CGIAE — Sistema de Informações sobre Mortalidade (SIM) e Sistema
de Informações sobre Nascidos Vivos (SINASC), obtidos via DATASUS/Tabnet
(dados de domínio público).

## Estrutura do repositório

```
├── analise_shg_piaui.Rmd          # script de análise (R Markdown)
├── analise_shg_piaui.html         # relatório técnico renderizado (código + figuras)
├── analise_shg_piaui.md           # mesmo relatório em Markdown puro (visualização no GitHub)
├── analise_shg_piaui_files/       # figuras usadas pelo .md
├── resultados_shg_piaui.html      # relatório só de resultados, para colaboradores (sem código)
├── dados/                         # planilhas originais baixadas do Tabnet
├── figuras/                       # gráficos exportados (PNG 300dpi + PDF vetorial)
└── tabelas/                       # tabelas de apoio exportadas (CSV, ; e UTF-8/BOM)
```

O `.Rmd` tem dois formatos de saída no cabeçalho YAML — no RStudio, o botão
**Knit** deixa escolher entre "HTML" e "GitHub Document (Markdown)".

## Como reproduzir a análise

1. Instale o [R](https://cloud.r-project.org/) e o [RStudio](https://posit.co/download/rstudio-desktop/).
2. Abra `analise_shg_piaui.Rmd` no RStudio.
3. `Session > Set Working Directory > To Source File Location`.
4. Clique em **Knit** (ou rode os chunks um a um) — o script instala
   sozinho os pacotes que faltarem na primeira execução.
5. Figuras (PNG + PDF) são salvas em `figuras/` e tabelas em `tabelas/`.

Os gráficos usam a fonte **Roboto** (baixada automaticamente do Google
Fonts via `sysfonts`/`showtext` na primeira execução — é necessário
estar conectado à internet nesse momento).

## Análises estatísticas (além do descritivo)

Seções 15–17 do `.Rmd` vão além da estatística descritiva:

- **Seção 15** — teste exato de Fisher comparando o perfil sociodemográfico
  de quem morreu por SHG com o de quem morreu por outras causas maternas
  (faixa etária, raça/cor, escolaridade, local de ocorrência).
- **Seção 16** — IC 95% da proporção de SHG por ano, teste de tendência
  de Cochran-Armitage e regressão de Poisson na RMM específica de SHG.
- **Seção 17** — RMM específica de SHG comparando Piauí x Região dos
  Cocais, incluindo teste de razão de taxas (Poisson exato) Cocais x
  resto do Estado.

A Seção 17 usa duas tabelas extras (`dados/shg_regiao_saude_2019_2024.tsv`
e `dados/shg_cocais_ano_2019_2024.tsv`) obtidas com uma consulta manual
no Tabnet (SIM/PI), já que o Tabnet não gera uma tabela pronta cruzando
CID-10 × Região de Saúde × Ano — foi preciso rodar a consulta filtrando
"Região de Saúde = Cocais" antes de exportar. Antes dessas duas tabelas,
a RMM Piauí x Cocais (Figura 7) só podia ser calculada com óbitos
maternos de todas as causas — essa limitação está resolvida na Seção 17.

- **Seção 18** — óbitos maternos e óbitos por SHG nas 12 Regiões de
  Saúde do Piauí (não só Cocais), com teste de Fisher.
- **Seção 19** — microdado individual do SIM (Declaração de Óbito
  anonimizada, via pacote `microdatasus`/OpenDataSUS): idade exata,
  sazonalidade (mês do óbito) e município exato de cada óbito por SHG.
  Requer o [Rtools](https://cran.r-project.org/bin/windows/Rtools/) no
  Windows (compila `microdatasus` e `read.dbc`, que não estão no CRAN).
  O download é cacheado em `dados/sim_pi_obitos_maternos_2019_2024.rds`
  na primeira execução. Validamos que o filtro por CID de SHG neste
  microdado reproduz exatamente os mesmos 62 óbitos usados no resto da
  análise, ano a ano.
