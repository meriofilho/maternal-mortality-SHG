# Óbitos Maternos por Síndromes Hipertensivas na Gestação (SHG) — Piauí, 2019–2024

Análise descritiva do perfil epidemiológico dos óbitos maternos por
Síndromes Hipertensivas na Gestação (SHG) no Estado do Piauí, com
comparação à Região de Saúde dos Cocais, no período de 2019 a 2024.

CID-10 considerados: **O10, O11, O13, O14, O15 e O16**.

## Autores

- Susana Silva Lima
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
├── analise_shg_piaui.html         # relatório já renderizado (abrir no navegador)
├── dados/                         # planilhas originais baixadas do Tabnet
├── figuras/                       # gráficos exportados (PNG 300dpi + PDF vetorial)
└── tabelas/                       # tabelas de apoio exportadas (CSV, ; e UTF-8/BOM)
```

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

## Limitação metodológica importante

A comparação de Razão de Mortalidade Materna (RMM) entre o Estado e a
Região dos Cocais (Figura 7) usa óbitos maternos de **todas as causas**,
não apenas SHG — o Tabnet não disponibiliza uma tabela pronta cruzando
CID-10 × Região de Saúde × Ano. Para obter a contagem de óbitos por SHG
especificamente na Região dos Cocais, é necessário gerar uma consulta
adicional no Tabnet (mesma tela de "Óbitos por Categoria CID-10 e Ano",
filtrando Região de Saúde = Cocais).
