# QualiR: análise qualitativa colaborativa em R

O QualiR é um aplicativo Shiny para análise qualitativa de textos e transcrições. A tela tem barra lateral de navegação, faixa de opções no topo, lista de itens e documento lado a lado, com cada item aberto em uma aba. Várias pessoas podem trabalhar no mesmo projeto ao mesmo tempo.

## 1. Instalação (uma vez)

Requisito: R 4.2 ou mais recente (o RStudio é opcional).

```r
setwd(choose.dir())          # escolha a pasta qualiR
source("instalar_pacotes.R")
```

## 2. Abrir o programa

- **Com dois cliques:** use **Iniciar QualiR.bat**, que fica dentro da pasta qualiR. Para trabalhar em equipe, use **Iniciar QualiR (equipe).bat**.
- **Pelo RStudio:** `setwd(choose.dir())` e depois `shiny::runApp(".")`.

No primeiro acesso, entre com o usuário **admin** e a senha **admin**, e troque a senha em **Arquivo > Trocar senha**.
O projeto inteiro fica no arquivo `projeto_qualir.sqlite`, dentro da pasta.

## 3. A tela

| Área | O que faz |
|---|---|
| **Barra lateral** (à esquerda) | Itens recentes; Dados (Arquivos, Classificações de arquivo); Nós (Nós, Relacionamentos, Tipos de relacionamento); Casos; Notas (Memorandos, Anotações); Conjuntos; Consultas (Consultas, Resultados, Matrizes); Visualizações; Relatórios; Equipe |
| **Faixa de opções** | Abas Arquivo, Início, Importar, Criar, Análise, Exportar e Colaboração. O botão **Recolher**, no canto direito, esconde a faixa para ganhar espaço |
| **Lista** (meio) | Itens da seção escolhida. Um duplo clique abre o item |
| **Detalhe** (direita) | O que você abriu, em abas. **Maximizar** esconde a lista e deixa o texto ocupando a tela toda |
| **Rodapé do documento** | Barra **Lista [Nós/Casos] — Codificar em [...]**. **Ctrl+Q** leva direto para ela |

Para ler textos longos no notebook, use **Recolher** (faixa de opções) e **Maximizar** (lista). Assim o documento ocupa quase a tela inteira.

## 4. Codificar

Selecione um trecho do texto e use qualquer uma destas formas:
- **arraste** a seleção até um nó na lista de Nós; soltá-la em "Arraste a seleção aqui…" cria um nó novo;
- clique com o **botão direito** e escolha o nó (dá para digitar e filtrar), crie um novo ou faça uma anotação;
- use a barra **Codificar em** (Ctrl+Q). Se o nome digitado não existir, o nó é criado;
- use os botões da barra do documento: Codificar seleção, Nó e Descodificar.

Também estão disponíveis:
- **Início > Autocodificar:** cria casos ou nós por falante, ou codifica os parágrafos que contêm um termo.
- **Início > Codificar intervalo:** codifica do parágrafo X ao Y (os números aparecem à esquerda do texto).
- **Clique num trecho destacado:** mostra quem codificou e em qual nó, e permite remover.
- **Faixas de codificação:** por nó ou por codificador. Clique numa faixa para destacar só aquele nó.
- **Anotações:** ficam sublinhadas em azul no texto e numeradas no rodapé do documento.

## 5. Funções

- **Arquivos:** importa `.txt`, `.docx`, `.pdf`, `.vtt` e `.srt`, ou texto colado. Transcrições `Falante: fala` são reconhecidas automaticamente.
- **Livro de nós:** importação de Excel/CSV (Importar > Livro de nós) e exportação para Word ou Excel.
- **Nós hierárquicos:** arraste um nó sobre outro para torná-lo nó filho. Também é possível mesclar nós e definir cor e descrição.
- **Casos, atributos e classificações:** de caso e de arquivo, em planilhas editáveis (duplo clique na célula e Enter para salvar).
- **Relacionamentos e tipos de relacionamento:** associativo, unidirecional ou simétrico.
- **Conjuntos:** agrupam arquivos ou nós e podem ser usados como escopo das consultas.
- **Memorandos:** do projeto, de um arquivo ou de um nó. **Anotações:** em trechos do texto.
- **Consultas:**
  - Frequência de palavras;
  - Pesquisa de texto (com `*`, aspas e OU);
  - Árvore de palavras;
  - Consulta de codificação (E / OU / NÃO, filtrável por casos ou atributos);
  - Matriz de codificação (por arquivos, casos, atributos de caso ou de arquivo, ou codificadores);
  - Coocorrência;
  - Análise de agrupamento;
  - Comparação de codificação (Kappa de Cohen).
- **Consultas salvas, resultados salvos e matrizes salvas:** na seção Consultas.
- **Visualizações:** gráfico hierárquico, mapa do projeto, mapa de relacionamentos, rede de coocorrência e gráfico de codificação por arquivo. Todas podem ser salvas em PNG.
- **Relatórios:** nós, arquivos, casos, codificação por arquivo, trechos, anotações e equipe. Exportam para Word ou Excel.
- **Exportações:** codificação completa (Excel) e arquivo aberto com sua codificação (Word).

## 6. Trabalho em equipe

O app roda em um computador ou servidor, e os colegas acessam pelo navegador.

- **Na rede local:** use **Iniciar QualiR (equipe).bat**. A janela mostra o endereço que os colegas devem abrir.
- **Pela internet:** use Shiny Server ou Posit Connect. Não use o shinyapps.io: ele apaga arquivos ao reiniciar.
- **Usuários e perfis:** administrador, pesquisador(a) ou leitor(a), em Equipe > Usuários.
- As alterações aparecem para todos em cerca de 1 segundo, com aviso de quem fez o quê. Cada codificação registra o autor.

## 7. Limitações

- O texto não pode ser editado depois de importado (reimporte o arquivo para corrigir).
- O app trabalha só com texto: não codifica áudio, vídeo ou imagem.
- O banco SQLite atende bem equipes de até cerca de 15 pessoas codificando ao mesmo tempo.
