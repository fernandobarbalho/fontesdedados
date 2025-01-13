# Carregar o pacote necessário
library(httr)
library(xml2)

# URL do web service
url <- "https://webservices.fazenda.sp.gov.br/WSTransparencia/TransparenciaServico.asmx"


# Credenciais do usuário
usuario <- "usuario"  # Substitua pelo nome do usuário real
senha <- "senha"      # Substitua pela senha real

# Corpo da requisição SOAP, incluindo o cabeçalho de autenticação
soap_body <- sprintf('
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
  <soap:Header>
    <AutenticacaoHeader xmlns="http://fazenda.sp.gov.br/wstransparencia">
      <Usuario>%s</Usuario>
      <Senha>%s</Senha>
    </AutenticacaoHeader>
  </soap:Header>
  <soap:Body>
    <ConsultarDespesas xmlns="http://fazenda.sp.gov.br/wstransparencia">
      <ano>2010</ano>
      <codigoOrgao>20000</codigoOrgao>
      <codigoGrupo>31</codigoGrupo>
      <codigoModalidade>3190</codigoModalidade>
      <flagEmpenhado>0</flagEmpenhado>
      <flagLiquidado>0</flagLiquidado>
      <flagPago>1</flagPago>
    </ConsultarDespesas>
  </soap:Body>
</soap:Envelope>', usuario, senha)

# Cabeçalhos HTTP necessários
headers <- c(
  "Content-Type" = "text/xml; charset=utf-8",
  "SOAPAction" = "http://fazenda.sp.gov.br/wstransparencia/ConsultarDespesas"
)

# Fazer a requisição POST
response <- POST(
  url,
  body = soap_body,
  encode = "raw",  # O corpo da mensagem não deve ser reformatado
  add_headers(headers),
  verbose()
)

# Verificar o status da requisição
if (status_code(response) == 200) {
  # Extrair o conteúdo da resposta
  xml_response <- content(response, as = "text", encoding = "UTF-8")
  cat("Resposta do serviço:\n", content)
} else {
  cat("Erro ao acessar o serviço: ", status_code(response), "\n")
}


# Parsear o XML
xml_parsed <- read_xml(xml_response)



# Visualizar a estrutura do XML (ajuda a identificar os nós que contêm os dados)
cat(as.character(xml_parsed),file= "saida.xml")



# Leia o arquivo XML
arquivo_xml <- "saida.xml"

# Carregue o XML
doc <- read_xml(arquivo_xml)

# Defina o namespace utilizado no arquivo
ns <- xml_ns(doc)

# Substitua "d1" pelo namespace real encontrado no arquivo
ns <- c(d1 = "http://fazenda.sp.gov.br/wstransparencia")

# Extraia os nós <ItemDespesa> dentro do namespace
itens_despesa <- xml_find_all(doc, ".//d1:ItemDespesa", ns = ns)

# Extraia os dados relevantes e crie um dataframe
dados <- tibble(
  CodigoNomeOrgao = xml_text(xml_find_first(itens_despesa, ".//d1:CodigoNomeOrgao")),
  CodigoNomeTipoLicitacao = xml_text(xml_find_first(itens_despesa, ".//d1:CodigoNomeTipoLicitacao")),
  NaturezaDespesaNomeItem = xml_text(xml_find_first(itens_despesa, ".//d1:NaturezaDespesaNomeItem")),
  ValorEmpenhado = xml_text(xml_find_first(itens_despesa, ".//d1:ValorEmpenhado")),
  ValorLiquidado = xml_text(xml_find_first(itens_despesa, ".//d1:ValorLiquidado")),
  ValorPago = xml_text(xml_find_first(itens_despesa, ".//d1:ValorPago")),
  ValorPagoAnosAnteriores = xml_text(xml_find_first(itens_despesa, ".d1://ValorPagoAnosAnteriores"))
)

# Limpeza dos valores numéricos (removendo espaços e formatando)
dados <- dados %>%
  mutate(
    ValorEmpenhado = as.numeric(gsub("[^0-9,]", "", ValorEmpenhado)),
    ValorLiquidado = as.numeric(gsub("[^0-9,]", "", ValorLiquidado)),
    ValorPago = as.numeric(gsub("[^0-9,]", "", ValorPago)),
    ValorPagoAnosAnteriores = as.numeric(gsub("[^0-9,]", "", ValorPagoAnosAnteriores))
  )




#####################Tentativas anteriores
# Extrair os dados específicos do XML
# Suponha que os dados estão em elementos <Item>
# Ajuste os nomes dos nós conforme o XML retornado pelo serviço

# data_nodes <- xml_find_all(xml_parsed, ".//ItemDespesa")
# 
# data_nodes<- xml_child(xml_child(xml_child(xml_child(xml_child(xml_parsed, 1), 1), 1), 3), 1)
# 
# # Converter os dados para uma lista ou tibble
# data_list <- data_nodes %>%
#   map(~ as.list(xml_attrs(.)) %>% as_tibble())
# 
# # Combinar todos os itens em um dataframe
# df <- bind_rows(data_list)
# 
# # Converter para data.table (opcional)
# dt <- as.data.table(df)
# 
# # Visualizar o resultado
# print(df)





<ano>2024</ano>
  <codigoOrgao>Consolidado</codigoOrgao>
  <codigoUo>Consolidado</codigoUo>
  <codigoUnidadeGestora>Consolidado</codigoUnidadeGestora>
  <codigoFonteRecursos>Consolidado</codigoFonteRecursos>
  <codigoTipoLicitacao>Todos</codigoTipoLicitacao>
  <codigoFuncao>Consolidado</codigoFuncao>
  <codigoSubfuncao>Consolidado</codigoSubfuncao>
  <codigoPrograma>Consolidado</codigoPrograma>
  <codigoAcao>Consolidado</codigoAcao>
  <codigoFuncionalProgramatica>Consolidado</codigoFuncionalProgramatica>
  <codigoMunicipio>Consolidado</codigoMunicipio>
  <codigoCategoria>Todos</codigoCategoria>
  <codigoGrupo>Todos</codigoGrupo>
  <codigoModalidade>Todos</codigoModalidade>
  <codigoElemento>Todos</codigoElemento>
  <naturezaDespesa>Todos</naturezaDespesa>
  <flagCredor>1</flagCredor>
  <cgcCpfCredor>Todos</cgcCpfCredor>
  <nomeCredor>Todos</nomeCredor>
  <flagEmpenhado>0</flagEmpenhado>
  <flagLiquidado>0</flagLiquidado>
  <flagPago>1</flagPago>
  