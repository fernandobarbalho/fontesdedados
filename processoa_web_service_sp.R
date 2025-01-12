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
      <ano>2024</ano>
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
cat(as.character(xml_parsed))

# Extrair os dados específicos do XML
# Suponha que os dados estão em elementos <Item>
# Ajuste os nomes dos nós conforme o XML retornado pelo serviço
data_nodes <- xml_find_all(xml_parsed, ".//ItemDespesa")

data_nodes<- xml_child(xml_child(xml_child(xml_child(xml_child(xml_parsed, 1), 1), 1), 3), 1)

# Converter os dados para uma lista ou tibble
data_list <- data_nodes %>%
  map(~ as.list(xml_attrs(.)) %>% as_tibble())

# Combinar todos os itens em um dataframe
df <- bind_rows(data_list)

# Converter para data.table (opcional)
dt <- as.data.table(df)

# Visualizar o resultado
print(df)


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
  