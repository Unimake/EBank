Dim service:		Set service = CreateObject("Unimake.EBank.Solutions.Services.Billet.BilletService")
Dim request:		Set request = CreateObject("Unimake.EBank.Solutions.Services.Billet.Request.RegisterRequest")
Dim Beneficiario:	Set Beneficiario = CreateObject("EBank.Solutions.Primitives.Billet.Models.Beneficiario")
Dim ContaCorrente:	Set ContaCorrente = CreateObject("EBank.Solutions.Primitives.Billet.Models.ContaCorrente")
Dim Pagador:		Set Pagador = CreateObject("EBank.Solutions.Primitives.Billet.Models.Pagador")
Dim Endereco:		Set Endereco = CreateObject("EBank.Solutions.Primitives.Billet.Models.Endereco")
Dim AuthService:	Set AuthService = CreateObject("Unimake.EBank.Solutions.Services.Security.AuthenticationService")
Dim AppId:				AppId = "61a73f4735ad4993959e28e2b0e4552a"
Dim Secret:				Secret =  "35955532e0c54517bc9d7e900b61b8d3"
Dim AuthScope      
Dim response

' Billet mínimo para gravação
' CPF e CNPJ foram gerados no site
' https://www.4devs.com.br

request.Especie = 99
request.ValorNominal = 45.88
request.Vencimento = DateAdd("d",10, Now)
request.NumeroNaEmpresa = "12345"
request.NumeroNoBanco = "12345"

' Dados Beneficiário
Beneficiario.Codigo = "1234"
Beneficiario.Nome = "Unimake Software"
Beneficiario.Inscricao = "71444314000121"

' Dados de conta corrente
ContaCorrente.Banco = 341
ContaCorrente.Agencia = "0246"
ContaCorrente.Numero = "0246"

Set Beneficiario.Conta = ContaCorrente
set request.Beneficiario = Beneficiario

'dados do pagador
Pagador.Nome = "Marcelo de Souza"
Pagador.Email = "pagador@exemplo.com.br"
Pagador.TipoInscricao = 1
Pagador.Inscricao = "38640211035"

'dados de endereço do pagador
Endereco.Rua = "Rua Fictícia"
Endereco.Numero = "11"
Endereco.Bairro = "Bairro"
Endereco.CEP = "11111111"
Endereco.Cidade = "Brasília"
Endereco.UF = "DF"

Set Pagador.Endereco = Endereco
Set request.Pagador = Pagador

'Autenticar
Set AuthScope = AuthService.Authenticate (AppId, Secret)

'Pegar retorno
Set response = service.Register ((request), (AuthScope))
MsgBox response.LinhaDigitavel