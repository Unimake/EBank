Imports EBank.Solutions.Primitives.Billet.Models
Imports EBank.Solutions.Primitives.Enumerations
Imports EBank.Solutions.Primitives.Enumerations.Billet
Imports EBank.Solutions.Primitives.Exceptions.Response.Billet
Imports Unimake.AuthServer.Authentication
Imports Unimake.EBank.Solutions.Services.Billet
Imports Unimake.EBank.Solutions.Services.Billet.Request

Friend Module Program

    Public Sub DumpAsJson(value As Object)
        Console.WriteLine(Newtonsoft.Json.JsonConvert.SerializeObject(value, Newtonsoft.Json.Formatting.Indented))
    End Sub

    Public Sub Main()
        Console.Title = "Registrar boleto"
        MessageCenter.Log("Iniciando registros de boletos")
        Dim billetService = New BilletService()

        ' Boleto mínimo para gravação
        ' CPF e CNPJ foram gerados no site https://www.4devs.com.br

        Dim request = New RegisterRequest With
        {
            .Testing = True,
            .Especie = EspecieTitulo.Outros,
            .ValorNominal = 45.88D,
            .Vencimento = Date.Today.AddDays(15),
            .NumeroNaEmpresa = "12345",
            .NumeroNoBanco = "12345",
            .Beneficiario = New Beneficiario With
            {
                .Codigo = "1234",
                .Nome = "Unimake Software",
                .Inscricao = "71444314000121",
                .Conta = New ContaCorrente With
                {
                    .Banco = Banco.Itau,
                    .Agencia = "0246",
                    .Numero = "0246"
                }
            },
            .Pagador = New Pagador With
            {
                .Nome = "Marcelo de Souza",
                .Email = "pagador@exemplo.com.br",
                .TipoInscricao = TipoInscricao.CPF,
                .Inscricao = "38640211035",
                .Endereco = New Endereco With
                {
                    .Rua = "Rua Fictícia",
                    .Numero = "11",
                    .Bairro = "Bairro",
                    .CEP = "11111111",
                    .Cidade = "Brasília",
                    .UF = "DF"
                }
            }
        }

        Try
            ' Para utilização do e-bank, é necessário utilizar-se de um escopo autenticado.
            ' Logo, iremos criar o nosso escopo
            ' Talvez você não consiga realizar os testes de emissão de seus boletos com estas informações.
            ' Para que seu boleto seja válido, deverá entrar em contato com a Unimake Software em http://www.unimake.com.br/ e obter suas chaves.
            ' Este AppId e Secret foram criados apenas para testes.
            Dim scope = New Services.Security.AuthenticationService().AuthenticateAsync(New AuthenticationRequest With
            {
                .AppId = "61a73f4735ad4993959e28e2b0e4552a",
                .Secret = "35955532e0c54517bc9d7e900b61b8d3"
            }).GetAwaiter().GetResult()
            Dim service = New BilletService()
            Dim response = service.RegisterAsync(request, scope).GetAwaiter().GetResult()
            DumpAsJson(response)
        Catch registerEx As RegisterResponseException
            DumpAsJson(registerEx.Errors)
            MessageCenter.Error(registerEx)
        End Try

        MessageCenter.DrawLine()
        MessageCenter.Alert("Registro concluído.")
        Dim unused = Console.ReadLine()

    End Sub

End Module