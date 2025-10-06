Imports System.IO
Imports EBank.Solutions.Primitives.Billet
Imports EBank.Solutions.Primitives.Billet.Models
Imports EBank.Solutions.Primitives.Enumerations
Imports EBank.Solutions.Primitives.Enumerations.Billet
Imports EBank.Solutions.Primitives.Exceptions.Response.Billet
Imports EBank.Solutions.Primitives.PDF.Models
Imports Unimake.AuthServer.Security.Scope
Imports Unimake.EBank.Solutions.Services.Billet
Imports Unimake.EBank.Solutions.Services.Billet.Request
Imports Unimake.Primitives.Security.Credentials

Friend Module Program

    Public Sub DumpAsJson(value As Object)
        Console.WriteLine(Newtonsoft.Json.JsonConvert.SerializeObject(value, Newtonsoft.Json.Formatting.Indented))
    End Sub

    Public Sub Main()
        Console.Title = "Registrar boleto"
        MessageCenter.Log("Iniciando registros de boletos")
        Dim billetService = New BilletService()
        Dim exitCode = 1

        ' Boleto m�nimo para grava��o
        ' CPF e CNPJ foram gerados no site https://www.4devs.com.br

        ' Este configuration id, informado aqui, � o
        ' identificador da conta corrente que ser� utilizada na emiss�o do boleto

        ' Com o configurationId, a propriedade Benefici�rio n�o precisa mais ser informada
        '.Beneficiario = New Beneficiario With
        '{
        '    .Codigo = "1234",
        '    .Nome = "Unimake Software",
        '    .Inscricao = "71444314000121",
        '    .Conta = New ContaCorrente With
        '    {
        '        .Banco = Banco.Itau,
        '        .Agencia = "0246",
        '        .Numero = "0246"
        '    }
        '},

        Dim request = New RegisterRequest With
        {
            .ConfigurationId = "ZCKWGQ55LTDXKYYC",
            .Testing = True,
            .Especie = EspecieTitulo.Outros,
            .ValorNominal = 45.88D,
            .Vencimento = Date.Today.AddDays(15),
            .NumeroNaEmpresa = "12345",
            .NumeroNoBanco = "12345",
            .Pagador = New Pagador With
            {
                .Nome = "Marcelo de Souza",
                .Email = "pagador@exemplo.com.br",
                .TipoInscricao = TipoDeInscricao.CPF,
                .Inscricao = "38640211035",
                .Endereco = New Endereco With
                {
                    .Logradouro = "Rua Fict�cia",
                    .Numero = "11",
                    .Bairro = "Bairro",
                    .CEP = "11111111",
                    .Cidade = "Bras�lia",
                    .UF = "DF"
                }
            }
        }

        ' Aqui, vamos solicitar o PDF
        request.PDFConfig = New PDFConfig With
        {
            .TryGeneratePDF = True
        }

        ' E se permitir boleto h�brido, vamos solicitar o bolet h�brido
        request.PIXConfig = New PIXBilletConfig With
        {
            .RegistrarPIX = True,
            .Chave = "chave@example.com"
        }

        Try
            ' Para utiliza��o do e-bank, � necess�rio utilizar-se de um escopo autenticado.
            ' Logo, iremos criar o nosso escopo
            ' Talvez voc� n�o consiga realizar os testes de emiss�o de seus boletos com estas informa��es.
            ' Para que seu boleto seja v�lido, dever� entrar em contato com a Unimake Software em http://www.unimake.com.br/ e obter suas chaves.
            ' Este AppId e Secret foram criados apenas para testes.
            Dim scope = New AuthenticatedScope(New AuthenticationToken With
            {
                .AppId = "e7be62b0e70a4488aa2b62ddc2a9738a",
                .Secret = "e3f50cb68e8a49f48dbf2a26b3db0383"
            })

            ' Criamos o servi�o para emiss�o de boletos
            Dim service = New BilletService()

            ' Enviamos a solicita��o de registro
            Dim response = service.RegisterAsync(request, scope).GetAwaiter().GetResult()

            ' Como n�o deu nenhum erro, tratados abaixo, o boleto foi registrado.

            MessageCenter.DrawLine()
            Dim messages = New List(Of String) From {
                "N�mero no Banco  : " + response.NumeroNoBanco,
                "Codigo de Barras : " + response.CodigoBarraNumerico,
                "Linha Digit�vel  : " + response.LinhaDigitavel
            }

            MessageCenter.Message(messages)

            ' Vamos exibir o pdf. no visualizador padr�o
            ' O conte�do do .pdf � base64

            File.WriteAllBytes("ebank-boleto.pdf", Convert.FromBase64String(response.PDFContent.Content))
            Process.Start(New ProcessStartInfo With
                         {
                             .FileName = "ebank-boleto.pdf",
                             .UseShellExecute = True
                         })

            exitCode = 0
        Catch registerEx As RegisterResponseException
            DumpAsJson(registerEx)
            MessageCenter.Error(registerEx)
        Catch ex As Exception
            DumpAsJson(ex)
            MessageCenter.Error(ex)
        End Try

        MessageCenter.DrawLine()
        MessageCenter.Alert("Registro conclu�do.")
        Dim unused = Console.ReadLine()
        Environment.Exit(exitCode)

    End Sub

End Module