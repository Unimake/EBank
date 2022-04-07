using EBank.Solutions.Primitives.Billet.Models;
using EBank.Solutions.Primitives.Enumerations.Billet;
using EBank.Solutions.Primitives.Exceptions.Response.Billet;
using System;
using System.Threading.Tasks;
using Unimake.AuthServer.Exceptions.Security;
using Unimake.EBank.Solutions.Services.Billet;
using Unimake.EBank.Solutions.Tests.Abstract;
using Xunit;
using Xunit.Abstractions;

namespace Unimake.EBank.Solutions.Tests.Billet
{
    public class BilletTest : TestBase
    {
        #region Public Constructors

        public BilletTest(ITestOutputHelper output) : base(output)
        {
        }

        #endregion Public Constructors

        #region Public Methods

        [Fact]
        public async Task Register()
        {
            // Billet m�nimo para grava��o
            // CPF e CNPJ foram gerados no site
            // https://www.4devs.com.br

            var request = new RegisterRequest
            {
                // Voc� consegue realizar os testes de emiss�o de seus Billets com estas informa��es.
                // Mas para que seu Billet seja v�lido, dever� entrar em contato com a Unimake Software em http://www.unimake.com.br/
                // Este AppId e Secret foram criados apenas para testes.
                AppId = "61a73f4735ad4993959e28e2b0e4552a",
                Secret = "35955532e0c54517bc9d7e900b61b8d3",
                Billet = new Boleto
                {
                    Especie = EspecieTitulo.Outros,
                    ValorNominal = 45.88m,
                    Vencimento = DateTime.Today.AddDays(15),
                    NumeroNaEmpresa = "12345",
                    NumeroNoBanco = "12345",
                    Beneficiario = new Beneficiario
                    {
                        Codigo = "1234",
                        Nome = "Unimake Software",
                        Inscricao = "71444314000121",
                        Conta = new ContaCorrente
                        {
                            Banco = global::EBank.Solutions.Primitives.Enumerations.Banco.Itau,
                            Agencia = "0246",
                            Numero = "0246"
                        }
                    },
                    Pagador = new Pagador
                    {
                        Nome = "Marcelo de Souza",
                        Email = "pagador@exemplo.com.br",
                        TipoInscricao = TipoInscricao.CPF,
                        Inscricao = "38640211035",
                        Endereco = new Endereco
                        {
                            Rua = "Rua Fict�cia",
                            Numero = "11",
                            Bairro = "Bairro",
                            Cep = "11111111",
                            Cidade = "Bras�lia",
                            UF = "DF",
                        },
                    },
                }
            };

            try
            {
                var service = new BilletService();
                var response = await service.RegisterAsync(request);
                DumpAsJson(response);
            }
            catch(RegisterResponseException registerEx)
            {
                DumpAsJson(registerEx.Errors);
                throw;//forward
            }
        }

        [Fact]
        public async Task RegisterInvalidAppIdOrSecret() =>
            await Assert.ThrowsAsync<AuthenticationServiceException>(async () =>
        {
            // Billet m�nimo para grava��o
            // CPF e CNPJ foram gerados no site
            // https://www.4devs.com.br

            var request = new RegisterRequest
            {
                // Voc� consegue realizar os testes de emiss�o de seus Billets com estas informa��es.
                // Mas para que seu Billet seja v�lido, dever� entrar em contato com a Unimake Software em http://www.unimake.com.br/
                // Este AppId e Secret foram criados apenas para testes.
                AppId = "Invalid AppId",
                Secret = "Invalid Secret",
                Billet = new Boleto()
            };

            var service = new BilletService();
            _ = await service.RegisterAsync(request);
        });

        #endregion Public Methods
    }
}