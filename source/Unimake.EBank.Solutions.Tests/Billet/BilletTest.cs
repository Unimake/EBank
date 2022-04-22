using EBank.Solutions.Primitives.Billet.Models;
using EBank.Solutions.Primitives.Enumerations.Billet;
using EBank.Solutions.Primitives.Exceptions.Response.Billet;
using System;
using System.Threading.Tasks;
using Unimake.AuthServer.Authentication;
using Unimake.AuthServer.Exceptions.Security;
using Unimake.EBank.Solutions.Services.Billet;
using Unimake.EBank.Solutions.Services.Billet.Request;
using Unimake.EBank.Solutions.Tests.Abstract;
using Xunit;
using Xunit.Abstractions;
using AuthenticationService = Unimake.EBank.Solutions.Services.Security.AuthenticationService;

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
        public async Task JustASimpleDebugScopeTest()
        {
            using(new Debug.DebugScope("invalid e-bank uri", "invalid authserver uri"))
            {
                var service = new BilletService();
                var response = await service.RegisterAsync(new RegisterRequest(), null);
                DumpAsJson(response);
            }
        }

        [Fact]
        public async Task Query()
        {
            var request = new QueryRequest
            {
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
            };

            try
            {
                using var scope = await new AuthenticationService().AuthenticateAsync(new AuthenticationRequest
                {
                    // Você consegue realizar os testes de emissão de seus Billets com estas informações.
                    // Mas para que seu Billet seja válido, deverá entrar em contato com a Unimake Software em http://www.unimake.com.br/
                    // Este AppId e Secret foram criados apenas para testes.
                    AppId = "61a73f4735ad4993959e28e2b0e4552a",
                    Secret = "35955532e0c54517bc9d7e900b61b8d3",
                });
                var service = new BilletService();
                var response = await service.QueryAsync(request, scope);
                DumpAsJson(response);
            }
            catch(QueryInformationResponseException registerEx)
            {
                DumpAsJson(registerEx);
                throw;//forward
            }
        }

        [Fact]
        public async Task Register()
        {
            // Billet mínimo para gravação
            // CPF e CNPJ foram gerados no site
            // https://www.4devs.com.br

            var request = new RegisterRequest
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
                        Rua = "Rua Fictícia",
                        Numero = "11",
                        Bairro = "Bairro",
                        Cep = "11111111",
                        Cidade = "Brasília",
                        UF = "DF",
                    },
                },
            };

            try
            {
                using var scope = await new AuthenticationService().AuthenticateAsync(new AuthenticationRequest
                {
                    // Você consegue realizar os testes de emissão de seus Billets com estas informações.
                    // Mas para que seu Billet seja válido, deverá entrar em contato com a Unimake Software em http://www.unimake.com.br/
                    // Este AppId e Secret foram criados apenas para testes.
                    AppId = "61a73f4735ad4993959e28e2b0e4552a",
                    Secret = "35955532e0c54517bc9d7e900b61b8d3",
                });
                var service = new BilletService();
                var response = await service.RegisterAsync(request, scope);
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
            using var scope = await new AuthenticationService().AuthenticateAsync(new AuthenticationRequest
            {
                // Você consegue realizar os testes de emissão de seus Billets com estas informações.
                // Mas para que seu Billet seja válido, deverá entrar em contato com a Unimake Software em http://www.unimake.com.br/
                // Este AppId e Secret foram criados apenas para testes.
                AppId = "invalid appId",
                Secret = "invalid secret",
            });
            var service = new BilletService();
            var response = await service.RegisterAsync(new RegisterRequest(), scope);
            DumpAsJson(response);
        });

        #endregion Public Methods
    }
}