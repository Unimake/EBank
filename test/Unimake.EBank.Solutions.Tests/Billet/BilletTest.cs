using EBank.Solutions.Primitives.Billet.Models;
using EBank.Solutions.Primitives.Enumerations;
using EBank.Solutions.Primitives.Enumerations.Billet;
using EBank.Solutions.Primitives.Exceptions.Response.Billet;
using Newtonsoft.Json;
using System;
using System.Threading.Tasks;
using Unimake.AuthServer.Exceptions;
using Unimake.AuthServer.Security.Scope;
using Unimake.EBank.Solutions.Services.Billet;
using Unimake.EBank.Solutions.Services.Billet.Request;
using Unimake.EBank.Solutions.Services.Billet.Response;
using Unimake.EBank.Solutions.Tests.Abstractions;
using Unimake.Primitives.Security.Credentials;
using Unimake.Primitives.UDebug;
using Xunit;
using Xunit.Abstractions;

namespace Unimake.EBank.Solutions.Tests.Billet
{
    public class BilletTest : TestBase
    {
        #region Public Constructors

        public BilletTest(ITestOutputHelper output)
            : base(output)
        {
        }

        #endregion Public Constructors

        #region Public Methods

        [Fact]
        public async Task InformarPagamentoAsync()
        {
            using var scope = await CreateAuthenticatedScopeAsync();
            var billetService = new BilletService();
            var response = await billetService.InformPaymentAsync(new InformPaymentRequest
            {
                Beneficiario = new Beneficiario
                {
                    Codigo = "1234",
                    Nome = "Unimake Software",
                    Inscricao = "<?>>",
                    Conta = new ContaCorrente
                    {
                        Banco = Banco.Itau,
                        Agencia = "0246",
                        Numero = "0246"
                    }
                },
                NumeroNoBanco = "00000033",
                Testing = true
            }, scope);
        }

        [Fact]
        public async Task InvalidRegister()
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
                    Conta = new ContaCorrente()
                },
                Pagador = new Pagador
                {
                    Nome = "Marcelo de Souza",
                    Email = "pagador@exemplo.com.br",
                    TipoInscricao = TipoDeInscricao.CPF,
                    Inscricao = "38640211035",
                    Endereco = new Endereco
                    {
                        Logradouro = "Rua Fictícia",
                        Numero = "11",
                        Bairro = "Bairro",
                        CEP = "11111111",
                        Cidade = "Brasília",
                        UF = "DF",
                    },
                },
            };

            try
            {
                using var scope = await CreateAuthenticatedScopeAsync();
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
        public async Task JustASimpleDebugScopeTest()
        {
            using(new DebugScope<DebugStateObject>(new DebugStateObject
            {
                AuthServerUrl = "invalid e-bank uri",
                AnotherServerUrl = "invalid authserver uri"
            }))
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
                NumeroNoBanco = "222145568",
                Beneficiario = new Beneficiario
                {
                    Nome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA",
                    Codigo = "94914",
                    Inscricao = "<?>>",
                    Conta = new ContaCorrente
                    {
                        Banco = Banco.Sicredi,
                        Agencia = "0718",
                        Numero = "94914"
                    }
                }
            };

            try
            {
                using var scope = await CreateAuthenticatedScopeAsync();
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
        [Trait("Issue", "#162636")]
        public async Task QueryByConfigurationId()
        {
            var request = new QueryRequest
            {
                Beneficiario = new Beneficiario
                {
                    Nome = "<?>>",
                    Codigo = "<?>>",
                    Inscricao = "<?>>",
                    Conta = new ContaCorrente
                    {
                        Banco = Banco.Sicoob,
                        Agencia = "<?>>",
                        Numero = "<?>>"
                    }
                },
                DataEmissaoInicial = DateTime.Parse("2023-06-30"),
                DataEmissaoFinal = DateTime.Parse("2023-07-05"),
                Testing = false,
                //ConfigurationId = "F8A7D69F6E5E402"
            };

            try
            {
                using var scope = await CreateAuthenticatedScopeAsync();
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
                    Inscricao = "<?>>",
                    Conta = new ContaCorrente
                    {
                        Banco = Banco.Itau,
                        Agencia = "0246",
                        Numero = "0246"
                    }
                },
                Pagador = new Pagador
                {
                    Nome = "Marcelo de Souza",
                    Email = "pagador@exemplo.com.br",
                    TipoInscricao = TipoDeInscricao.CPF,
                    Inscricao = "38640211035",
                    Endereco = new Endereco
                    {
                        Logradouro = "Rua Fictícia",
                        Numero = "11",
                        Bairro = "Bairro",
                        CEP = "11111111",
                        Cidade = "Brasília",
                        UF = "DF",
                    },
                },
            };

            try
            {
                using var scope = await CreateAuthenticatedScopeAsync();
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
            using var scope = new AuthenticatedScope(new AuthenticationToken
            {
                AppId = "invalid appId",
                Secret = "invalid secret",
            });

            var service = new BilletService();
            var response = await service.RegisterAsync(new RegisterRequest(), scope);
            DumpAsJson(response);
        });

        [Fact]
        public void ResultFromJson()
        {
            var json = "{\"CodigoBarraNumerico\":null,\"LinhaDigitavel\":\"03399617328610000000805641701015393460000057100\",\"NumeroNoBanco\":\"0000000056417\",\"PDFContent\":{\"Content\":\"\",\"Message\":\"Santander não retorna o PDF do boleto.\",\"Success\":false}}";
            var response = JsonConvert.DeserializeObject<RegisterResponse>(json);
            Assert.NotNull(response);
        }

        [Fact]
        public void WrongKey()
        {
            Assert.Throws<AuthenticationServiceException>(() =>
            {
                var x = new AuthenticationToken
                {
                    AppId = "<?>>",
                    Secret = "<?>>"
                };
                _ = new AuthenticatedScope(x);
            });
        }

        #endregion Public Methods
    }
}