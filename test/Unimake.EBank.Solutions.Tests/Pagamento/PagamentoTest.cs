using EBank.Solutions.Primitives.CNAB.CNAB240.Campo;
using EBank.Solutions.Primitives.Enumerations;
using EBank.Solutions.Primitives.Enumerations.CNAB;
using System;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Services.Pagamento;
using Unimake.EBank.Solutions.Services.Pagamento.Request;
using Unimake.EBank.Solutions.Tests.Abstractions;
using Xunit;
using Xunit.Abstractions;
using ContaCorrente = EBank.Solutions.Primitives.Billet.Models.ContaCorrente;
using Favorecido = EBank.Solutions.Primitives.Billet.Models.Favorecido;

namespace Unimake.EBank.Solutions.Tests.Pagamento
{
    public class PagamentoTest(ITestOutputHelper output) : TestBase(output)
    {

        #region Public Constructors

        #endregion Public Constructors

        #region Public Methods

        [Fact]
        public async Task AutorizarPagamento()
        {
            try
            {
                var service = new PagamentoService();
                using var scope = await CreateAuthenticatedScopeAsync();
                var response = await service.AutorizarPagamento(
                [
                    new() {
                        Testing = true,
                        FormaDePagamento = FormaDeLancamento.CreditoEmContaCorrente,
                        NossoNumero = "1234",
                        ValorDoTitulo = 500,
                        ValorDoPagamento = 450,
                        ValorDoDesconto = 50,
                        DataPagamento = DateTime.Today,
                        Beneficiario = BeneficiarioDefault,
                        Favorecido = new Favorecido
                        {
                            TipoInscricao = TipoDeInscricao.CPF,
                            Inscricao = "12345678911",
                            Banco = Banco.Banrisul,
                            Bairro = "Lorem",
                            CEP = "89653-236",
                            Cidade = "Ispum",
                            Conta = new ContaCorrente
                            {
                                Agencia = "122",
                                Banco = Banco.Banrisul,
                                Numero = "11223"
                            }
                        },
                        EnderecoEmpresa = new EnderecoEmpresa
                        {
                            CEP = "96532-365",
                            Cidade = "Dolor",
                            UF = "SP"
                        }
                    }
                ], scope);

                DumpAsJson(response);
            }
            catch(Exception ex)
            {
                DumpAsJson(ex);
                throw;//forward
            }
        }

        [Fact]
        public async Task Get()
        {
            try
            {
                var service = new PagamentoService();
                using var scope = await CreateAuthenticatedScopeAsync();
                var response = await service.GetAsync(new PagamentoRequest
                {
                    Testing = true,
                    Bank = Banco.Itau,
                    StartDate = DateTime.Now.AddDays(-6),
                    EndDate = DateTime.Now,
                    Registration = "76472349000198",
                    AccountNumber = "7137",
                }, scope);

                DumpAsJson(response);
            }
            catch(Exception ex)
            {
                DumpAsJson(ex);
                throw;//forward
            }
        }

        [Fact]
        public async Task ListAsCnabAsync()
        {
            try
            {
                var service = new PagamentoService();
                using var scope = await CreateAuthenticatedScopeAsync();
                var response = await service.ListAsCnabAsync(new PagamentoRequest
                {
                    Testing = true,
                    AccountNumber = "007137",
                    StartDate = DateTime.Parse("04/03/2022"),
                    Bank = Banco.Itau
                }, scope);

                DumpAsJson(response);
            }
            catch(Exception ex)
            {
                DumpAsJson(ex);
                throw;//forward
            }
        }

        [Fact]
        public async Task ListAsJsonAsync()
        {
            try
            {
                var service = new PagamentoService();
                using var scope = await CreateAuthenticatedScopeAsync();
                var response = await service.ListAsJsonAsync(new PagamentoRequest
                {
                    Testing = true,
                    AccountNumber = "007137",
                    StartDate = DateTime.Parse("04/03/2022"),
                    Bank = Banco.Itau
                }, scope);

                DumpAsJson(response);
            }
            catch(Exception ex)
            {
                DumpAsJson(ex);
                throw;//forward
            }
        }

        #endregion Public Methods
    }
}