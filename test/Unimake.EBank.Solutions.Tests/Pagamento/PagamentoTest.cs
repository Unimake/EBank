using EBank.Solutions.Primitives.Pagamento.Model;
using EBank.Solutions.Primitives.Pagamento.Request;
using System;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Services.Pagamento;
using Unimake.EBank.Solutions.Tests.Abstractions;
using Xunit;

namespace Unimake.EBank.Solutions.Tests.Pagamento
{
    [Trait("Category", "Publish")]
    public class PagamentoTest(ITestOutputHelper output) : TestBase(output)
    {
        #region Protected Methods

        protected override T CreateRequest<T>(Func<T> builder)
        {
            var request = base.CreateRequest(builder);

            if(request is PagamentoRequest pagamentoRequest)
            {
                pagamentoRequest.Pagador = new Pagador
                {
                    Inscricao = "06117473000079",
                    Nome = "Unifake Software"
                };
            }

            return request;
        }

        #endregion Protected Methods

        #region Public Methods

        [Fact]
        public async Task AutorizarBoletosAsync()
        {
            try
            {
                using var scope = await CreateAuthenticatedScopeAsync();
                var service = new PagamentoService();
                var response = await service.AutorizarPagamentoAsync(CreateRequest(() => new PagamentoRequest
                {
                    Boletos =
                    [
                        new LancamentoBoletoRequest
                        {
                            ValorPagamento = 100.00m,
                            CodigoBarras = "00190500954014481606906809350314337370000000100",
                            IdentificadorPagamento = "202309141234567890",
                            DataVencimento = DateTime.Now.AddDays(15)
                        }
                    ]
                }), scope);

                DumpAsJson(response);
            }
            catch(Exception ex)
            {
                DumpAsJson(ex);
                throw;//forward
            }
        }

        [Fact]
        public async Task CancelarBoletosAsync()
        {
            try
            {
                using var scope = await CreateAuthenticatedScopeAsync();
                var service = new PagamentoService();
                var response = await service.CancelarPagamentoAsync(CreateRequest(() => new MultiplosPagamentosRequest
                {
                    Pagamentos = [
                        new IdentificadorPagamentoItem
                        {
                            IdentificadorTransacao = "202309141234567890",
                            Tipo = global::EBank.Solutions.Primitives.Enumerations.SisPag.TipoAutorizacaoPagamento.Boleto,
                        }
                    ]
                }), scope);

                DumpAsJson(response);
            }
            catch(Exception ex)
            {
                DumpAsJson(ex);
                throw;//forward
            }
        }

        [Fact]
        public async Task ConsultarBoletosAsync()
        {
            try
            {
                using var scope = await CreateAuthenticatedScopeAsync();
                var service = new PagamentoService();
                var response = await service.ConsultarPagamentosAsync(CreateRequest(() => new MultiplosPagamentosRequest
                {
                    Pagamentos = [
                        new IdentificadorPagamentoItem
                        {
                            IdentificadorTransacao = "202309141234567890",
                            Tipo = global::EBank.Solutions.Primitives.Enumerations.SisPag.TipoAutorizacaoPagamento.Boleto,
                        }
                    ]
                }), scope);

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