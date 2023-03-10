using EBank.Solutions.Primitives.PIX.Request.Cobranca;
using System;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Services.PIX;
using Unimake.EBank.Solutions.Tests.Abstractions;
using Xunit;
using Xunit.Abstractions;

namespace Unimake.EBank.Solutions.Tests.PIX
{
    public class PIXCriarCobrancaTest : TestBase
    {
        #region Private Fields

        private const string TX_ID = "CobrancaDeTestePix20230213183421";

        #endregion Private Fields

        #region Public Constructors

        public PIXCriarCobrancaTest(ITestOutputHelper output)
                    : base(output) => StartServerDebugMode();

        #endregion Public Constructors

        #region Public Methods

        [Theory]
        [InlineData(TX_ID)]
        public async Task ConsultarCobrancaComTxId(string txId)
        {
            try
            {
                var beneficiario = BeneficiarioDefault;
                using var scope = await CreateAuthenticatedScopeAsync();
                var service = new PIXService();
                var cobResponse = await service.QueryCobAsync(new PIXCobrancaGetRequest
                {
                    Banco = beneficiario.Conta.Banco.GetValueOrDefault(),
                    Inscricao = beneficiario.Inscricao,
                    NumeroAgencia = beneficiario.Conta.Agencia,
                    NumeroConta = beneficiario.Conta.Numero,
                    TxId = txId
                }, scope);

                //Assert.Equal(response.Txid, cobResponse.Txid);
                Assert.Equal(txId, cobResponse.Txid);
                DumpAsJson(cobResponse);
            }
            catch(Exception ex)
            {
                DumpAsJson(ex);
                throw;//forward
            }
        }

        [Theory]
        [InlineData(TX_ID)]
        public async Task ConsultarCobrancaERetornarQrCode(string txId)
        {
            try
            {
                var beneficiario = BeneficiarioDefault;
                using var scope = await CreateAuthenticatedScopeAsync();
                var service = new PIXService();
                var cobResponse = await service.QueryCobAsync(new PIXCobrancaGetRequest
                {
                    Banco = beneficiario.Conta.Banco.GetValueOrDefault(),
                    Inscricao = beneficiario.Inscricao,
                    NumeroAgencia = beneficiario.Conta.Agencia,
                    NumeroConta = beneficiario.Conta.Numero,
                    TxId = txId,
                    GerarQRCode = true
                }, scope);

                //Assert.Equal(response.Txid, cobResponse.Txid);
                Assert.Equal(txId, cobResponse.Txid);
                DumpAsJson(cobResponse);
            }
            catch(Exception ex)
            {
                DumpAsJson(ex);
                throw;//forward
            }
        }

        [Theory]
        [InlineData(TX_ID)]
        public async Task GerarCobrancaComQrCode(string txId)
        {
            try
            {
                var beneficiario = BeneficiarioDefault;
                using var scope = await CreateAuthenticatedScopeAsync();
                var service = new PIXService();
                var response = await service.CreateCobAsync(new PIXCobrancaCreateRequest
                {
                    Beneficiario = beneficiario,
                    Valor = 1.17852m,
                    Chave = "<<CHAVE PIX VÁLIDA PARA QUAL O VALOR SERÁ REPASSADO>>",
                    TxId = txId,
                    GerarQRCode = true
                }, scope);

                DumpAsJson(response);
            }
            catch(Exception ex)
            {
                DumpAsJson(ex);
                throw;//forward
            }
        }

        [Theory]
        [InlineData(TX_ID)]
        public async Task GerarCobrancaComTxId(string txId)
        {
            try
            {
                var beneficiario = BeneficiarioDefault;

                using var scope = await CreateAuthenticatedScopeAsync();
                var service = new PIXService();
                var response = await service.CreateCobAsync(new PIXCobrancaCreateRequest
                {
                    Beneficiario = beneficiario,
                    Valor = 1.17852m,
                    Chave = "<<CHAVE PIX VÁLIDA PARA QUAL O VALOR SERÁ REPASSADO>>",
                    TxId = txId
                }, scope);

                DumpAsJson(response);

                //Já realizar a consulta e garantir que foi gerada corretamente
                //Já fica como teste :)
                var cobResponse = await service.QueryCobAsync(new PIXCobrancaGetRequest
                {
                    Banco = beneficiario.Conta.Banco.GetValueOrDefault(),
                    Inscricao = beneficiario.Inscricao,
                    NumeroAgencia = beneficiario.Conta.Agencia,
                    NumeroConta = beneficiario.Conta.Numero,
                    TxId = txId
                }, scope);

                //Assert.Equal(response.Txid, cobResponse.Txid);
                Assert.Equal(txId, cobResponse.Txid);

                DumpAsJson(cobResponse);
            }
            catch(Exception ex)
            {
                DumpAsJson(ex);
                throw;//forward
            }
        }

        [Fact]
        public async Task GerarCobrancaComTxIdDefindoPeloPSP()
        {
            try
            {
                var beneficiario = BeneficiarioDefault;

                using var scope = await CreateAuthenticatedScopeAsync();
                var service = new PIXService();
                var response = await service.CreateCobAsync(new PIXCobrancaCreateRequest
                {
                    Beneficiario = beneficiario,
                    Valor = 1.17852m,
                    Chave = "<<CHAVE PIX VÁLIDA PARA QUAL O VALOR SERÁ REPASSADO>>"
                }, scope);

                DumpAsJson(response);

                var txId = response.Txid;

                //Já realizar a consulta e garantir que foi gerada corretamente
                //Já fica como teste :)
                var cobResponse = await service.QueryCobAsync(new PIXCobrancaGetRequest
                {
                    Banco = beneficiario.Conta.Banco.GetValueOrDefault(),
                    Inscricao = beneficiario.Inscricao,
                    NumeroAgencia = beneficiario.Conta.Agencia,
                    NumeroConta = beneficiario.Conta.Numero,
                    TxId = txId
                }, scope);

                //Assert.Equal(response.Txid, cobResponse.Txid);
                Assert.Equal(txId, cobResponse.Txid);

                DumpAsJson(cobResponse);
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