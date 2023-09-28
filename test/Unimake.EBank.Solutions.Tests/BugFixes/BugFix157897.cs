using EBank.Solutions.Primitives.PIX.Request.Cobranca;
using EBank.Solutions.Primitives.PIX.Request.Consulta;
using System;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Services.PIX;
using Unimake.EBank.Solutions.Tests.Abstractions;
using Xunit;
using Xunit.Abstractions;

namespace Unimake.EBank.Solutions.Tests.BugFixes
{
    public class BugFix157897 : TestBase
    {
        #region Public Constructors

        public BugFix157897(ITestOutputHelper output)
            : base(output)
        {
        }

        #endregion Public Constructors

        #region Public Methods

        [Fact]
        [Trait("Description", "Unexpected character encountered while parsing value: &lt;. Path '', line 0, position 0.")]
        public async Task FixUnexpectedCharacterEncountered_CobrancaGetRequest()
        {
            using var scope = await CreateAuthenticatedScopeAsync();
            var service = new PIXService();
            var response = await service.QueryCobAsync(new PIXCobrancaGetRequest
            {
                Beneficiario = BeneficiarioDefault,
                Testing = true,
                TxId = "6b344afaef3b40d5b345274c92358f89"
            }, scope);

            DumpAsJson(response);
        }

        [Fact]
        [Trait("Description", "Unexpected character encountered while parsing value: &lt;. Path '', line 0, position 0.")]
        public async Task FixUnexpectedCharacterEncountered_GetRequest()
        {
            using var scope = await CreateAuthenticatedScopeAsync();
            var service = new PIXService();
            var response = await service.GetAsync(new PIXGetRequest
            {
                Beneficiario = BeneficiarioDefault,
                Testing = true,
                TxId = "c321df8318e04ab98dc347054be684c9",
                StartDate = DateTime.Parse("2023-09-26"),
                EndDate = DateTime.Parse("2023-09-26")
            }, scope);

            DumpAsJson(response);
        }

        [Fact]
        [Trait("Description", "Unexpected character encountered while parsing value: &lt;. Path '', line 0, position 0.")]
        public async Task FixUnexpectedCharacterEncountered_GetRequestNoTxId()
        {
            using var scope = await CreateAuthenticatedScopeAsync();
            var service = new PIXService();
            var response = await service.GetAsync(new PIXGetRequest
            {
                Beneficiario = BeneficiarioDefault,
                Testing = true,
                StartDate = DateTime.Parse("2023-09-22"),
                EndDate = DateTime.Parse("2023-09-26")
            }, scope);

            DumpAsJson(response);
        }

        #endregion Public Methods
    }
}