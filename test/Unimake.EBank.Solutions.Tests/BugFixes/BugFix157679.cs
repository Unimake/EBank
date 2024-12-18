using EBank.Solutions.Primitives.PIX.Request.Cobranca;
using System;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Services.PIX;
using Unimake.EBank.Solutions.Tests.Abstractions;
using Xunit;
using Xunit.Abstractions;

namespace Unimake.EBank.Solutions.Tests.BugFixes
{
    public class BugFix157679(ITestOutputHelper output) : TestBase(output)
    {

        #region Public Constructors

        #endregion Public Constructors

        #region Public Methods

        [Fact]
        public async Task FixSoftwareNaocadastradoExcpetionAsync()
        {
            try
            {
                using var scope = await CreateAuthenticatedScopeAsync();
                var service = new PIXService();
                var response = await service.CreateCobAsync(new PIXCobrancaCreateRequest
                {
                    Testing = true,
                    Beneficiario = BeneficiarioDefault,
                    Valor = 1.17852m,
                    Chave = "<<CHAVE>>"
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