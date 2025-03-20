using EBank.Solutions.Primitives.Billet.Request;
using EBank.Solutions.Primitives.Exceptions.Response.Billet;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Services.Billet;
using Unimake.EBank.Solutions.Tests.Abstractions;
using Xunit;
using Xunit.Abstractions;

namespace Unimake.EBank.Solutions.Tests.BugFixes
{
    public class BugFix164606(ITestOutputHelper output) : TestBase(output)
    {
        #region Public Methods

        [Fact]
        [Trait("Issue", "#164606")]
        [Trait("Error", "Unexpected character encountered while parsing value: {. Path '[0].PdfContent', line 1, position 340")]
        public async Task Fix()
        {
            var request = CreateRequest(() => new QueryInformationRequest
            {
                DataEmissaoInicial = StartDate,
                DataEmissaoFinal = EndDate
            });

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

        #endregion Public Methods
    }
}