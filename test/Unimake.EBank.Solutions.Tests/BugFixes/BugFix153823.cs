using EBank.Solutions.Primitives.PIX.Request.Consulta;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Services.PIX;
using Unimake.EBank.Solutions.Tests.Abstractions;
using Xunit;
using Xunit.Abstractions;

namespace Unimake.EBank.Solutions.Tests.BugFixes
{
    public class BugFix153823(ITestOutputHelper output) : TestBase(output)
    {
        #region Public Methods

        [Trait("Bug", "153823")]
        [Trait("Error", "HeaderDeArquivo deve ter 240 caracteres")]
        [Fact]
        public async Task UnimakeEBankSolutionsExceptionsResponseException()
        {
            using var authScope = await CreateAuthenticatedScopeAsync();

            var pixService = new PIXService();

            //No response da consulta PIX tem as propriedades de paginação, mas sempre será 1:1
            var response = await pixService.GetAsync(CreateRequest(() => new PIXGetRequest
            {
                StartDate = StartDate,
                EndDate = EndDate,
            }), authScope);

            DumpAsJson(response);
        }

        #endregion Public Methods
    }
}