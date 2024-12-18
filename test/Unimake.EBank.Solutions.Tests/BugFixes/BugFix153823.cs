using EBank.Solutions.Primitives.PIX.Request.Consulta;
using System;
using System.Threading.Tasks;
using Unimake.AuthServer.Security.Scope;
using Unimake.EBank.Solutions.Services.PIX;
using Unimake.EBank.Solutions.Tests.Abstractions;
using Unimake.Primitives.Security.Credentials;
using Xunit;
using Xunit.Abstractions;

namespace Unimake.EBank.Solutions.Tests.BugFixes
{
    public class BugFix153823(ITestOutputHelper output) : TestBase(output)
    {

        #region Public Constructors

        #endregion Public Constructors

        #region Public Methods

        [Trait("Bug", "153823")]
        [Trait("Error", "HeaderDeArquivo deve ter 240 caracteres")]
        [Fact]
        public async Task UnimakeEBankSolutionsExceptionsResponseException()
        {
            using var authScope = new AuthenticatedScope(new AuthenticationToken
            {
                AppId = "<< app id >>",
                Secret = "<< secret >>"
            });

            var pixService = new PIXService();

            var startDate = DateTime.Parse("2022-11-01");
            var endDate = DateTime.Parse("2022-11-30");

            //No response da consulta PIX tem as propriedades de paginação, mas sempre será 1:1
            var response = await pixService.GetAsync(new PIXGetRequest
            {
                StartDate = startDate,
                EndDate = endDate,
                Beneficiario = BeneficiarioDefault
            }, authScope);

            DumpAsJson(response);
        }

        #endregion Public Methods
    }
}