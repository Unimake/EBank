using EBank.Solutions.Primitives.Enumerations;
using System;
using Unimake.AuthServer.Authentication;
using Unimake.AuthServer.Security.Scope;
using Unimake.EBank.Solutions.Services.Extrato;
using Unimake.EBank.Solutions.Services.Extrato.Request;
using Unimake.EBank.Solutions.Tests.Abstractions;
using Xunit;
using Xunit.Abstractions;

namespace Unimake.EBank.Solutions.Tests.BugFixes
{
    public class BugFix149986 : TestBase
    {
        #region Public Constructors

        public BugFix149986(ITestOutputHelper output)
            : base(output)
        {
        }

        #endregion Public Constructors

        #region Public Methods

        [Trait("Bug", "149986")]
        [Trait("Error", "HeaderDeArquivo deve ter 240 caracteres")]
        [Fact]
        public async void Get240Caracteres()
        {
            StartServerDebugMode();

            using var authScope = new AuthenticatedScope(new AuthenticationRequest
            {
                AppId = "<<appid>>",
                Secret = "<<secret>>"
            });

            var extratoService = new ExtratoService();

            var startDate = new DateTime(2022, 10, 01);
            var endDate = new DateTime(2022, 10, 07);
            var pageNumber = 1;

            var response = await extratoService.GetAsync(new ExtratoRequest
            {
                AccountNumber = "82406",
                StartDate = startDate,
                EndDate = endDate,
                Bank = Banco.Sicoob,
                PageNumber = pageNumber, //Número de página a ser consultada
                PageSize = 10 //Quantos itens por página
            }, authScope);

            DumpAsJson(response);
        }

        #endregion Public Methods
    }
}