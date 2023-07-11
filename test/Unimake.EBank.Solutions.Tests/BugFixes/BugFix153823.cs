using EBank.Solutions.Primitives.Billet.Models;
using EBank.Solutions.Primitives.Enumerations;
using EBank.Solutions.Primitives.PIX.Request.Consulta;
using System;
using Unimake.AuthServer.Authentication;
using Unimake.AuthServer.Security.Scope;
using Unimake.EBank.Solutions.Services.PIX;
using Unimake.EBank.Solutions.Tests.Abstractions;
using Xunit;
using Xunit.Abstractions;

namespace Unimake.EBank.Solutions.Tests.BugFixes
{
    public class BugFix153823 : TestBase
    {
        #region Public Constructors

        public BugFix153823(ITestOutputHelper output)
            : base(output)
        {
        }

        #endregion Public Constructors

        #region Public Methods

        [Trait("Bug", "153823")]
        [Trait("Error", "HeaderDeArquivo deve ter 240 caracteres")]
        [Fact]
        public async void UnimakeEBankSolutionsExceptionsResponseException()
        {
            using var authScope = new AuthenticatedScope(new AuthenticationRequest
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
                Beneficiario = new Beneficiario
                {
                    Conta = new ContaCorrente
                    {
                        Agencia = "4340",
                        Numero = "1899430",
                        Banco = Banco.Sicoob,
                    },
                    Inscricao = "37765786000148",
                    Nome = "Dream Solutions" //Não é obrigatório
                }
            }, authScope);

            DumpAsJson(response);
        }

        #endregion Public Methods
    }
}