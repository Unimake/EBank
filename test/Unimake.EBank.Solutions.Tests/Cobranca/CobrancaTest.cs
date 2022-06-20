﻿using EBank.Solutions.Primitives.Enumerations;
using System;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Services.Cobranca;
using Unimake.EBank.Solutions.Services.Cobranca.Request;
using Unimake.EBank.Solutions.Tests.Abstractions;
using Xunit;
using Xunit.Abstractions;

namespace Unimake.EBank.Solutions.Tests.Cobranca
{
    public class CobrancaTest : TestBase
    {
        #region Public Constructors

        public CobrancaTest(ITestOutputHelper output)
            : base(output)
        {
        }

        #endregion Public Constructors

        #region Public Methods

        [Fact]
        public async Task ListAsJsonAsync()
        {
            try
            {
                var service = new CobrancaService();
                using var scope = await CreateAuthenticatedScopeAsync();
                var response = await service.ListAsJsonAsync(new CobrancaRequest
                {
                    AccountNumber = "007145",
                    StartDate = DateTime.Parse("24/07/2020"),
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
        public async Task ListAsCnabAsync()
        {
            try
            {
                var service = new CobrancaService();
                using var scope = await CreateAuthenticatedScopeAsync();
                var response = await service.ListAsCnabAsync(new CobrancaRequest
                {
                    AccountNumber = "007145",
                    StartDate = DateTime.Parse("24/07/2020"),
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
        public async Task Get()
        {
            try
            {
                var service = new CobrancaService();
                using var scope = await CreateAuthenticatedScopeAsync();
                var response = await service.GetAsync(new CobrancaRequest
                {
                    AccountNumber = "007145",
                    StartDate = DateTime.Parse("24/07/2020"),
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