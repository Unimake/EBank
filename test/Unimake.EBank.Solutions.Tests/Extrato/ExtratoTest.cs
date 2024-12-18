using EBank.Solutions.Primitives.Enumerations;
using System;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Services.Extrato;
using Unimake.EBank.Solutions.Services.Extrato.Request;
using Unimake.EBank.Solutions.Tests.Abstractions;
using Xunit;
using Xunit.Abstractions;

namespace Unimake.EBank.Solutions.Tests.Extrato
{
    public class ExtratoTest(ITestOutputHelper output) : TestBase(output)
    {
        #region Private Methods

        private static ExtratoRequest GetRequest() => new()
        {
            Testing = true,
            AccountNumber = "82406",
            StartDate = DateTime.Now.AddDays(-3),
            EndDate = DateTime.Now,
            Bank = Banco.Sicoob,
        };

        #endregion Private Methods
        #region Public Constructors

        #endregion Public Constructors

        #region Public Methods

        [Fact]
        public async Task Get()
        {
            try
            {
                using var scope = await CreateAuthenticatedScopeAsync();
                var service = new ExtratoService();
                var response = await service.GetAsync(GetRequest(), scope);

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
                var service = new ExtratoService();
                using var scope = await CreateAuthenticatedScopeAsync();
                var response = await service.ListAsCnabAsync(GetRequest(), scope);

                DumpAsJson(response);
            }
            catch(Exception ex)
            {
                DumpAsJson(ex);
                throw;//forward
            }
        }

        [Fact]
        public async Task ListAsJsonAsync()
        {
            try
            {
                var service = new ExtratoService();
                using var scope = await CreateAuthenticatedScopeAsync();
                var response = await service.ListAsJsonAsync(GetRequest(), scope);
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