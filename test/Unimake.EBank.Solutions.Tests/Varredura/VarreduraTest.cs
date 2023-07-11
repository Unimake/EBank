using EBank.Solutions.Primitives.Enumerations;
using System;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Services.Varredura;
using Unimake.EBank.Solutions.Services.Varredura.Request;
using Unimake.EBank.Solutions.Tests.Abstractions;
using Xunit;
using Xunit.Abstractions;

namespace Unimake.EBank.Solutions.Tests.Varredura
{
    public class VarreduraTest : TestBase
    {
        #region Private Methods

        private static VarreduraRequest GetRequest() => new()
        {
            StartDate = DateTime.Parse("2022-08-29"),
            EndDate = DateTime.Parse("2022-08-30"),
            Bank = Banco.Sicredi,
        };

        #endregion Private Methods

        #region Public Constructors

        public VarreduraTest(ITestOutputHelper output)
                    : base(output)
        {
            StartServerDebugMode();
        }

        #endregion Public Constructors

        #region Public Methods

        [Fact]
        public async Task Get()
        {
            try
            {
                using var scope = await CreateAuthenticatedScopeAsync();
                var service = new VarreduraService();
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
                var service = new VarreduraService();
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
                var service = new VarreduraService();
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