using EBank.Solutions.Primitives.Extrato.Request;
using System;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Tests.Abstractions;
using Xunit;
using Xunit.Abstractions;

namespace Unimake.EBank.Solutions.Tests.Extrato
{
    public class ExtratoTest(ITestOutputHelper output) : TestBase(output)
    {
        #region Public Methods

        [Fact]
        public async Task GetExtrato()
        {
            using var scope = await CreateAuthenticatedScopeAsync();
            var service = new Services.Extrato.ExtratoService();
            var response = await service.GetExtratoAsync(CreateRequest(() => new ExtratoRequest
            {
                StartDate = DateTime.Today.AddDays(-30),
                EndDate = DateTime.Today,
            }), scope);

            Assert.NotNull(response);
            Assert.NotEmpty(response.Items);
            Assert.NotEmpty(response.Saldos);
        }

        #endregion Public Methods
    }
}