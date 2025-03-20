using EBank.Solutions.Primitives.PIX.Models.Consulta;
using EBank.Solutions.Primitives.PIX.Request.Consulta;
using System;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Services.PIX;
using Unimake.EBank.Solutions.Tests.Abstractions;
using Unimake.Primitives.Collections.Page;
using Xunit;
using Xunit.Abstractions;

namespace Unimake.EBank.Solutions.Tests.PIX
{
    public class PIXConsultarTest(ITestOutputHelper output) : TestBase(output)
    {
        #region Public Methods

        [Fact]
        public async Task ConsultarE2EIdTestAsync()
        {
            try
            {
                using var scope = await CreateAuthenticatedScopeAsync();
                var service = new PIXService();
                var response = await service.GetAsync(CreateRequest(() => new PIXGetRequest
                {
                    EndToEndId = $"E{new Random().NextString(31, true, false).ToUpper()}"
                }), scope);

                DumpAsJson(response);
            }
            catch(Exception ex)
            {
                DumpAsJson(ex);
                throw;//forward
            }
        }

        [Fact]
        public async Task ConsultarPaginadoTestAsync()
        {
            try
            {
                using var scope = await CreateAuthenticatedScopeAsync();
                var service = new PIXService();
                var currentPage = 1;
                var response = default(PagedList<PIXItem>);

                do
                {
                    response = await service.GetAsync(CreateRequest(() => new PIXGetRequest
                    {
                        StartDate = StartDate,
                        EndDate = EndDate,
                        PageNumber = currentPage,
                        PageSize = 3
                    }), scope);

                    DumpAsJson(response);
                    currentPage++;
                } while(response.PageInfo.HasNext);
            }
            catch(Exception ex)
            {
                DumpAsJson(ex);
                throw;//forward
            }
        }

        [Fact]
        public async Task ConsultarTxIdTestAsync()
        {
            try
            {
                using var scope = await CreateAuthenticatedScopeAsync();
                var service = new PIXService();
                var response = await service.GetAsync(CreateRequest(() => new PIXGetRequest
                {
                    StartDate = StartDate,
                    EndDate = EndDate,
                    TxId = "xxInformeUmTxIdValidoAquixx"
                }), scope);

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