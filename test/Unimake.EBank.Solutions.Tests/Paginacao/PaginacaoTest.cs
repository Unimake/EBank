using EBank.Solutions.Primitives.Enumerations;
using System;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Services.Extrato;
using Unimake.EBank.Solutions.Tests.Abstractions;
using Xunit;
using Xunit.Abstractions;

namespace Unimake.EBank.Solutions.Tests.Paginacao
{
    public class PaginacaoTest(ITestOutputHelper output) : TestBase(output)
    {

        #region Public Constructors

        #endregion Public Constructors

        #region Public Methods

        [Fact]
        public async Task GetByPage_LoopFor()
        {
            // Definir a página de início.
            var currentPage = 1;

            // Criar escopo de autenticação
            using var scope = await CreateAuthenticatedScopeAsync();

            // Criar serviço de extrato
            var service = new ExtratoService();

            // Criar a requisição
            var request = new Services.Extrato.Request.ExtratoRequest
            {
                Testing = true,
                AccountNumber = "94914",
                StartDate = DateTime.Parse("2024-08-01"),
                EndDate = DateTime.Parse("2024-08-05"),
                Bank = Banco.Sicredi,
                PageNumber = currentPage,
                PageSize = 5
            };

            // Buscar os dados paginados
            var response = await service.GetAsync(request, scope);

            // For percorrendo as páginas

            for(var i = 0; i < response.PageInfo.TotalPages; i++)
            {
                // ... faça seus tratamentos aqui ...
                DumpAsJson(response);

                // Buscar próxima página
                request.PageNumber = ++currentPage;
                request.Testing = true;

                // Buscar os dados paginados
                response = await service.GetAsync(request, scope);
            }
        }

        [Fact]
        public async Task GetByPage_LoopWhile()
        {
            // Definir a página de início.
            var currentPage = 1;

            // Assume que existe
            var hasNext = true;

            // Criar escopo de autenticação
            using var scope = await CreateAuthenticatedScopeAsync();

            // Criar serviço de extrato
            var service = new ExtratoService();

            // Criar a requisição
            var request = new Services.Extrato.Request.ExtratoRequest
            {
                Testing = true,
                AccountNumber = "94914",
                StartDate = DateTime.Parse("2024-08-01"),
                EndDate = DateTime.Parse("2024-08-05"),
                Bank = Banco.Sicredi,
                PageNumber = currentPage,
                PageSize = 5
            };

            // Enquanto hasNext for verdadeiro, leia
            while(hasNext)
            {
                // Buscar os dados paginados
                var response = await service.GetAsync(request, scope);
                hasNext = response.PageInfo.HasNext;

                // ... faça seus tratamentos aqui ...
                DumpAsJson(response);

                // Buscar próxima página
                request.PageNumber = ++currentPage;
            }
        }

        #endregion Public Methods
    }
}