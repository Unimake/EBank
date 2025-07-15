using EBank.Solutions.Primitives.Extrato.Request;
using EBank.Solutions.Primitives.Extrato.Response;
using System;
using System.Threading.Tasks;
using Unimake.AuthServer.Security.Scope;
using Unimake.EBank.Solutions.Client;

namespace Unimake.EBank.Solutions.Services.Extrato
{
    /// <summary>
    /// Serviço de consulta de extrato
    /// </summary>
    public class ExtratoService
    {
        #region Public Methods

        /// <summary>
        /// Retorna o extrato de uma conta bancária, com base no período informado.
        /// </summary>
        /// <param name="request">Requisição</param>
        /// <param name="authenticatedScope">Escopo autenticado</param>
        /// <returns></returns>
        public async Task<ExtratoResponse> GetExtratoAsync(ExtratoRequest request, AuthenticatedScope authenticatedScope) =>
           await new APIClient(authenticatedScope, $"extrato/{request.Banco}/" +
                                                   $"{request.Agencia}/" +
                                                   $"{request.Conta}")
            {
                QueryString = {
                    { nameof(request.Testing), request.Testing },
                    { nameof(request.EndDate), request.EndDate.FormatDate("yyyy-MM-dd") },
                    { nameof(request.StartDate), request.StartDate.FormatDate("yyyy-MM-dd") }
                }
            }.GetAsync<ExtratoResponse>(request);

        #endregion Public Methods
    }
}