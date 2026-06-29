using EBank.Solutions.Primitives.Pagamento.Request;
using EBank.Solutions.Primitives.Pagamento.Response;
using System.Collections.Generic;
using System.Threading.Tasks;
using Unimake.AuthServer.Security.Scope;
using Unimake.EBank.Solutions.Client;

namespace Unimake.EBank.Solutions.Services.Pagamento
{
    /// <summary>
    /// Serviço para realizar operações relacionadas a pagamentos.
    /// </summary>
    public class PagamentoService
    {
        #region Public Methods

        /// <summary>
        /// Realiza um pagamento utilizando a API.
        /// </summary>
        /// <param name="request">Requisição contendo os dados do pagamento.</param>
        /// <param name="authenticatedScope">Escopo autenticado utilizado para autorizar a requisição à API.</param>
        /// <returns>Resposta contendo o resultado do pagamento.</returns>
        public async Task<PagamentoResponse> AutorizarPagamentoAsync(PagamentoRequest request, AuthenticatedScope authenticatedScope) =>
            await new APIClient(authenticatedScope, $"pagamento/{request.Banco}/{request.Agencia}/{request.Conta}/autorizar").PostAsync<PagamentoResponse>(request);

        /// <summary>
        /// Cancela um pagamento utilizando a API.
        /// </summary>
        /// <param name="request">Requisição contendo os dados do pagamento a ser cancelado.</param>
        /// <param name="authenticatedScope">Escopo autenticado utilizado para autorizar a requisição à API.</param>
        /// <returns>Resposta contendo o resultado do cancelamento do pagamento.</returns>
        public async Task<List<LancamentoResponse>> CancelarPagamentoAsync(MultiplosPagamentosRequest request, AuthenticatedScope authenticatedScope) =>
            await new APIClient(authenticatedScope, $"pagamento/{request.Banco}/{request.Agencia}/{request.Conta}/cancelar").PostAsync<List<LancamentoResponse>>(request);

        /// <summary>
        /// Consulta um pagamento utilizando a API.
        /// </summary>
        /// <param name="request">Requisição contendo os dados para consulta do pagamento.</param>
        /// <param name="authenticatedScope">Escopo autenticado utilizado para autorizar a requisição à API.</param>
        /// <returns>Resposta contendo os detalhes do pagamento.</returns>
        public async Task<List<LancamentoResponse>> ConsultarPagamentosAsync(MultiplosPagamentosRequest request, AuthenticatedScope authenticatedScope) =>
            await new APIClient(authenticatedScope, $"pagamento/{request.Banco}/{request.Agencia}/{request.Conta}/consultar").PostAsync<List<LancamentoResponse>>(request);

        #endregion Public Methods
    }
}