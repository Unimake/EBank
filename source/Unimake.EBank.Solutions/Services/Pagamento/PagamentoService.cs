using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Client;
using Unimake.EBank.Solutions.Exceptions;
using Unimake.EBank.Solutions.Scopes.Security;
using Unimake.EBank.Solutions.Services.Abstractions.Service;
using Unimake.EBank.Solutions.Services.Pagamento.Request;
using Unimake.EBank.Solutions.Services.Pagamento.Response;

namespace Unimake.EBank.Solutions.Services.Pagamento
{
    /// <summary>
    /// Serviço de cobrança
    /// <inheritdoc cref="FileServiceBase{TRequest, TGetResponse, TFileResponse}"/>
    /// </summary>
    public class PagamentoService : FileServiceBase<PagamentoRequest, PagamentoResponse, ArquivoPagamentoResponse>
    {
        #region Protected Properties

        /// <summary>
        /// <inheritdoc cref="FileServiceBase{TRequest, TGetResponse, TFileResponse}.Path"/>
        /// </summary>
        protected override string Path => "Pagamento";

        #endregion Protected Properties

        #region Public Methods

        /// <summary>
        /// Faz a autorização dos pagamentos
        /// </summary>
        /// <param name="request">Pagamentos que serão autorizados</param>
        /// <param name="authenticatedScope">Escopo autenticado para acesso ao serviço de autorização</param>
        /// <returns></returns>
        public async Task<AutorizarPagamentoResponse> AutorizarPagamento(List<AutorizarPagamentoRequest> request, AuthenticatedScope authenticatedScope)
        {
            if(request.IsNullOrEmpty())
            {
                throw new ArgumentNullException(nameof(request));
            }

            var apiClient = new APIClient(authenticatedScope, $"{Path}/autorizar");
            var json = Newtonsoft.Json.JsonConvert.SerializeObject(request);
            var response = await apiClient.PostAsync(json);
            var jsonResponse = await response.Content.ReadAsStringAsync();
            return response.IsSuccessStatusCode ? new AutorizarPagamentoResponse() : throw new ResponseException(jsonResponse, (int)response.StatusCode);
        }

        #endregion Public Methods
    }
}