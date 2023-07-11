using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Unimake.AuthServer.Security.Scope;
using Unimake.EBank.Solutions.Client;
using Unimake.EBank.Solutions.Converters.Json;
using Unimake.EBank.Solutions.Model.Pagamento;
using Unimake.EBank.Solutions.Services.Abstractions.Service;
using Unimake.EBank.Solutions.Services.Pagamento.Request;
using Unimake.EBank.Solutions.Services.Pagamento.Response;

namespace Unimake.EBank.Solutions.Services.Pagamento
{
    /// <summary>
    /// Serviço de cobrança
    /// <inheritdoc cref="FileServiceBase{TRequest, TGet, TJson, TCNAB}"/>
    /// </summary>
    public class PagamentoService : FileServiceBase<PagamentoRequest, ItemPagamento, ItemPagamentoJson, ItemPagamentoCNAB>
    {
        #region Protected Properties

        /// <summary>
        /// <inheritdoc cref="FileServiceBase{TRequest, TGet, TJson, TCNAB}.Path"/>
        /// </summary>
        protected override string Path => "Pagamento";

        #endregion Protected Properties

        #region Protected Methods

        /// <summary>
        /// <inheritdoc cref="FileServiceBase{TRequest, TGet, TJson, TCNAB}.GetConverter"/>
        /// </summary>
        /// <returns></returns>
        protected override JsonConverter GetConverter() => new PagamentoConverter();

        #endregion Protected Methods

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
            return await PrepareResponseAsync<AutorizarPagamentoResponse>(await apiClient.PostAsync(request));
        }

        #endregion Public Methods
    }
}