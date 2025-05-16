using EBank.Solutions.Primitives.Billet.Request;
using EBank.Solutions.Primitives.Billet.Response;
using EBank.Solutions.Primitives.Exceptions.Response.Billet;
using System.Collections.Generic;
using System.Threading.Tasks;
using Unimake.AuthServer.Security.Scope;
using Unimake.EBank.Solutions.Client;
using Unimake.EBank.Solutions.Exceptions;

namespace Unimake.EBank.Solutions.Services.Billet
{
    /// <summary>
    /// Serviço de registro de Billets
    /// </summary>
    public class BilletService
    {
        #region Public Methods

        /// <summary>
        /// Realiza o cancelamento do boleto
        /// </summary>
        /// <param name="request">Dados do boleto para cancelamento</param>
        /// <param name="authenticatedScope">Escopo autenticado</param>
        /// <returns></returns>
        /// <exception cref="BaixarResponseException">Erros diversos no cancelamento</exception>
        public async Task<BaixarResponse> BaixarAsync(BaixarRequest request, AuthenticatedScope authenticatedScope) =>
            await new APIClient(authenticatedScope, "boleto/baixar").PostAsync<BaixarResponse>(request);

        /// <summary>
        /// Altera o vencimento do boleto
        /// </summary>
        /// <param name="request">Dados do novo vencimento</param>
        /// <param name="authenticatedScope">Escopo autenticado</param>
        /// <returns></returns>
        /// <exception cref="ExtendPaymentResponseException">Erros diversos ocorridos durante a alteração de vencimento</exception>
        public async Task<ExtendPaymentResponse> ExtendPaymentAsync(ExtendPaymentRequest request, AuthenticatedScope authenticatedScope) =>
            await new APIClient(authenticatedScope, "boleto/alterarvencimento").PostAsync<ExtendPaymentResponse>(request);

        /// <summary>
        /// Enviar instruções ao boleto
        /// </summary>
        /// <param name="request">Dados instrução de boleto</param>
        /// <param name="authenticatedScope">Escopo autenticado</param>
        /// <exception cref="ResponseException">Erros gerais durante a instrução</exception>
        /// <returns></returns>
        public async Task<PutInstructionsResponse> PutInstructionsAsync(PutInstructionsRequest request, AuthenticatedScope authenticatedScope) =>
            await new APIClient(authenticatedScope, "boleto/enviarinstrucao").PostAsync<PutInstructionsResponse>(request);

        /// <summary>
        /// Realiza a consulta de um boleto
        /// </summary>
        /// <param name="request">Requisição que será enviada ao servidor</param>
        /// <param name="authenticatedScope">Escopo de autenticação válido</param>
        /// <returns></returns>
        /// <exception cref="QueryInformationResponseException">Lançada quando ocorrer erros na validação ou consulta dos boletos</exception>
        public async Task<List<QueryInformationResponse>> QueryAsync(QueryInformationRequest request, AuthenticatedScope authenticatedScope) =>
                await new APIClient(authenticatedScope, "boleto/consultar").PostAsync<List<QueryInformationResponse>>(request);

        /// <summary>
        /// Realiza o registro de um boleto de modo assíncrono
        /// </summary>
        /// <param name="request">Requisição que será enviada ao servidor</param>
        /// <param name="authenticatedScope">Escopo de autenticação válido</param>
        /// <returns></returns>
        /// <exception cref="RegisterResponseException">Lançada quando ocorrer erros na validação ou emissão dos Billets</exception>
        public async Task<RegisterResponse> RegisterAsync(RegisterRequest request, AuthenticatedScope authenticatedScope) =>
            await new APIClient(authenticatedScope, "boleto/registrar").PostAsync<RegisterResponse>(request);

        #endregion Public Methods
    }
}