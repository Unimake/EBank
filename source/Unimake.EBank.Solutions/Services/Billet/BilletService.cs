using EBank.Solutions.Primitives.Exceptions.Response.Billet;
using System.Collections.Generic;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Exceptions;
using Unimake.EBank.Solutions.Resolver;
using Unimake.EBank.Solutions.Scopes.Security;
using Unimake.EBank.Solutions.Services.Billet.Request;
using Unimake.EBank.Solutions.Services.Billet.Response;
using Unimake.Threading;

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
        /// <exception cref="CancelResponseException">Erros diversos no cancelamento</exception>
        public async Task<CancelResponse> CancelAsync(CancelRequest request, AuthenticatedScope authenticatedScope)
        {
            return await BilletServiceClient<CancelRequest, CancelResponse, CancelResponseException>.RequestAsync(request, authenticatedScope, "cancelar");
        }

        /// <summary>
        /// Altera o vencimento do boleto
        /// </summary>
        /// <param name="request">Dados do novo vencimento</param>
        /// <param name="authenticatedScope">Escopo autenticado</param>
        /// <returns></returns>
        /// <exception cref="ExtendPaymentResponseException">Erros diversos ocorridos durante a alteração de vencimento</exception>
        public async Task<ExtendPaymentResponse> ExtendPaymentAsync(ExtendPaymentRequest request, AuthenticatedScope authenticatedScope)
        {
            return await BilletServiceClient<ExtendPaymentRequest, ExtendPaymentResponse, ExtendPaymentResponseException>.RequestAsync(request, authenticatedScope, "alterarvencimento");
        }

        /// <summary>
        /// Informar que o boleto foi pago
        /// </summary>
        /// <param name="request">Dados para pagamento do boleto</param>
        /// <param name="authenticatedScope">Escopo autenticado</param>
        /// <exception cref="ResponseException">Erros gerais durante a confirmação do pagamento</exception>
        /// <returns></returns>
        public async Task<InformPaymentResponse> InformPaymentAsync(InformPaymentRequest request, AuthenticatedScope authenticatedScope)
        {
            return await BilletServiceClient<InformPaymentRequest, InformPaymentResponse, ResponseException>.RequestAsync(request, authenticatedScope, "informarpagamento");
        }

        /// <summary>
        /// Enviar instruções ao boleto
        /// </summary>
        /// <param name="request">Dados instrução de boleto</param>
        /// <param name="authenticatedScope">Escopo autenticado</param>
        /// <exception cref="ResponseException">Erros gerais durante a instrução</exception>
        /// <returns></returns>
        public async Task<PutInstructionsResponse> PutInstructionsAsync(PutInstructionsRequest request, AuthenticatedScope authenticatedScope)
        {
            return await BilletServiceClient<PutInstructionsRequest, PutInstructionsResponse, ResponseException>.RequestAsync(request, authenticatedScope, "enviarinstrucao");
        }

        /// <summary>
        /// Realiza a consulta de um boleto
        /// </summary>
        /// <param name="request">Requisição que será enviada ao servidor</param>
        /// <param name="authenticatedScope">Escopo de autenticação válido</param>
        /// <returns></returns>
        /// <exception cref="QueryInformationResponseException">Lançada quando ocorrer erros na validação ou consulta dos boletos</exception>
        public async Task<List<QueryResponse>> QueryAsync(QueryRequest request, AuthenticatedScope authenticatedScope)
        {
            return await BilletServiceClient<QueryRequest, List<QueryResponse>, QueryInformationResponseException>.RequestAsync(request, authenticatedScope, "consultar");
        }

        /// <summary>
        /// Realiza o registro de um boleto
        /// </summary>
        /// <param name="request">Requisição que será enviada ao servidor</param>
        /// <param name="authenticatedScope">Escopo de autenticação válido</param>
        /// <returns></returns>
        /// <exception cref="RegisterResponseException">Lançada quando ocorrer erros na validação ou emissão dos Billets</exception>
        public RegisterResponse Register(RegisterRequest request, AuthenticatedScope authenticatedScope) =>
            AsyncHelper.RunSync(() => RegisterAsync(request, authenticatedScope));

        /// <summary>
        /// Realiza o registro de um boleto de modo assíncrono
        /// </summary>
        /// <param name="request">Requisição que será enviada ao servidor</param>
        /// <param name="authenticatedScope">Escopo de autenticação válido</param>
        /// <returns></returns>
        /// <exception cref="RegisterResponseException">Lançada quando ocorrer erros na validação ou emissão dos Billets</exception>
        public async Task<RegisterResponse> RegisterAsync(RegisterRequest request, AuthenticatedScope authenticatedScope)
        {
            return await BilletServiceClient<RegisterRequest, RegisterResponse, RegisterResponseException>.RequestAsync(request, authenticatedScope, "registrar");
        }

        #endregion Public Methods
    }
}