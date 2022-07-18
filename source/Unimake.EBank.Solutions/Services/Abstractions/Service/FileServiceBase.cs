using EBank.Solutions.Primitives.Exceptions;
using Newtonsoft.Json;
using System.Collections.Generic;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Client;
using Unimake.EBank.Solutions.Exceptions;
using Unimake.EBank.Solutions.Scopes.Security;
using Unimake.EBank.Solutions.Services.Abstractions.Request;
using static Newtonsoft.Json.JsonConvert;

namespace Unimake.EBank.Solutions.Services.Abstractions.Service
{
    /// <summary>
    /// Serviço de consulta por arquivo
    /// </summary>
    public abstract class FileServiceBase<TRequest, TItem, TFileResponse>
        where TRequest : FileRequestBase
        where TItem : class, new()
        where TFileResponse : class, new()
    {
        #region Protected Properties

        /// <summary>
        /// Caminho base da requisição
        /// </summary>
        protected abstract string Path { get; }

        #endregion Protected Properties

        #region Protected Methods

        /// <summary>
        /// Prepara a resposta e retorna.
        /// </summary>
        /// <typeparam name="T">Tipo de resultado</typeparam>
        /// <param name="response">Resposta recebida do servidor</param>
        /// <returns></returns>
        /// <exception cref="ResponseException">Exceção lançada caso ocorra erro no servidor</exception>
        protected static async Task<T> PrepareResponseAsync<T>(System.Net.Http.HttpResponseMessage response)
        {
            var json = await response.Content.ReadAsStringAsync();

            if(response.IsSuccessStatusCode)
            {
                return DeserializeObject<T>(json, new JsonSerializerSettings
                {
                    NullValueHandling = NullValueHandling.Ignore
                });
            }

            var errors = DeserializeObject<ExceptionObject>(json);
            System.Diagnostics.Debug.WriteLine(errors.Message);
            throw new ResponseException(errors.Message, (int)response.StatusCode);
        }

        #endregion Protected Methods

        #region Public Methods

        /// <summary>
        /// Lista os resultados em JSON de forma resumida. Para consulta mais detalhadas, utilize o método <see cref="ListAsJsonAsync"/>
        /// </summary>
        /// <param name="request">Requisição com os dados para pesquisa</param>
        /// <param name="authenticatedScope">Escopo autenticado para uso no serviço</param>
        /// <returns></returns>
        public async Task<List<TItem>> GetAsync(TRequest request, AuthenticatedScope authenticatedScope)
        {
            request.Validate();
            var apiClient = new APIClient(authenticatedScope, Path);
            return await PrepareResponseAsync<List<TItem>>(await apiClient.GetAsync(request.ToQueryString()));
        }

        /// <summary>
        /// Lista os arquivos em formato CNAB. Ideal para sistemas que não fazem a leitura no formato JSON
        /// </summary>
        /// <param name="request">Requisição com os dados para pesquisa</param>
        /// <param name="authenticatedScope">Escopo autenticado para uso no serviço</param>
        /// <returns></returns>
        public async Task<List<TFileResponse>> ListAsCnabAsync(TRequest request, AuthenticatedScope authenticatedScope)
        {
            request.Validate();

            var apiClient = new APIClient(authenticatedScope, $"{Path}/ListarCnab");
            return await PrepareResponseAsync<List<TFileResponse>>(await apiClient.GetAsync(request.ToQueryString()));
        }

        /// <summary>
        /// Faz a listagem do que tem para cobrança dentro do período definido na requisição
        /// </summary>
        /// <param name="request">Requisição com as definições de filtragem do resultado da listagem</param>
        /// <param name="authenticatedScope">Escopo autenticado para uso no serviço</param>
        /// <returns></returns>
        public async Task<List<TFileResponse>> ListAsJsonAsync(TRequest request, AuthenticatedScope authenticatedScope)
        {
            request.Validate();

            var apiClient = new APIClient(authenticatedScope, $"{Path}/ListarJson");
            return await PrepareResponseAsync<List<TFileResponse>>(await apiClient.GetAsync(request.ToQueryString()));
        }

        #endregion Public Methods
    }
}