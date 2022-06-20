using Newtonsoft.Json;
using System.Collections.Generic;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Client;
using Unimake.EBank.Solutions.Exceptions;
using Unimake.EBank.Solutions.Scopes.Security;
using Unimake.EBank.Solutions.Services.Abstractions.Request;
using Unimake.EBank.Solutions.Services.Cobranca.Request;
using static Newtonsoft.Json.JsonConvert;

namespace Unimake.EBank.Solutions.Services.Abstractions.Service
{
    /// <summary>
    /// Serviço de consulta por arquivo
    /// </summary>
    public abstract class FileServiceBase<TRequest, TGetResponse, TFileResponse>
        where TRequest : FileRequestBase
        where TGetResponse : class, new()
        where TFileResponse : class, new()
    {
        #region Protected Properties

        /// <summary>
        /// Caminho base da requisição
        /// </summary>
        protected abstract string Path { get; }

        #endregion Protected Properties

        #region Public Methods

        /// <summary>
        /// Lista os resultados em JSON de forma resumida. Para consulta mais detalhadas, utilize o método <see cref="ListAsJsonAsync"/>
        /// </summary>
        /// <param name="request">Requisição com os dados para pesquisa</param>
        /// <param name="authenticatedScope">Escopo autenticado para uso no serviço</param>
        /// <returns></returns>
        public async Task<List<TGetResponse>> GetAsync(TRequest request, AuthenticatedScope authenticatedScope)
        {
            request.Validate();

            var apiClient = new APIClient(authenticatedScope, Path);
            var response = await apiClient.GetAsync(request.ToQueryString());
            var json = await response.Content.ReadAsStringAsync();

            return response.IsSuccessStatusCode
                ? DeserializeObject<List<TGetResponse>>(json, new JsonSerializerSettings
                {
                    NullValueHandling = NullValueHandling.Ignore
                })
                : throw new ResponseException(json, (int)response.StatusCode);
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
            var response = await apiClient.GetAsync(request.ToQueryString());
            var json = await response.Content.ReadAsStringAsync();

            return response.IsSuccessStatusCode
                ? DeserializeObject<List<TFileResponse>>(json, new JsonSerializerSettings
                {
                    NullValueHandling = NullValueHandling.Ignore
                })
                : throw new ResponseException(json, (int)response.StatusCode);
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
            var response = await apiClient.GetAsync(request.ToQueryString());
            var json = await response.Content.ReadAsStringAsync();

            return response.IsSuccessStatusCode
                ? DeserializeObject<List<TFileResponse>>(json, new JsonSerializerSettings
                {
                    NullValueHandling = NullValueHandling.Ignore
                })
                : throw new ResponseException(json, (int)response.StatusCode);
        }

        #endregion Public Methods
    }
}