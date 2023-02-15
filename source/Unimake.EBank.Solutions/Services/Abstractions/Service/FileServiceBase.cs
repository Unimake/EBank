using EBank.Solutions.Primitives.Exceptions;
using Newtonsoft.Json;
using System.Collections.Generic;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Client;
using Unimake.EBank.Solutions.Converters.Json;
using Unimake.EBank.Solutions.Exceptions;
using Unimake.EBank.Solutions.Scopes.Security;
using Unimake.EBank.Solutions.Services.Abstractions.Request;
using Unimake.Primitives.Collections.Page;
using static Newtonsoft.Json.JsonConvert;

namespace Unimake.EBank.Solutions.Services.Abstractions.Service
{
    /// <summary>
    /// Serviço de consulta por arquivo
    /// </summary>
    /// <typeparam name="TRequest">Requisição do serviço</typeparam>
    /// <typeparam name="TCNAB">Retorno para as requisições do tipo CNAB, <see cref="ListAsCnabAsync(TRequest, AuthenticatedScope)"/> </typeparam>
    /// <typeparam name="TGet">Retorno para as requisições do tipo Get, <see cref="GetAsync(TRequest, AuthenticatedScope)"/></typeparam>
    /// <typeparam name="TJson">Retorno para as requisições do tipo Json, <see cref="ListAsJsonAsync(TRequest, AuthenticatedScope)"/></typeparam>
    public abstract class FileServiceBase<TRequest, TGet, TJson, TCNAB>
        where TRequest : FileRequestBase
    {
        #region Protected Properties

        /// <summary>
        /// Caminho base da requisição
        /// </summary>
        protected abstract string Path { get; }

        #endregion Protected Properties

        #region Protected Methods

        /// <summary>
        /// Conversor para os tipos
        /// </summary>
        /// <returns></returns>
        protected abstract JsonConverter GetConverter();

        /// <summary>
        /// Prepara a resposta e retorna.
        /// </summary>
        /// <typeparam name="T">Tipo de resultado</typeparam>
        /// <param name="response">Resposta recebida do servidor</param>
        /// <returns></returns>
        /// <exception cref="ResponseException">Exceção lançada caso ocorra erro no servidor</exception>
        protected async Task<T> PrepareResponseAsync<T>(System.Net.Http.HttpResponseMessage response)
        {
            var json = await response.Content.ReadAsStringAsync();

            if(response.IsSuccessStatusCode)
            {
                return DeserializeObject<T>(json, new JsonSerializerSettings
                {
                    NullValueHandling = NullValueHandling.Ignore,
                    Converters = new List<JsonConverter>
                    {
                        new CharConverter(),
                        GetConverter()
                    }
                });
            }

            var errors = ExceptionObject.FromJson(json);
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
        public async Task<PagedList<TGet>> GetAsync(TRequest request, AuthenticatedScope authenticatedScope)
        {
            request.Validate();
            var apiClient = new APIClient(authenticatedScope, Path, request.ToQueryString());
            return await PrepareResponseAsync<PagedList<TGet>>(await apiClient.GetAsync());
        }

        /// <summary>
        /// Lista os arquivos em formato CNAB. Ideal para sistemas que não fazem a leitura no formato JSON
        /// </summary>
        /// <param name="request">Requisição com os dados para pesquisa</param>
        /// <param name="authenticatedScope">Escopo autenticado para uso no serviço</param>
        /// <returns></returns>
        public async Task<PagedList<TCNAB>> ListAsCnabAsync(TRequest request, AuthenticatedScope authenticatedScope)
        {
            request.Validate();

            var apiClient = new APIClient(authenticatedScope, $"{Path}/ListarCnab", request.ToQueryString());
            return await PrepareResponseAsync<PagedList<TCNAB>>(await apiClient.GetAsync());
        }

        /// <summary>
        /// Faz a listagem do que tem para cobrança dentro do período definido na requisição
        /// </summary>
        /// <param name="request">Requisição com as definições de filtragem do resultado da listagem</param>
        /// <param name="authenticatedScope">Escopo autenticado para uso no serviço</param>
        /// <returns></returns>
        public async Task<PagedList<TJson>> ListAsJsonAsync(TRequest request, AuthenticatedScope authenticatedScope)
        {
            request.Validate();

            var apiClient = new APIClient(authenticatedScope, $"{Path}/ListarJson", request.ToQueryString());
            return await PrepareResponseAsync<PagedList<TJson>>(await apiClient.GetAsync());
        }

        #endregion Public Methods
    }
}