using EBank.Solutions.Primitives.Contract.Request;
using EBank.Solutions.Primitives.Exceptions;
using Newtonsoft.Json;
using System;
using System.Http;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;
using Unimake.AuthServer.Security.Scope;
using Unimake.EBank.Solutions.Exceptions;
using Unimake.Primitives.UDebug;
using HttpClient = EBank.Solutions.Primitives.Net.Http.HttpClient;

namespace Unimake.EBank.Solutions.Client
{
    /// <summary>
    /// Classe responsável por fazer requisições HTTP para a API do sistema EBank.
    /// </summary>
    public sealed class APIClient : IDisposable
    {
        #region Private Fields

        /// <summary>
        /// Escopo de autenticação utilizado para todas as requisições.
        /// </summary>
        private readonly AuthenticatedScope authenticatedScope;

        /// <summary>
        /// Cliente HTTP utilizado para enviar as requisições.
        /// </summary>
        private readonly HttpClient client = new HttpClient();

        /// <summary>
        /// String de consulta para as requisições.
        /// </summary>
        private QueryString _queryString;

        #endregion Private Fields

        #region Private Properties

        /// <summary>
        /// Objeto de estado para depuração.
        /// </summary>
        private static DebugStateObject debugStateObject => DebugScope<DebugStateObject>.Instance?.ObjectState;

        #endregion Private Properties

        #region Private Methods

        /// <summary>
        /// Garante que o cabeçalho de autorização esteja presente nas requisições.
        /// </summary>
        private void EnsureAuthorization()
        {
            client.DefaultRequestHeaders.Remove("Authorization");
            client.DefaultRequestHeaders.Add("Authorization", $"{authenticatedScope.Type} {authenticatedScope.Token}");
        }

        /// <summary>
        /// Realiza uma requisição POST assíncrona com o corpo fornecido.
        /// </summary>
        /// <param name="json">Corpo da requisição em formato JSON.</param>
        /// <param name="request">Objeto de requisição contendo os parâmetros necessários.</param>
        /// <returns>Resposta HTTP recebida da API.</returns>
        private async Task<HttpResponseMessage> PostAsync(string json, IRequest request)
        {
            EnsureAuthorization();
            var response = await client.PostAsync(PrepareURI(request), new StringContent(json, Encoding.UTF8, "application/json"));
            return response;
        }

        /// <summary>
        /// Prepara e processa a resposta da API, retornando o objeto esperado ou lançando exceção em caso de erro.
        /// </summary>
        /// <typeparam name="T">Tipo do objeto que será retornado na resposta.</typeparam>
        /// <param name="response">Resposta recebida do servidor.</param>
        /// <returns>Objeto do tipo T com o conteúdo da resposta.</returns>
        /// <exception cref="ResponseException">Lançada caso ocorra erro na resposta do servidor.</exception>
        private async Task<T> PrepareResponseAsync<T>(System.Net.Http.HttpResponseMessage response)
        {
            var json = await response.ReadAsJsonAsync();

            if(response.IsSuccessStatusCode())
            {
                if(response.StatusCode == System.Net.HttpStatusCode.NoContent)
                {
                    return default;
                }

                return JsonConvert.DeserializeObject<T>(json, new JsonSerializerSettings
                {
                    NullValueHandling = NullValueHandling.Ignore
                });
            }

            var errors = ExceptionObject.FromJson(json);
            System.Diagnostics.Debug.WriteLine(errors.Message);
            throw new ResponseException(errors.Message, response.StatusCode);
        }

        /// <summary>
        /// Prepara a URI completa para a requisição com base nos parâmetros fornecidos.
        /// </summary>
        /// <param name="request">Objeto de requisição utilizado para gerar a URI.</param>
        /// <returns>URI completa para a requisição.</returns>
        private string PrepareURI(IRequest request)
        {
            var uri = $"{debugStateObject?.AnotherServerUrl ?? $"https://unimake.app/ebank/api/v1/"}{Action}{ToQueryString(request)}";
            return uri;
        }

        /// <summary>
        /// Converte os parâmetros da requisição para uma string de consulta (query string).
        /// </summary>
        /// <param name="request">Objeto de requisição contendo os parâmetros.</param>
        /// <returns>String de consulta gerada a partir dos parâmetros.</returns>
        private string ToQueryString(IRequest request)
        {
            QueryString.AddOrUpdateValue("configurationId", request?.ConfigurationId ?? "");
            return QueryString.ToString(urlEncodeValue: false);
        }

        #endregion Private Methods

        #region Public Properties

        /// <summary>
        /// Ação específica da API que será utilizada nas requisições.
        /// </summary>
        public string Action { get; }

        /// <summary>
        /// Consulta de parâmetros para a requisição.
        /// </summary>
        public QueryString QueryString { get => _queryString ?? (_queryString = new QueryString()); }

        #endregion Public Properties

        #region Public Constructors

        /// <summary>
        /// Construtor da classe APIClient.
        /// </summary>
        /// <param name="scope">Escopo de autenticação.</param>
        /// <param name="action">Ação da API a ser utilizada nas requisições.</param>
        /// <exception cref="ArgumentNullException">Lançada caso o escopo de autenticação seja nulo.</exception>
        public APIClient(AuthenticatedScope scope, string action)
        {
            authenticatedScope = scope ?? throw new ArgumentNullException(nameof(scope));
            Action = action;
        }

        #endregion Public Constructors

        #region Public Methods

        /// <summary>
        /// Libera os recursos utilizados pela instância da classe.
        /// </summary>
        public void Dispose()
        {
            client.Dispose();
        }

        /// <summary>
        /// Realiza uma requisição GET assíncrona e retorna a resposta.
        /// </summary>
        /// <param name="request">Objeto de requisição.</param>
        /// <returns>Resposta HTTP da requisição.</returns>
        public async Task<HttpResponseMessage> GetAsync(IRequest request)
        {
            EnsureAuthorization();
            return await client.GetAsync(PrepareURI(request));
        }

        /// <summary>
        /// Realiza uma requisição GET assíncrona e retorna o resultado da resposta.
        /// </summary>
        /// <typeparam name="T">Tipo do resultado esperado.</typeparam>
        /// <param name="request">Objeto de requisição.</param>
        /// <returns>Resultado da resposta no tipo T.</returns>
        public async Task<T> GetAsync<T>(IRequest request)
        {
            EnsureAuthorization();
            return await PrepareResponseAsync<T>(await client.GetAsync(PrepareURI(request)));
        }

        /// <summary>
        /// Realiza uma requisição POST assíncrona e retorna o resultado da resposta.
        /// </summary>
        /// <typeparam name="T">Tipo do resultado esperado.</typeparam>
        /// <param name="request">Objeto de requisição.</param>
        /// <returns>Resultado da resposta no tipo T.</returns>
        public async Task<T> PostAsync<T>(IRequest request) => await PrepareResponseAsync<T>(await PostAsync(Newtonsoft.Json.JsonConvert.SerializeObject(request), request));

        /// <summary>
        /// Realiza uma requisição POST assíncrona e retorna a resposta.
        /// </summary>
        /// <param name="request">Objeto de requisição.</param>
        /// <returns>Resposta HTTP da requisição.</returns>
        public async Task<HttpResponseMessage> PostAsync(IRequest request) => await PostAsync(Newtonsoft.Json.JsonConvert.SerializeObject(request), request);

        #endregion Public Methods
    }
}