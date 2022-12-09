using EBank.Solutions.Primitives.Exceptions;
using EBank.Solutions.Primitives.PIX.Models.Consulta;
using EBank.Solutions.Primitives.PIX.Request;
using EBank.Solutions.Primitives.PIX.Response;
using Newtonsoft.Json;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Client;
using Unimake.EBank.Solutions.Exceptions;
using Unimake.EBank.Solutions.Scopes.Security;
using Unimake.Primitives.Collections.Page;

namespace Unimake.EBank.Solutions.Services.PIX
{
    /// <summary>
    /// Serviços PIX
    /// </summary>
    public class PIXService
    {
        #region Private Methods

        /// <summary>
        /// Prepara a resposta e retorna.
        /// </summary>
        /// <typeparam name="T">Tipo de resultado</typeparam>
        /// <param name="response">Resposta recebida do servidor</param>
        /// <returns></returns>
        /// <exception cref="ResponseException">Exceção lançada caso ocorra erro no servidor</exception>
        private async Task<T> PrepareResponseAsync<T>(System.Net.Http.HttpResponseMessage response)
        {
            var json = await response.Content.ReadAsStringAsync();

            if(response.IsSuccessStatusCode)
            {
                return JsonConvert.DeserializeObject<T>(json, new JsonSerializerSettings
                {
                    NullValueHandling = NullValueHandling.Ignore
                });
            }

            var errors = ExceptionObject.FromJson(json);
            System.Diagnostics.Debug.WriteLine(errors.Message);
            throw new ResponseException(errors.Message, (int)response.StatusCode);
        }

        #endregion Private Methods

        #region Public Methods

        /// <summary>
        /// Faz uma pesquisa pela chave de transação do PIX
        /// </summary>
        /// <param name="request">Dados da requisição PIX</param>
        /// <param name="authenticatedScope">Escopo autenticado para requisição</param>
        /// <returns></returns>
        public async Task<PagedList<PIXItem>> GetAsync(PIXGetRequest request, AuthenticatedScope authenticatedScope)
        {
            var client = new APIClient(authenticatedScope, "pix/consultar");
            var response = await client.PostAsync(request);
            return await PrepareResponseAsync<PagedList<PIXItem>>(response);
        }

        #endregion Public Methods
    }
}