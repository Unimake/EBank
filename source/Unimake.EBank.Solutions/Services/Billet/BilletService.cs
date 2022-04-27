using EBank.Solutions.Primitives.Exceptions.Response.Billet;
using Newtonsoft.Json;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Client;
using Unimake.EBank.Solutions.Scopes.Security;
using Unimake.EBank.Solutions.Services.Billet.Request;
using Unimake.EBank.Solutions.Services.Billet.Response;
using static Newtonsoft.Json.JsonConvert;

namespace Unimake.EBank.Solutions.Services.Billet
{
    /// <summary>
    /// Serviço de registro de Billets
    /// </summary>
    public class BilletService
    {
        #region Public Methods

        /// <summary>
        /// Realiza a consulta de um boleto
        /// </summary>
        /// <param name="request">Requisição que será enviada ao servidor</param>
        /// <param name="authenticatedScope">Escopo de autenticação válido</param>
        /// <returns></returns>
        /// <exception cref="QueryInformationResponseException">Lançada quando ocorrer erros na validação ou consulta dos boletos</exception>
        public async Task<List<QueryResponse>> QueryAsync(QueryRequest request, AuthenticatedScope authenticatedScope)
        {
            request.Validate();

            var apiClient = new APIClient(authenticatedScope, "boleto/consultar");
            var response = await apiClient.PostAsync(SerializeObject(request));
            var json = await response.Content.ReadAsStringAsync();

            if(response.IsSuccessStatusCode)
            {
                return DeserializeObject<List<QueryResponse>>(json, new JsonSerializerSettings
                {
                    NullValueHandling = NullValueHandling.Ignore
                });
            }

            var errors = DeserializeObject<ErrorResponse>(json);
            System.Diagnostics.Debug.WriteLine(errors.Errors);
            throw new QueryInformationResponseException(errors.Errors.FirstOrDefault().Value?.FirstOrDefault() ?? "", (int)response.StatusCode);
        }

        /// <summary>
        /// Realiza o registro de um Billet de modo assíncrono
        /// </summary>
        /// <param name="request">Requisição que será enviada ao servidor</param>
        /// <param name="authenticatedScope">Escopo de autenticação válido</param>
        /// <returns></returns>
        /// <exception cref="RegisterResponseException">Lançada quando ocorrer erros na validação ou emissão dos Billets</exception>
        public async Task<RegisterResponse> RegisterAsync(RegisterRequest request, AuthenticatedScope authenticatedScope)
        {
            request.Validate();

            var apiClient = new APIClient(authenticatedScope, "boleto/registrar");
            var response = await apiClient.PostAsync(SerializeObject(request));
            var json = await response.Content.ReadAsStringAsync();

            if(response.IsSuccessStatusCode)
            {
                return DeserializeObject<RegisterResponse>(json);
            }

            var errors = DeserializeObject<ErrorResponse>(json);
            System.Diagnostics.Debug.WriteLine(errors.Errors);
            throw new RegisterResponseException(errors.Errors.FirstOrDefault().Value?.FirstOrDefault() ?? "", errors.Errors);
        }

        #endregion Public Methods
    }
}