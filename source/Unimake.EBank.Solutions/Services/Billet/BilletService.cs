using EBank.Solutions.Primitives.Exceptions.Response.Billet;
using System.Threading.Tasks;
using Unimake.AuthServer.Authentication;
using Unimake.EBank.Solutions.Client;
using Unimake.EBank.Solutions.Scopes.Security;
using Unimake.EBank.Solutions.Validators.Billet;
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
        /// Realiza o registro de um Billet de modo assíncrono
        /// </summary>
        /// <param name="request">Requisição que será enviada ao servidor</param>
        /// <returns></returns>
        /// <exception cref="RegisterResponseException">Lançada quando ocorrer erros na validação ou emissão dos Billets</exception>
        public async Task<RegisterResponse> RegisterAsync(RegisterRequest request)
        {
            request.Validate();

            using(var authScope = new AuthenticatedScope(new AuthenticationRequest
            {
                AppId = request.AppId,
                Secret = request.Secret
            }))
            {
                var apiClient = new APIClient(authScope, "boleto/registrar");
                var response = await apiClient.PostAsync(SerializeObject(request.Billet));
                var json = await response.Content.ReadAsStringAsync();

                if(response.IsSuccessStatusCode)
                {
                    return DeserializeObject<RegisterResponse>(json);
                }

                var errors = DeserializeObject<RegisterErrorResponse>(json);
                throw new RegisterResponseException(errors.Errors);
            }
        }

        #endregion Public Methods
    }
}