using EBank.Solutions.Primitives.Exceptions;
using System;
using System.Linq;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Client;
using Unimake.EBank.Solutions.Exceptions;
using Unimake.EBank.Solutions.Scopes.Security;
using Unimake.EBank.Solutions.Services.Billet.Response;
using static Newtonsoft.Json.JsonConvert;

namespace Unimake.EBank.Solutions.Services.Billet
{
    internal class BilletServiceClient<TRequest, TResponse, TException>
        where TRequest : Contract.IRequest
        where TResponse : class
        where TException : EBankException
    {
        #region Private Constructors

        private BilletServiceClient()
        {
        }

        #endregion Private Constructors

        #region Public Methods

        public static async Task<TResponse> RequestAsync(TRequest request, AuthenticatedScope authenticatedScope, string action)
        {
            request.Validate();

            var apiClient = new APIClient(authenticatedScope, $"boleto/{action}");
            var response = await apiClient.PostAsync(SerializeObject(request));
            var json = await response.Content.ReadAsStringAsync();

            if(response.IsSuccessStatusCode)
            {
                return DeserializeObject<TResponse>(json);
            }

            var errors = DeserializeObject<ErrorResponse>(json);
            System.Diagnostics.Debug.WriteLine(errors.Errors);

            var errorMessage = errors.Errors.FirstOrDefault().Value?.FirstOrDefault() ?? "";

            if(typeof(TException) == typeof(ResponseException))
            {
                throw new ResponseException(errorMessage, (int)response.StatusCode);
            }

            throw Activator.CreateInstance(typeof(TException), new[] { errorMessage }) as EBankException;
        }

        #endregion Public Methods
    }
}