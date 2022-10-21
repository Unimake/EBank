using EBank.Solutions.Primitives.Exceptions;
using System;
using System.Linq;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Client;
using Unimake.EBank.Solutions.Exceptions;
using Unimake.EBank.Solutions.Scopes.Security;
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

            var errors = ExceptionObject.FromJson(json);
            System.Diagnostics.Debug.WriteLine(errors.Message);

            if(typeof(TException) == typeof(ResponseException))
            {
                throw new ResponseException(errors.Message, (int)response.StatusCode);
            }

            var exType = typeof(TException);
            var constructor = (from ctor in exType.GetConstructors()
                               let ctorParams = ctor.GetParameters()
                               where ctorParams.Length > 1 &&
                                     ctorParams[1].ParameterType == typeof(int)
                               select ctor).FirstOrDefault();

            if(constructor != null)
            {
                throw Activator.CreateInstance(exType, new object[] { errors.Message, (int)response.StatusCode }) as EBankException;
            }

            throw Activator.CreateInstance(typeof(TException), new[] { errors.Message }) as EBankException;
        }

        #endregion Public Methods
    }
}