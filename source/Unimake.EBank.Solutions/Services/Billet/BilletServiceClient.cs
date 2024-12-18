using EBank.Solutions.Primitives.Exceptions;
using System;
using System.Linq;
using System.Net.Http;
using System.Threading.Tasks;
using Unimake.AuthServer.Security.Scope;
using Unimake.EBank.Solutions.Client;
using Unimake.EBank.Solutions.Exceptions;
using Unimake.EBank.Solutions.Services.Billet.Request;
using Unimake.EBank.Solutions.Services.Billet.Response;
using static Newtonsoft.Json.JsonConvert;

namespace Unimake.EBank.Solutions.Services.Billet
{
    internal class BilletServiceClient<TRequest, TResponse, TException>
        where TRequest : Contract.IRequest
        where TResponse : class, new()
        where TException : EBankPrimitiveException
    {
        #region Private Constructors

        private BilletServiceClient()
        {
        }

        #endregion Private Constructors

        #region Internal Methods

        internal static Task<CancelResponse> RequestAsync(CancelRequest request, AuthenticatedScope authenticatedScope, string v) => throw new NotImplementedException();

        #endregion Internal Methods

        #region Public Methods

        public static async Task<TResponse> RequestAsync(TRequest request, AuthenticatedScope authenticatedScope, string action)
        {
            request.Validate();

            var apiClient = new APIClient(authenticatedScope, $"boleto/{action}");
            var response = await apiClient.PostAsync(request);
            var json = await response.ReadAsJsonAsync();

            if(response.IsSuccessStatusCode())
            {
                if(response.StatusCode == System.Net.HttpStatusCode.NoContent)
                {
                    return new TResponse();
                }

                return DeserializeObject<TResponse>(json);
            }

            var errors = ExceptionObject.FromJson(json);
            System.Diagnostics.Debug.WriteLine(errors.Message);

            if(typeof(TException) == typeof(ResponseException))
            {
                throw new ResponseException(errors.Message, response.StatusCode);
            }

            var exType = typeof(TException);
            var constructors = exType.GetConstructors();

            if(constructors != null)
            {
                var constructor = constructors.FirstOrDefault(c => c.GetParameters().Length == 2 &&
                                                                     c.GetParameters()[0].ParameterType == typeof(string) &&
                                                                    c.GetParameters()[1].ParameterType == typeof(int));

                if(constructor != null)
                {
                    throw Activator.CreateInstance(exType, new object[] { errors.Message, (int)response.StatusCode }) as EBankPrimitiveException;
                }

                constructor = constructors.FirstOrDefault(c => c.GetParameters().Length == 2 &&
                                                                     c.GetParameters()[0].ParameterType == typeof(string) &&
                                                                    c.GetParameters()[1].ParameterType == typeof(System.Net.HttpStatusCode));

                if(constructor != null)
                {
                    throw Activator.CreateInstance(exType, new object[] { errors.Message, response.StatusCode }) as EBankPrimitiveException;
                }

                constructor = constructors.FirstOrDefault(c => c.GetParameters().Length == 1 &&
                                                                     c.GetParameters()[0].ParameterType == typeof(string));

                if(constructor != null)
                {
                    throw Activator.CreateInstance(exType, new object[] { errors.Message }) as EBankPrimitiveException;
                }
            }

            throw new EBankPrimitiveException(errors.Message, response.StatusCode);
        }

        #endregion Public Methods
    }
}