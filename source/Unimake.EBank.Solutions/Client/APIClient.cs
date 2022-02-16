using System;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Scopes.Security;

namespace Unimake.EBank.Solutions.Client
{
    internal class APIClient : IDisposable
    {
        #region Private Fields

        private readonly AuthenticatedScope authenticatedScope;
        private readonly HttpClient client = new HttpClient();

        #endregion Private Fields

        #region Private Methods
        private string PrepareURI() =>
#if DEBUG
            $"https://localhost:44382/api/v1/{Action}";

#else
            $"https://ebank.solutions/api/v1/{Action}";
#endif

        #endregion Private Methods

        #region Public Properties

        public string Action { get; }

        #endregion Public Properties

        #region Public Constructors

        public APIClient(AuthenticatedScope scope, string action)
        {
            authenticatedScope = scope;
            Action = action;
        }

        #endregion Public Constructors

        #region Public Methods

        public void Dispose()
        {
            client.Dispose();
        }

        public async Task<HttpResponseMessage> PostAsync(string json)
        {
            client.DefaultRequestHeaders.Add("Authorization", $"{authenticatedScope.Type} {authenticatedScope.Token}");
            return await client.PostAsync(PrepareURI(), new StringContent(json, Encoding.UTF8, "application/json"));
        }

        #endregion Public Methods
    }
}