using EBank.Solutions.Primitives.Debug;
using System;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;
using Unimake.Debug;
using Unimake.EBank.Solutions.Scopes.Security;

namespace Unimake.EBank.Solutions.Client
{
    internal class APIClient : IDisposable
    {
        #region Private Fields

        private readonly AuthenticatedScope authenticatedScope;
        private readonly HttpClient client = new HttpClient();

        #endregion Private Fields

        #region Private Properties

        private static DebugStateObject debugStateObject => DebugScope<DebugStateObject>.Instance?.ObjectState;

        #endregion Private Properties

        #region Private Methods

        private void EnsureAuthorization()
        {
            client.DefaultRequestHeaders.Remove("Authorization");
            client.DefaultRequestHeaders.Add("Authorization", $"{authenticatedScope.Type} {authenticatedScope.Token}");
        }

        private string PrepareURI(string queryString = "")
        {
            return $"{debugStateObject?.EBankServerUrl ?? $"https://ebank.solutions/api/v1/"}{Action}?{queryString}";
        }

        #endregion Private Methods

        #region Public Properties

        public string Action { get; }

        #endregion Public Properties

        #region Public Constructors

        public APIClient(AuthenticatedScope scope, string action)
        {
            authenticatedScope = scope ?? throw new ArgumentNullException(nameof(scope));
            Action = action;
        }

        #endregion Public Constructors

        #region Public Methods

        public void Dispose()
        {
            client.Dispose();
        }

        public async Task<HttpResponseMessage> GetAsync(string queryString)
        {
            EnsureAuthorization();
            return await client.GetAsync(PrepareURI(queryString));
        }

        public async Task<HttpResponseMessage> PostAsync(string json)
        {
            EnsureAuthorization();
            return await client.PostAsync(PrepareURI(), new StringContent(json, Encoding.UTF8, "application/json"));
        }

        #endregion Public Methods
    }
}