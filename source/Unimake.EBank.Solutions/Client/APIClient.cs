using EBank.Solutions.Primitives.Debug;
using System;
using System.Http;
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
        private QueryString _queryString;

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

        private async Task<HttpResponseMessage> PostAsync(string json)
        {
            EnsureAuthorization();
            return await client.PostAsync(PrepareURI(), new StringContent(json, Encoding.UTF8, "application/json"));
        }

        private string PrepareURI()
        {
            return $"{debugStateObject?.EBankServerUrl ?? $"https://unimake.app/ebank/api/v1/"}{Action}{ToQueryString()}";
        }

        private string ToQueryString()
        {
            if(QueryString == null)
            {
                return "";
            }
            return QueryString.ToString(urlEncodeValue: false);
        }

        #endregion Private Methods

        #region Public Properties

        public string Action { get; }
        public QueryString QueryString { get => _queryString ?? (_queryString = new QueryString()); }

        #endregion Public Properties

        #region Public Constructors

        public APIClient(AuthenticatedScope scope, string action, QueryString queryString = null)
        {
            authenticatedScope = scope ?? throw new ArgumentNullException(nameof(scope));
            Action = action;
            _queryString = queryString;
        }

        #endregion Public Constructors

        #region Public Methods

        public void Dispose()
        {
            client.Dispose();
        }

        public async Task<HttpResponseMessage> GetAsync()
        {
            EnsureAuthorization();
            return await client.GetAsync(PrepareURI());
        }

        public async Task<HttpResponseMessage> PostAsync(object param) => await PostAsync(Newtonsoft.Json.JsonConvert.SerializeObject(param));

        #endregion Public Methods
    }
}