using System;
using System.Http;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;
using Unimake.AuthServer.Security.Scope;
using Unimake.EBank.Solutions.Services.Contract;
using Unimake.Primitives.UDebug;

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

        private async Task<HttpResponseMessage> PostAsync(string json, Unimake.EBank.Solutions.Services.Contract.IRequest request)
        {
            EnsureAuthorization();
            var response = await client.PostAsync(PrepareURI(request), new StringContent(json, Encoding.UTF8, "application/json"));
            return response;
        }

        private string PrepareURI(Unimake.EBank.Solutions.Services.Contract.IRequest request)
        {
            var uri = $"{debugStateObject?.AnotherServerUrl ?? $"https://unimake.app/ebank/api/v1/"}{Action}{ToQueryString(request)}";
            return uri;
        }

        private string ToQueryString(Unimake.EBank.Solutions.Services.Contract.IRequest request)
        {
            QueryString.AddOrUpdateValue("configurationId", request?.ConfigurationId ?? "");
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

        public async Task<HttpResponseMessage> GetAsync(Unimake.EBank.Solutions.Services.Contract.IRequest request)
        {
            EnsureAuthorization();
            return await client.GetAsync(PrepareURI(request));
        }

        public async Task<HttpResponseMessage> GetAsync()
        {
            EnsureAuthorization();
            return await client.GetAsync(PrepareURI(default));
        }

        public async Task<HttpResponseMessage> PostAsync(object param) => await PostAsync(Newtonsoft.Json.JsonConvert.SerializeObject(param), default);

        public async Task<HttpResponseMessage> PostAsync(IRequest request) => await PostAsync(Newtonsoft.Json.JsonConvert.SerializeObject(request), request);

        #endregion Public Methods
    }
}