using EBank.Solutions.Primitives.Billet.Models;
using Newtonsoft.Json;
using System;
using System.Threading.Tasks;
using Unimake.AuthServer.Authentication;
using Unimake.AuthServer.Security.Scope;
using Unimake.Primitives.UDebug;
using Xunit.Abstractions;
using static Newtonsoft.Json.JsonConvert;

namespace Unimake.EBank.Solutions.Tests.Abstractions
{
    public abstract class TestBase : IDisposable
    {
        #region Private Fields

        private readonly ITestOutputHelper output;
        private JsonSerializerSettings _jsonSettings;
        private DebugScope<DebugStateObject> debugScope;

        #endregion Private Fields

        #region Private Properties

        private JsonSerializerSettings JsonSettings => _jsonSettings ??= _jsonSettings = new JsonSerializerSettings
        {
            Formatting = Formatting.Indented,
            MissingMemberHandling = MissingMemberHandling.Ignore,
            NullValueHandling = NullValueHandling.Include,
            ReferenceLoopHandling = ReferenceLoopHandling.Ignore
        };

        #endregion Private Properties

        #region Protected Properties

        protected static Beneficiario BeneficiarioDefault => new Beneficiario
        {
            Conta = new ContaCorrente
            {
                Agencia = "...",
                Banco = global::EBank.Solutions.Primitives.Enumerations.Banco.BancoDoBrasil,
                Numero = "..."
            },
            Inscricao = "...",
            Nome = "..."
        };

        #endregion Protected Properties

        #region Protected Methods

        protected static async Task<AuthenticatedScope> CreateAuthenticatedScopeAsync() => await Task.FromResult(new AuthenticatedScope(new AuthenticationRequest
        {
            // Você consegue realizar os testes de emissão de seus Billets com estas informações.
            // Mas para que seu Billet seja válido, deverá entrar em contato com a Unimake Software em http://www.unimake.com.br/
            // Este AppId e Secret foram criados apenas para testes.
            AppId = "...",
            Secret = "..."
        }));

        protected void StartServerDebugMode()
        {
#if DEBUG_UNIMAKE
            debugScope = new DebugScope<DebugStateObject>(new DebugStateObject
            {
                AuthServerUrl = "https://localhost:44386/api/auth/",
                AnotherServerUrl = "https://localhost:44341/api/v1/"
            });
#else
            debugScope = null;
#endif
        }

        #endregion Protected Methods

        #region Public Constructors

        public TestBase(ITestOutputHelper output) => this.output = output;

        #endregion Public Constructors

        #region Public Methods

        public void Dispose()
        {
            debugScope?.Dispose();
            GC.SuppressFinalize(this);
        }

        public void DumpAsJson(object value)
        {
            var text = SerializeObject(value, JsonSettings);
            output.WriteLine(text);
            System.Diagnostics.Debug.WriteLine(text, "EBankDebug");
        }

        public void WriteLine(string line)
        {
            output.WriteLine(line);
            System.Diagnostics.Debug.WriteLine(line, "EBankDebug");
        }

        #endregion Public Methods
    }
}