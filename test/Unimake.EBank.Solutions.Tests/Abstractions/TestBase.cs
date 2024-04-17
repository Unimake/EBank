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

        #region Private Methods

        private void StartServerDebugMode()
        {
#if DEBUG_UNIMAKE
            debugScope = new DebugScope<DebugStateObject>(new DebugStateObject
            {
                AuthServerUrl = "http://homolog.unimake.software:54469/api/auth/",
                AnotherServerUrl = "http://localhost:58200/api/v1/"
            });
#else
            debugScope = null;
#endif
        }

        #endregion Private Methods

        #region Protected Properties

        protected static Beneficiario BeneficiarioDefault => new()
        {
            Conta = new ContaCorrente
            {
                Agencia = "4340",
                Banco = global::EBank.Solutions.Primitives.Enumerations.Banco.Sicoob,
                Numero = "123365"
            },
            Inscricao = "<<?>>",
            Nome = "<?>>",
            Codigo = "<?>>"
        };

        #endregion Protected Properties

        #region Protected Constructors

        protected TestBase(ITestOutputHelper output)
        {
            this.output = output;
            StartServerDebugMode();
        }

        #endregion Protected Constructors

        #region Protected Methods

        protected static async Task<AuthenticatedScope> CreateAuthenticatedScopeAsync() => await Task.FromResult(new AuthenticatedScope(new AuthenticationRequest
        {
            // Você consegue realizar os testes de emissão de seus Billets com estas informações.
            // Mas para que seu Billet seja válido, deverá entrar em contato com a Unimake Software em http://www.unimake.com.br/
            // Este AppId e Secret foram criados apenas para testes.
            AppId = "<?>>",
            Secret = "<?>>"
        }));

        #endregion Protected Methods

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