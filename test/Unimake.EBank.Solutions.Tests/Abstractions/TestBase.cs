using EBank.Solutions.Primitives.Billet.Models;
using EBank.Solutions.Primitives.Debug;
using Newtonsoft.Json;
using System;
using System.Threading.Tasks;
using Unimake.AuthServer.Authentication;
using Unimake.AuthServer.Security.Scope;
using Unimake.Debug;
using Xunit.Abstractions;
using static Newtonsoft.Json.JsonConvert;
using AuthenticationService = Unimake.EBank.Solutions.Services.Security.AuthenticationService;

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
                Agencia = "<<AGENCIA>>",
                //Até o momento da criação deste teste, apenas o Sicoob estava homologado.
                Banco = global::EBank.Solutions.Primitives.Enumerations.Banco.Sicoob,
                Numero = "<<CONTA>>"
            },
            Inscricao = "<<CPF OU CNPJ>>",
            Nome = "<<NOME>>"
        };

        #endregion Protected Properties

        #region Protected Methods

        protected static async Task<AuthenticatedScope> CreateAuthenticatedScopeAsync() => await new AuthenticationService().AuthenticateAsync(new AuthenticationRequest
        {
            // Você consegue realizar os testes de emissão de seus Billets com estas informações.
            // Mas para que seu Billet seja válido, deverá entrar em contato com a Unimake Software em http://www.unimake.com.br/
            // Este AppId e Secret foram criados apenas para testes.
            AppId = "61a73f4735ad4993959e28e2b0e4552a",
            Secret = "35955532e0c54517bc9d7e900b61b8d3"
        });

#if DEBUG_UNIMAKE
        protected void StartServerDebugMode() => debugScope = new DebugScope<DebugStateObject>(new DebugStateObject
        {
            AuthServerUrl = "https://localhost:44386/api/auth/",
            EBankServerUrl = "https://localhost:44341/api/v1/"
        });
#else

        protected void StartServerDebugMode() => debugScope = null;

#endif

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

        #endregion Public Methods
    }
}