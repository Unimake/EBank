using EBank.Solutions.Primitives.Billet.Models;
using EBank.Solutions.Primitives.Contract.Request;
using Newtonsoft.Json;
using System;
using System.Threading.Tasks;
using Unimake.AuthServer.Security.Scope;
using Unimake.Primitives.Security.Credentials;
using Unimake.Primitives.UDebug;
using Xunit;
using Xunit.Abstractions;
using static Newtonsoft.Json.JsonConvert;

namespace Unimake.EBank.Solutions.Tests.Abstractions
{
    [Collection("e-Bank Tests")]
    public abstract class TestBase : IDisposable
    {
        #region Private Fields

        private readonly ITestOutputHelper output;
        private JsonSerializerSettings _jsonSettings;
        private DebugScope<DebugStateObject> debugScope;

        #endregion Private Fields

        #region Private Properties

        protected static Beneficiario BeneficiarioDefault => new()
        {
            Nome = "Unifake",  //Não é obrigatório
            Codigo = "000014340",
            Inscricao = "06117473000079",
            Conta = new ContaCorrente
            {
                Banco = global::EBank.Solutions.Primitives.Enumerations.Banco.Sicoob,
                Agencia = "4340",
                Numero = "00001"
            }
        };

        private JsonSerializerSettings JsonSettings => _jsonSettings ??= _jsonSettings = new JsonSerializerSettings
        {
            Formatting = Formatting.Indented,
            MissingMemberHandling = MissingMemberHandling.Ignore,
            NullValueHandling = NullValueHandling.Include,
            ReferenceLoopHandling = ReferenceLoopHandling.Ignore
        };

        #endregion Private Properties

        #region Private Methods

        private void StartServerDebugMode() =>
            debugScope = new DebugScope<DebugStateObject>(new DebugStateObject
            {
                AuthServerUrl = "https://auth.sandbox.unimake.software/api/auth/", // "https://unimake.app/auth/api/auth/"
                AnotherServerUrl = "https://ebank.sandbox.unimake.software/api/v1/" //"https://unimake.app/ebank/api/vi/"
            });

        #endregion Private Methods

        #region Protected Fields

        protected DateTime EndDate = DateTime.Now.Date;
        protected DateTime StartDate = DateTime.Now.AddDays(-5).Date;

        #endregion Protected Fields

        #region Protected Constructors

        protected TestBase(ITestOutputHelper output)
        {
            this.output = output;
            StartServerDebugMode();
        }

        #endregion Protected Constructors

        #region Protected Methods

        protected static async Task<AuthenticatedScope> CreateAuthenticatedScopeAsync() => await Task.FromResult(new AuthenticatedScope(new AuthenticationToken
        {
            // Você consegue realizar os testes de emissão de seus Billets com estas informações.
            // Mas para que seu Billet seja válido, deverá entrar em contato com a Unimake Software em http://www.unimake.com.br/
            // Este AppId e Secret foram criados apenas para testes.
            AppId = "fe5a17ecfe9c4fedac9ea1b7b7da63c7",
            Secret = "36bc3c9bbb9242f78b498aa9e868cafc"
        }));

        protected T CreateRequest<T>(Func<T> builder)
                    where T : class, IRequest, new()
        {
            var t = builder();

            t.ConfigurationId = "ZCKWGQ55LTDXKYYC";
            t.Testing = true;

            var pi = t.GetType().GetProperty(nameof(Beneficiario));

            if(pi != null &&
                pi.CanWrite)
            {
                pi.SetValue(t, BeneficiarioDefault);
            }

            return t;
        }

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
            System.Diagnostics.Debug.WriteLine(text, "EBank Debug");
        }

        public void WriteLine(string line)
        {
            output.WriteLine(line);
            System.Diagnostics.Debug.WriteLine(line, "EBank Debug");
        }

        #endregion Public Methods
    }
}