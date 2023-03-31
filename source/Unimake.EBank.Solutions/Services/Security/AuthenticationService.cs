using System.Runtime.InteropServices;
using System.Threading.Tasks;
using Unimake.AuthServer.Authentication;
using Unimake.AuthServer.Security.Scope;
using Unimake.EBank.Solutions.Resolver;
using Unimake.EBank.Solutions.Services.Security.Contract;

namespace Unimake.EBank.Solutions.Services.Security
{
    /// <summary>
    /// Serviço de autenticação
    /// </summary>
    /// <inheritdoc cref="IAuthenticationService"/>
    public sealed class AuthenticationService : IAuthenticationService
    {
        #region Public Constructors

        /// <summary>
        /// Instancia um novo objeto
        /// </summary>
        public AuthenticationService()
        {
            System.AppDomain.CurrentDomain.ResourceResolve += AssemblyResolver.AssemblyResolve;
        }

        #endregion Public Constructors

        #region Public Methods

        /// <summary>
        /// Realiza a autenticação e retorna o escopo autenticado, se válido
        /// </summary>
        /// <param name="appId">Identificador da aplicação</param>
        /// <param name="secret">Segredo</param>
        /// <returns></returns>
        public AuthenticatedScope Authenticate(string appId, string secret)
        {
            return new AuthenticatedScope(new AuthenticationRequest
            {
                AppId = appId,
                Secret = secret
            });
        }

        /// <inheritdoc cref="IAuthenticationService.AuthenticateAsync(AuthenticationRequest)"/>
        [ComVisible(false)]
        public async Task<AuthenticatedScope> AuthenticateAsync(AuthenticationRequest credentials)
        {
            await Task.CompletedTask;
            return new AuthenticatedScope(credentials);
        }

        #endregion Public Methods
    }
}