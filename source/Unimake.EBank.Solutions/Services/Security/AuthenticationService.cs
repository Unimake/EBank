using System.Threading.Tasks;
using Unimake.AuthServer.Authentication;
using Unimake.EBank.Solutions.Scopes.Security;
using Unimake.EBank.Solutions.Services.Security.Contract;

namespace Unimake.EBank.Solutions.Services.Security
{
    /// <summary>
    /// Serviço de autenticação
    /// </summary>
    /// <inheritdoc cref="IAuthenticationService"/>
    public sealed class AuthenticationService : IAuthenticationService
    {
        #region Public Methods

        /// <inheritdoc cref="IAuthenticationService.AuthenticateAsync(AuthenticationRequest)"/>
        public async Task<AuthenticatedScope> AuthenticateAsync(AuthenticationRequest credentials)
        {
            await Task.CompletedTask;
            return new AuthenticatedScope(credentials);
        }

        #endregion Public Methods
    }
}