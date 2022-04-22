using System;
using System.Threading.Tasks;
using Unimake.AuthServer.Authentication;
using Unimake.EBank.Solutions.Scopes.Security;

namespace Unimake.EBank.Solutions.Services.Security.Contract
{
    /// <summary>
    /// Serviço de autenticação no E-Bank
    /// </summary>
    public interface IAuthenticationService
    {
        #region Public Methods

        /// <summary>
        /// Realiza a autenticação no servidor E-Bank e retorna um escopo de autenticação válido.
        /// </summary>
        /// <param name="credentials">Credenciais para autenticação no serviço E-Bank</param>
        /// <returns>Um escopo autenticado</returns>
        /// <exception cref="Exception">Exceções gerais</exception>
        /// <exception cref="InvalidOperationException">Quando um escopo autenticado já existir</exception>
        Task<AuthenticatedScope> AuthenticateAsync(AuthenticationRequest credentials);

        #endregion Public Methods
    }
}