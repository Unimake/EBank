using System;
using System.Collections.Concurrent;
using System.Threading.Tasks;
using Unimake.AuthServer.Authentication;
using Unimake.AuthServer.Authentication.Models;
using Unimake.EBank.Solutions.Scopes.Contract;
using Unimake.Threading;

namespace Unimake.EBank.Solutions.Scopes.Security
{
    /// <summary>
    /// Escopo de autenticação.
    /// <para>Utilizado para realizar as ações dentro do E-Bank, e garantir que o escopo está autenticado</para>
    /// </summary>
    public sealed class AuthenticatedScope : IScope
    {
        #region Private Fields

        private static readonly ConcurrentDictionary<string, AuthenticatedScope> contexts = new ConcurrentDictionary<string, AuthenticatedScope>();
        private AuthenticationRequest authRequest;

        #endregion Private Fields

        #region Private Destructors

        /// <summary>
        /// Destruir :)
        /// </summary>
        ~AuthenticatedScope() => Dispose(false);

        #endregion Private Destructors

        #region Private Methods

        private async Task<AuthenticationResponse> Authenticate()
        {
            var service = new AuthenticationService();
            var response = await service.AuthenticateAsync(authRequest);
            return response;
        }

        private void Dispose(bool disposing)
        {
            if(Disposed)
            {
                return;
            }

            Disposed = true;

            if(disposing)
            {
                //¯\_(ツ)_/¯ nada... por enquanto
            }

            try
            {
                _ = contexts.TryRemove(authRequest.AppId, out _);
            }
            catch
            {
                //¯\(°_o)/¯
            }
        }

        #endregion Private Methods

        #region Public Properties

        /// <summary>
        /// Se verdadeiro, este objeto foi descartado
        /// </summary>
        public bool Disposed { get; private set; }

        /// <summary>
        /// Token de autenticação
        /// </summary>
        public string Token { get; private set; }

        /// <summary>
        /// Tipo do token de autenticação
        /// </summary>
        public string Type { get; private set; }

        #endregion Public Properties

        #region Public Constructors

        /// <summary>
        /// Inicia o escopo de autenticação
        /// </summary>
        /// <param name="authRequest">Dados para autenticação</param>
        /// <exception cref="InvalidOperationException">Quando um escopo autenticado já existir</exception>
        public AuthenticatedScope(AuthenticationRequest authRequest)
        {
            this.authRequest = authRequest;

            // É para dar erro mesmo. Não é para criar o escopo se não autenticar
            var auth = AsyncHelper.RunSync(Authenticate);
            Type = auth.Type;
            Token = auth.Token;

            // Se o escopo já existir, é para dar erro também.
            _ = contexts.AddOrUpdate(authRequest.AppId, this, (key, value) => throw new InvalidOperationException("Scope already defined."));
        }

        #endregion Public Constructors

        #region Public Methods

        /// <summary>
        /// Diz que este escopo está completo.
        /// <para>Usar sempre que o escopo for concluído ou fora da declaração using</para>
        /// </summary>
        public void Complete()
        {
            Dispose(false);
        }

        /// <summary>
        /// Descarta este objeto
        /// </summary>
        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        void IScope.Dispose(bool disposing)
        {
            Dispose(disposing);
        }

        #endregion Public Methods
    }
}