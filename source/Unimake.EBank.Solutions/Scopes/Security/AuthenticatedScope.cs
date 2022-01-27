using System;
using System.Collections.Concurrent;
using System.Threading.Tasks;
using Unimake.AuthServer.Authentication;
using Unimake.AuthServer.Authentication.Models;
using Unimake.EBank.Solutions.Scopes.Contract;

namespace Unimake.EBank.Solutions.Scopes.Security
{
    internal class AuthenticatedScope : IScope
    {
        #region Private Fields

        private static readonly ConcurrentDictionary<string, AuthenticatedScope> contexts = new ConcurrentDictionary<string, AuthenticatedScope>();
        private AuthenticationRequest authRequest;

        #endregion Private Fields

        #region Private Destructors

        ~AuthenticatedScope() => Dispose(false);

        #endregion Private Destructors

        #region Private Methods

        private async Task<AuthenticationResponse> Authenticate()
        {
            var service = new AuthenticationService();
            var response = await service.AuthenticateAsync(authRequest);
            return response;
        }

        #endregion Private Methods

        #region Public Properties

        public bool Disposed { get; private set; }
        public string Token { get; private set; }
        public string Type { get; private set; }

        #endregion Public Properties

        #region Public Constructors

        public AuthenticatedScope(AuthenticationRequest authRequest)
        {
            this.authRequest = authRequest;

            // É para dar erro mesmo. Não é para criar o escopo se não autenticar
            var auth = AsyncHelper.RunSync(Authenticate);
            Type = auth.Type;
            Token = auth.Token;

            // Se o escopo já existir, é para dar erro também.
            contexts.AddOrUpdate(authRequest.AppId, this, (key, value) => throw new InvalidOperationException("Scope already defined."));
        }

        #endregion Public Constructors

        #region Public Methods

        public void Complete()
        {
            Dispose(false);
        }

        public void Dispose(bool disposing)
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
                contexts.TryRemove(authRequest.AppId, out _);
            }
            catch
            {
                //¯\(°_o)/¯
            }
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);  
        }

        #endregion Public Methods
    }
}