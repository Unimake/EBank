using System;
using Unimake.EBank.Solutions.Services.Abstract;
using Unimake.EBank.Solutions.Services.Billet;

namespace Unimake.EBank.Solutions.Validators.Billet
{
    /// <summary>
    /// Validador para o registro dos Billets
    /// </summary>
    public static class RegisterValidator
    {
        #region Public Methods

        /// <summary>
        /// Realiza a validação do Billet antes de enviar.
        /// </summary>
        /// <param name="request"></param>
        /// <exception cref="ArgumentNullException">Lançada se o <see cref="RegisterRequest.Billet"/> for nulo.</exception>
        public static void Validate(this RegisterRequest request)
        {
            (request as RequestBase).Validate();

            if(request.Billet is null)
            {
                throw new ArgumentNullException(nameof(request.Billet));
            }
        }

        #endregion Public Methods
    }
}