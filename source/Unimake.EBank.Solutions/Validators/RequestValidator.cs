using System;
using Unimake.EBank.Solutions.Services.Abstract;

namespace Unimake.EBank.Solutions.Validators
{
    /// <summary>
    /// Validador dos dados principais da requisição
    /// </summary>
    public static class RequestValidator
    {
        #region Public Methods

        /// <summary>
        /// Valida os campos <see cref="RequestBase.AppId"/> e <see cref="RequestBase.Secret"/> foram informados.
        /// </summary>
        /// <param name="request">Requisição</param>
        /// <exception cref="ArgumentNullException">Lançada quando o parâmetro 'request' é nulo</exception>
        public static void Validate(this RequestBase request)
        {
            if(request is null)
            {
                throw new ArgumentNullException(nameof(request));
            }

            request.AppId.ValidateRequiredField(nameof(request.AppId));
            request.Secret.ValidateRequiredField(nameof(request.Secret));
        }

        #endregion Public Methods
    }
}