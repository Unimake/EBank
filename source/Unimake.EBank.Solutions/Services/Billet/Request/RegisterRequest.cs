using EBank.Solutions.Primitives.Billet.Models;
using Unimake.EBank.Solutions.Services.Contract;

namespace Unimake.EBank.Solutions.Services.Billet.Request
{
    /// <summary>
    /// Requisição do serviço de registro do Billet
    /// </summary>
    public class RegisterRequest : Boleto, Contract.IRequest
    {
        #region Public Constructors

        /// <summary>
        /// Inicia o objeto.
        /// <para>A propriedade <see cref="Boleto.Testing"/> sempre será true. Modifique para false caso vá utilizar no ambiente de produção</para>
        /// </summary>
        public RegisterRequest()
        {
            Testing = true;
        }

        #endregion Public Constructors
    }
}