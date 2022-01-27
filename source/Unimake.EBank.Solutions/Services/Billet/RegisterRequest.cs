using EBank.Solutions.Primitives.Billet.Models;
using Unimake.EBank.Solutions.Services.Abstract;

namespace Unimake.EBank.Solutions.Services.Billet
{
    /// <summary>
    /// Requisição do serviço de registro do Billet
    /// </summary>
    public class RegisterRequest : RequestBase
    {
        #region Public Properties

        /// <summary>
        /// Dados do Billet que será registrado
        /// </summary>
        public Boleto Billet { get; set; }

        #endregion Public Properties
    }
}