using EBank.Solutions.Primitives.Billet.Models;

namespace Unimake.EBank.Solutions.Services.Billet.Contract
{
    /// <summary>
    /// Dados para requisição dos serviços com base em boletos
    /// </summary>
    public interface IRequest : Services.Contract.IRequest
    {
        #region Public Properties

        /// <summary>
        /// Dados do beneficiário
        /// </summary>
        Beneficiario Beneficiario { get; set; }

        #endregion Public Properties
    }
}