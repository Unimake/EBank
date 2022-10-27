using EBank.Solutions.Primitives.Billet.Models;
using System.ComponentModel.DataAnnotations;

namespace Unimake.EBank.Solutions.Services.PIX.Request
{
    /// <summary>
    /// Dados de requisição do PIX
    /// </summary>
    public class PIXRequest
    {
        #region Public Properties

        /// <summary>
        /// Dados do beneficiário do PIX
        /// </summary>
        [Required]
        public Beneficiario Beneficiario { get; set; }

        #endregion Public Properties
    }
}