using Unimake.EBank.Solutions.Services.Billet.Abstractions.Request;

namespace Unimake.EBank.Solutions.Services.Billet.Request
{
    /// <summary>
    /// <inheritdoc cref="InformPaymentRequest"/>
    /// </summary>
    public class InformPaymentRequest : RequestBase
    {
        #region Public Properties

        /// <summary>
        /// Número do documento no banco (nosso número).
        /// </summary>
        public string NumeroNoBanco { get; set; }

        #endregion Public Properties
    }
}