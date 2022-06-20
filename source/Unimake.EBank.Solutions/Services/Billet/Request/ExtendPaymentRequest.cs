using System;
using System.Collections.Generic;
using System.Text;
using Unimake.EBank.Solutions.Services.Billet.Abstractions.Request;

namespace Unimake.EBank.Solutions.Services.Billet.Request
{
    /// <summary>
    /// Dados para alteração do vencimento
    /// </summary>
    public class ExtendPaymentRequest : RequestBase
    {
        #region Public Properties

        /// <summary>
        /// Nova data de vencimento do boleto
        /// </summary>
        public DateTime DataVencimento { get; set; }

        /// <summary>
        /// Número do documento no banco (nosso número).
        /// </summary>
        public string NumeroNoBanco { get; set; }

        #endregion Public Properties
    }
}