using EBank.Solutions.Primitives.PDF.Models;
using Unimake.EBank.Solutions.Services.Abstractions.Response;

namespace Unimake.EBank.Solutions.Services.Billet.Response
{
    /// <summary>
    /// Resposta do serviço de registro de Billets.
    /// </summary>
    public class RegisterResponse : ResponseBase
    {
        #region Public Properties

        /// <summary>
        /// Representação do código de barras do boletos.
        /// </summary>
        /// <remarks>
        /// Não são todos os bancos que retornam esta informação, ela pode vir nula ou vazia
        /// </remarks>
        public string CodigoBarraNumerico { get; set; }

        /// <summary>
        /// Linha digitável.
        /// </summary>
        /// <example>34191.09008 00060.330289 91469.200009 7 86190000004588</example>
        public string LinhaDigitavel { get; set; }

        /// <summary>
        /// Número do documento no banco (nosso número).
        /// </summary>
        /// <example>00000603</example>
        public string NumeroNoBanco { get; set; }

        ///// <summary>
        ///// Código de barras.
        ///// </summary>
        //public string CodigoDeBarras { get; set; }

        /// <summary>
        /// Arquivo pdf do boleto em base 64.
        /// </summary>
        public PDFContent PDFContent { get; set; }

        #endregion Public Properties
    }
}