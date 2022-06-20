using Unimake.EBank.Solutions.Services.Abstractions.Response;

namespace Unimake.EBank.Solutions.Services.Billet.Abstractions.Response
{
    /// <summary>
    /// Retorno das instruções dos boletos
    /// </summary>
    public abstract class PutInstructionsResponseBase : ResponseBase
    {
        #region Public Properties

        /// <summary>
        /// Código da mensagem de erro
        /// </summary>
        public string Codigo { get; set; }

        /// <summary>
        /// Mensagem de erro, se existir
        /// </summary>
        public string Mensagem { get; set; }

        /// <summary>
        /// Parâmetro com erro, se existir
        /// </summary>
        public string Parametro { get; set; }

        #endregion Public Properties

        #region Public Constructors

        /// <summary>
        /// </summary>
        public PutInstructionsResponseBase()
        {
        }

        #endregion Public Constructors
    }
}