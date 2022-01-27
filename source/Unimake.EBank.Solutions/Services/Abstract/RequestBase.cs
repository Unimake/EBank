using Unimake.EBank.Solutions.Services.Contract;

namespace Unimake.EBank.Solutions.Services.Abstract
{
    /// <summary>
    /// Abstração para requisições gerais
    /// </summary>
    public abstract class RequestBase : IRequest
    {
        #region Public Properties

        /// <summary>
        /// Identificador da aplicação
        /// </summary>
        public string AppId { get; set; }

        /// <summary>
        /// Segredo gerado no momento da criação da aplicação
        /// </summary>
        public string Secret { get; set; }

        #endregion Public Properties
    }
}