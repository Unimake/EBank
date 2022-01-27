namespace Unimake.EBank.Solutions.Services.Contract
{
    /// <summary>
    /// Contrato de requisição padrão E-Bank
    /// </summary>
    public interface IRequest
    {
        #region Public Properties

        /// <summary>
        /// Identificador da aplicação
        /// </summary>
        string AppId { get; set; }

        /// <summary>
        /// Segredo gerado no momento da criação da aplicação
        /// </summary>
        string Secret { get; set; }

        #endregion Public Properties
    }
}