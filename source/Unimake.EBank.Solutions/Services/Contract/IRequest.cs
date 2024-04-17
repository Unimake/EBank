namespace Unimake.EBank.Solutions.Services.Contract
{
    /// <summary>
    /// Contrato de requisição padrão E-Bank
    /// </summary>
    public interface IRequest
    {
        #region Public Properties

        /// <summary>
        /// Se informado, a requisição irá tentar usar o identificador da configuração.
        /// </summary>
        string ConfigurationId { get; set; }

        /// <summary>
        /// Se verdadeiro, será utilizado um servidor de testes
        /// </summary>
        bool Testing { get; set; }

        #endregion Public Properties
    }
}