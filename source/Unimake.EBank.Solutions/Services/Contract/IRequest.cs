namespace Unimake.EBank.Solutions.Services.Contract
{
    /// <summary>
    /// Contrato de requisição padrão E-Bank
    /// </summary>
    public interface IRequest
    {
        #region Public Properties

        /// <summary>
        /// Se verdadeiro, será utilizado um servidor de testes
        /// </summary>
        bool Testing { get; set; }

        #endregion Public Properties
    }
}