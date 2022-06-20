namespace Unimake.EBank.Solutions.Services.Abstractions.Response
{
    /// <summary>
    /// Resposta para requisições do tipo arquivo CNAB de extrato, cobrança e pagamentos
    /// </summary>
    public abstract class FileResponseBase : ResponseBase
    {
        #region Public Properties

        /// <summary>
        /// Conteúdo do registro, se for JSON é um objeto, se for CNAB é o conteúdo do arquivo
        /// </summary>
        public object Content { get; set; }

        /// <summary>
        /// Identificador do registro
        /// </summary>
        public string Identifier { get; set; }

        #endregion Public Properties
    }
}