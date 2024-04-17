using EBank.Solutions.Primitives.Exceptions;

namespace Unimake.EBank.Solutions.Exceptions
{
    /// <summary>
    /// Exceção lançada sempre que algum erro acontecer entre as requisições
    /// </summary>
    public class ResponseException : EBankPrimitiveException
    {
        #region Public Constructors

        /// <summary>
        /// Cria uma nova instância do erro
        /// </summary>
        /// <param name="message">Mensagem de erro</param>
        public ResponseException(string message)
            : base(message)
        {
        }

        /// <summary>
        /// Cria uma nova instância do erro
        /// </summary>
        /// <param name="message">Mensagem de erro</param>
        /// <param name="statusCode">Código do estado original da resposta </param>
        public ResponseException(string message, int statusCode)
            : base(message)
        {
            StatusCode = statusCode;
        }

        #endregion Public Constructors
    }
}