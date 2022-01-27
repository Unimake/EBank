namespace System
{
    /// <summary>
    /// Extensões de validação de valores
    /// </summary>
    public static class ValidationExtensions
    {
        #region Public Methods

        /// <summary>
        /// Valida se o comprimento máximo do campo foi excedido
        /// </summary>
        /// <param name="text">Valor para validação</param>
        /// <param name="fieldName">Nome do campo a qual o valor está vinculado</param>
        /// <param name="maximumLength">Comprimento permitido para o campo</param>
        /// <exception cref="ArgumentOutOfRangeException">Lançada quando o campo exceder o tamanho.</exception>
        public static void ValidateMaximumLengthExceeded(this string text, string fieldName, int maximumLength)
        {
            if((text?.Length ?? 0) > maximumLength)
            {
                throw new ArgumentOutOfRangeException(fieldName, text, $"The '{fieldName}' value cannot be longer than '{maximumLength}' in length.");
            }
        }

        /// <summary>
        /// Valida se o comprimento mínimo do campo é válido
        /// </summary>
        /// <param name="text">Valor para validação</param>
        /// <param name="fieldName">Nome do campo a qual o valor está vinculado</param>
        /// <param name="minimumLength">Comprimento permitido para o campo</param>
        /// <exception cref="ArgumentOutOfRangeException">Lançada quando o campo exceder o tamanho.</exception>
        public static void ValidateMinimumLengthRequired(this string text, string fieldName, int minimumLength)
        {
            if((text?.Length ?? 0) < minimumLength)
            {
                throw new ArgumentOutOfRangeException(fieldName, text, $"The '{fieldName}' value cannot be less than '{minimumLength}' in length.");
            }
        }

        /// <summary>
        /// Valida se o texto informado é válido.
        /// <para>Se o texto for vazio, nulo ou composto apenas de espaços em branco, a exceção <see cref="ArgumentException"/> será lançada.</para>
        /// </summary>
        /// <param name="text">Texto para validação</param>
        /// <param name="fieldName">Nome do campo a qual o valor está vinculado</param>
        /// <exception cref="ArgumentException">Lançada se o texto for vazio, nulo ou composto apenas de espaços em branco.</exception>
        public static void ValidateRequiredField(this string text, string fieldName)
        {
            if(string.IsNullOrWhiteSpace(fieldName))
            {
                throw new ArgumentException($"The '{fieldName}' value cannot be null or whitespace.", fieldName);
            }
        }

        /// <summary>
        /// Valida se o valor do enumerador é válido.
        /// <para>Considera como válidos, valores existentes no enumerador TEnum. </para>
        /// </summary>
        /// <param name="value">Valor para validação</param>
        /// <param name="fieldName">Nome do campo a qual o valor está vinculado</param>
        /// <exception cref="ArgumentException">Lançada se o valor não existir no enumerador.</exception>
        public static void ValidateRequiredField<TEnum>(this TEnum value, string fieldName) where TEnum : Enum
        {
            if(value == null || !Enum.IsDefined(typeof(TEnum), value))
            {
                throw new ArgumentException($"The '{value}' value cannot be found in '{typeof(TEnum)}' values.", fieldName);
            }
        }

        /// <summary>
        /// Valida se o valor informado for diferente de 'default'/>
        /// </summary>
        /// <param name="value">Valor para validação</param>
        /// <param name="fieldName">Nome do campo a qual o valor está vinculado</param>
        /// <exception cref="ArgumentException">Lançada se o valor for igual a default</exception>
        public static void ValidateRequiredField(this float value, string fieldName)
        {
            if(value == default)
            {
                throw new ArgumentException($"The '{fieldName}' value cannot be default.", fieldName);
            }
        }

        /// <summary>
        /// Valida se a data informada é válida.
        /// </summary>
        /// <param name="value">Valor para validação</param>
        /// <param name="fieldName">Nome do campo a qual o valor está vinculado</param>
        /// <exception cref="ArgumentException">Lançada se o valor for igual a default</exception>
        public static void ValidateRequiredField(this DateTime value, string fieldName)
        {
            if(value == default)
            {
                throw new ArgumentException($"The '{fieldName}' value cannot be default.", fieldName);
            }
        }

        #endregion Public Methods
    }
}