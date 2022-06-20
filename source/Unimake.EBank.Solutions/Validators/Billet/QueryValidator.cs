using System;
using Unimake.EBank.Solutions.Services.Billet.Abstractions.Request;
using Unimake.EBank.Solutions.Services.Billet.Request;

/// <summary>
/// Realiza a validação dos dados da requisição de consulta do boleto
/// </summary>
public static class QueryValidator
{
    #region Public Methods

    /// <summary>
    /// Realiza a validação do Billet antes de enviar.
    /// </summary>
    /// <param name="request"></param>
    /// <exception cref="ArgumentNullException">Lançada se o <see cref="QueryRequest"/> for nulo.</exception>
    public static void Validate(this QueryRequest request)
    {
        if(request is null)
        {
            throw new ArgumentNullException(nameof(request));
        }

        (request as RequestBase).Validate();
    }

    #endregion Public Methods
}