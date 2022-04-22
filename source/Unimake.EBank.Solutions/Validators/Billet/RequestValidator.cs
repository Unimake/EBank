using System;
using Unimake.EBank.Solutions.Services.Billet.Abstract;

/// <summary>
/// Validador dos dados principais da requisição
/// </summary>
public static class RequestValidator
{
    #region Public Methods

    /// <summary>
    /// Valida os campos necessários para a requisição
    /// </summary>
    /// <param name="request">Requisição</param>
    /// <exception cref="ArgumentNullException">Lançada quando o parâmetro 'request' é nulo</exception>
    public static void Validate(this RequestBase request)
    {
        if(request is null)
        {
            throw new ArgumentNullException(nameof(request));
        }
    }

    #endregion Public Methods
}