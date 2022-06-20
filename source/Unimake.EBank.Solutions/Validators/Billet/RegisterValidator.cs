using System;
using Unimake.EBank.Solutions.Services.Billet.Request;

/// <summary>
/// Validador para o registro dos Billets
/// </summary>
public static class RegisterValidator
{
    #region Public Methods

    /// <summary>
    /// Realiza a validação do Billet antes de enviar.
    /// </summary>
    /// <param name="request"></param>
    /// <exception cref="ArgumentNullException">Lançada se o <see cref="RegisterRequest"/> for nulo.</exception>
    public static void Validate(this Unimake.EBank.Solutions.Services.Billet.Contract.IRequest request)
    {
        if(request is null)
        {
            throw new ArgumentNullException(nameof(request));
        }
    }

    #endregion Public Methods
}