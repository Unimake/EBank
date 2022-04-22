using System;
using Unimake.AuthServer.Authentication;

/// <summary>
/// Faz a validação dos dados de autenticação
/// </summary>
public static class AuthenticationValidator
{
    #region Public Methods

    /// <summary>
    /// Valida os campos necessários para a autenticação
    /// </summary>
    /// <param name="request">Requisição</param>
    ///<exception cref="ArgumentException">lançada se o <see cref="AuthenticationRequest.AppId"/> ou <see cref="AuthenticationRequest.Secret"/> for nulo, vazio ou espaços em branco </exception>
    public static void Validate(this AuthenticationRequest request)
    {
        request.AppId.ValidateRequiredField(nameof(request.AppId));
        request.Secret.ValidateRequiredField(nameof(request.Secret));
    }

    #endregion Public Methods
}