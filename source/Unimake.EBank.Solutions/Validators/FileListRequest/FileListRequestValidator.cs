using System;
using Unimake.EBank.Solutions.Services.Abstractions.Request;
using Unimake.Validators.Exceptions;

/// <summary>
/// Valida a requisição do tipo arquivo antes de enviar para o servidor.
/// </summary>
public static class FileListRequestValidator
{
    #region Public Methods

    /// <summary>
    /// Valida a requisição do tipo arquivo antes de enviar para o servidor.
    /// </summary>
    public static void Validate(this FileRequestBase request)
    {
        if(request is null)
        {
            throw new ArgumentNullException(nameof(request));
        }

        request.Bank.ValidateRequiredField(nameof(request.Bank));
        request.StartDate.ValidateRequiredField("StartDate");

        if(!request.EndDate.IsValid())
        {
            request.EndDate = request.StartDate;
        }

        if(request.StartDate.DateDiff(request.EndDate).TotalDays > 7)
        {
            throw new ValidationException("O período de datas para pesquisa não pode ser maior que 7 dias.");
        }
    }

    #endregion Public Methods
}