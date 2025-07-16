using EBank.Solutions.Primitives.Extrato.Request;
using EBank.Solutions.Primitives.Extrato.Response;
using System;
using System.Threading.Tasks;
using Unimake.AuthServer.Security.Scope;
using Unimake.EBank.Solutions.Client;

namespace Unimake.EBank.Solutions.Services.Extrato
{
    /// <summary>
    /// Serviço de consulta de extrato
    /// </summary>
    public class ExtratoService
    {
        #region Public Methods

        /// <summary>
        /// Retorna o extrato de uma conta bancária, com base no período informado.
        /// </summary>
        /// <param name="request">Requisição</param>
        /// <param name="authenticatedScope">Escopo autenticado</param>
        /// <returns></returns>
        public async Task<ExtratoResponse> GetExtratoAsync(ExtratoRequest request, AuthenticatedScope authenticatedScope)
        {
            if(request is null)
            {
                throw new ArgumentNullException(nameof(request), "A requisição do extrato não pode ser nula.");
            }

            if(authenticatedScope is null)
            {
                throw new ArgumentNullException(nameof(authenticatedScope), "O escopo autenticado não pode ser nulo.");
            }

            request.Agencia.ValidateRequiredIfNull(nameof(request.Agencia));
            request.Conta.ValidateRequiredIfNull(nameof(request.Conta));

            if(request.Banco == global::EBank.Solutions.Primitives.Enumerations.Banco.Desconhecido)
            {
                throw new ArgumentException($"O banco informado; {request.Banco}; não é válido.", nameof(request.Banco));
            }

            return await new APIClient(authenticatedScope, $"extrato/{request.Banco}/" +
                                                   $"{request.Agencia}/" +
                                                   $"{request.Conta}")
            {
                QueryString = {
                    { nameof(request.Testing), request.Testing },
                    { nameof(request.EndDate), request.EndDate.FormatDate("yyyy-MM-dd") },
                    { nameof(request.StartDate), request.StartDate.FormatDate("yyyy-MM-dd") }
                }
            }.GetAsync<ExtratoResponse>(request);
        }

        #endregion Public Methods
    }
}