using Unimake.EBank.Solutions.Services.Billet.Abstractions.Request;

namespace Unimake.EBank.Solutions.Services.Billet.Request
{
    /// <summary>
    /// Dados da requisição do cancelamento do boleto
    /// </summary>
    public class CancelRequest : RequestBase
    {
        #region Public Properties

        /// <summary>
        /// Número do documento no banco (nosso número).
        /// <para>Regras para validação:</para>
        /// <para>Itaú (314): -- Tamanho: 8 caracteres (sem informar o dígito verificador) -- Obrigatório: Sim</para>
        /// <para>Banco do Brasil: -- Tamanho: 10 caracteres (sem informar o dígito verificador) -- Obrigatório: Sim</para>
        /// <para>Caixa Econômica Federal: -- Tamanho: 17 caracteres -- Obrigatório: Nãos</para>
        /// <para>Bradesco: -- Tamanho: 11 caracteres -- Obrigatório: Não</para>
        /// <para>Santander: -- Tamanho: 12 caracteres (informando o dígito verificador) -- Obrigatório: Não</para>
        /// <para>Sicredi: -- Tamanho: 9 caracteres (informando o dígito verificador) -- Obrigatório: Não</para>
        /// </summary>
        public string NumeroNoBanco { get; set; }

        #endregion Public Properties
    }
}