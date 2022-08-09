using EBank.Solutions.Primitives.CNAB;
using EBank.Solutions.Primitives.CNAB.CNAB240.Campo;
using Newtonsoft.Json;
using System;

namespace Unimake.EBank.Solutions.Model.Extrato
{
    /// <summary>
    /// Define um item de extrato de resposta
    /// </summary>
    public sealed class ItemExtrato
    {
        #region Public Properties

        /// <summary>
        /// Complemento do lançamento
        /// </summary>
        [JsonProperty("ComplementoLancamento")]
        public string ComplementoLancamento { get; set; }

        /// <summary>
        /// Origem do arquivo
        /// </summary>
        [JsonProperty("Controle")]
        public Controle Controle { get; set; }

        /// <summary>
        /// Data de efetivação do Lançamento.
        /// </summary>
        [JsonProperty("DataContabil")]
        public DateTime DataContabil { get; set; }

        /// <summary>
        /// Cliente que firmou o convênio de prestação de serviços
        /// </summary>
        [JsonProperty("Empresa")]
        public Empresa85 Empresa { get; set; }

        /// <summary>
        /// Código adotado pela FEBRABAN para identificação de Lançamentos desobrigados de recolhimento do CPMF.
        /// <br/>Domínio:
        /// <br/>'S' = Isento
        /// <br/>'N' = Não Isento
        /// </summary>
        [JsonProperty("IdentificacaoIsencaoCPMF")]
        public IsencaoCPMF? IdentificacaoIsencaoCPMF { get; set; }

        /// <summary>
        /// Lançamento que deu origem a este pagamento
        /// </summary>
        [JsonProperty("Lancamento")]
        public Lancamento Lancamento { get; set; }

        /// <summary>
        /// Natureza do lançamento
        /// </summary>
        [JsonProperty("Natureza")]
        public string Natureza { get; set; }

        /// <summary>
        /// Detalhes do serviço
        /// </summary>
        [JsonProperty("Servico")]
        public ServicoDoDetalhe6 Servico { get; set; }

        /// <summary>
        /// Código adotado pela FEBRABAN para identificar a padronização a ser utilizada no complemento.
        /// <br/>Domínio:
        /// <br/>'00' = Sem Informação do Complemento do Lançamento
        /// <br/>'01' = Identificação da Origem do Lançamento
        /// </summary>
        [JsonProperty("TipoComplemento")]
        public int TipoComplemento { get; set; }

        #endregion Public Properties
    }
}