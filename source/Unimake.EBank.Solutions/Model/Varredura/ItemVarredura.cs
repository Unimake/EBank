using EBank.Solutions.Primitives.CNAB.CNAB240.Campo;
using EBank.Solutions.Primitives.CNAB.CNAB240.Contract;
using EBank.Solutions.Primitives.Enumerations;
using EBank.Solutions.Primitives.Enumerations.Billet;
using System;

namespace Unimake.EBank.Solutions.Model.Varredura
{
    /// <summary>
    /// Define um item de Varredura de resposta
    /// </summary>
    public sealed class ItemVarredura
    {
        #region Public Properties

        /// <summary>
        /// Agência do pagador
        /// </summary>
        public string AgenciaClientePagador { get; set; }

        /// <summary>
        /// Inscrição do beneficiário
        /// </summary>
        public string CNPJCPFBeneficiario { get; set; }

        /// <summary>
        /// Inscrição do pagador
        /// </summary>
        public string CNPJCPFPagador { get; set; }

        /// <summary>
        /// Banco
        /// </summary>
        public Banco CodigoBanco { get; set; }

        /// <summary>
        /// Código de barras, se houver, pode vir vazio
        /// </summary>
        public string CodigoBarras { get; set; }

        /// <summary>
        /// Número da conta do cliente pagador
        /// </summary>
        public string ContaClientePagador { get; set; }

        /// <summary>
        /// Dados de controle do arquivo CNAB que originou este registro
        /// </summary>
        public Controle Controle { get; set; }

        /// <summary>
        /// Data do desconto, se houver
        /// </summary>
        public DateTime? DataDesconto { get; set; }

        /// <summary>
        /// Data do segundo desconto, se houver
        /// </summary>
        public DateTime? DataDesconto2 { get; set; }

        /// <summary>
        /// Data do terceiro desconto, se houver
        /// </summary>
        public DateTime? DataDesconto3 { get; set; }

        /// <summary>
        /// Data de vencimento
        /// </summary>
        public DateTime? DataDeVencimento { get; set; }

        /// <summary>
        /// Data de emissão
        /// </summary>
        public DateTime? DataEmissao { get; set; }

        /// <summary>
        /// Data dos juros
        /// </summary>
        public DateTime? DataJuros { get; set; }

        /// <summary>
        /// Data limite para pagamento do lançamento
        /// </summary>
        public DateTime? DataLimitePagamento { get; set; }

        /// <summary>
        /// Data dda multa
        /// </summary>
        public DateTime? DataMulta { get; set; }

        /// <summary>
        /// Define a espécie do título
        /// </summary>
        public EspecieTitulo EspecieDocumento { get; set; }

        /// <summary>
        /// Código no banco para identificar o sacador
        /// </summary>
        public string IdentificacaoSacado { get; set; }

        /// <summary>
        /// Linha digitável, somente quando boleto
        /// </summary>
        public string LinhaDigtavel { get; set; }

        /// <summary>
        /// Nome do banco
        /// </summary>
        public string NomeBanco { get; set; }

        /// <summary>
        /// Nome do beneficiário
        /// </summary>
        public string NomeBeneficiario { get; set; }

        /// <summary>
        /// Nome do pagador
        /// </summary>
        public string NomePagador { get; set; }

        /// <summary>
        /// Nome do sacado
        /// </summary>
        public string NomeSacado { get; set; }

        /// <summary>
        /// Número do documento gerado pelo banco
        /// </summary>
        public string NumeroDocumento { get; set; }

        /// <summary>
        /// Campos de serviço do arquivo CNAB
        /// </summary>
        public ICampoServico Servico { get; }

        /// <summary>
        /// Tipo de inscrição do beneficiário
        /// </summary>
        public TipoDeInscricao TipoBeneficiario { get; set; }

        /// <summary>
        /// Tipo de desconto aplicado
        /// </summary>
        public TipoDesconto TipoDesconto { get; set; }

        /// <summary>
        /// Tipo de segundo desconto aplicado
        /// </summary>
        public TipoDesconto TipoDesconto2 { get; set; }

        /// <summary>
        /// Tipo de terceiro desconto aplicado
        /// </summary>
        public TipoDesconto TipoDesconto3 { get; set; }

        /// <summary>
        /// Tipo de inscrição do sacador
        /// </summary>
        public TipoDeInscricao TipoIdentificacaoSacado { get; set; }

        /// <summary>
        /// Tipo de juros
        /// </summary>
        public TipoJuros TipoJuros { get; set; }

        /// <summary>
        /// Tipos de multa
        /// </summary>
        public TipoMulta TipoMulta { get; set; }

        /// <summary>
        /// Tipo de inscrição do Pagador
        /// </summary>
        public TipoDeInscricao TipoPessoaPagador { get; set; }

        /// <summary>
        /// Valor do lançamento
        /// </summary>

        public decimal Valor { get; set; }

        /// <summary>
        /// Valor do abatimento, se existir
        /// </summary>

        public decimal ValorAbatimento { get; set; }

        /// <summary>
        /// Valor percentual do desconto
        /// </summary>
        public decimal ValorPercentualDesconto { get; set; }

        /// <summary>
        /// Valor percentual do segundo desconto
        /// </summary>
        public decimal ValorPercentualDesconto2 { get; set; }

        /// <summary>
        /// Valor percentual do terceiro desconto
        /// </summary>
        public decimal ValorPercentualDesconto3 { get; set; }

        /// <summary>
        /// Valor percentual dos juros
        /// </summary>
        public decimal ValorPercentualJuros { get; set; }

        /// <summary>
        /// Valor percentual das multas
        /// </summary>
        public decimal ValorPercentualMulta { get; set; }

        #endregion Public Properties
    }
}