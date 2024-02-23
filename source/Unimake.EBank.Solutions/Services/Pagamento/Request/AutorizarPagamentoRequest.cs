using EBank.Solutions.Primitives.CNAB.CNAB240;
using EBank.Solutions.Primitives.CNAB.CNAB240.Campo;
using EBank.Solutions.Primitives.Enumerations.CNAB;
using System;
using System.ComponentModel.DataAnnotations;
using Unimake.EBank.Solutions.Services.Abstractions.Request;
using Beneficiario = EBank.Solutions.Primitives.Billet.Models.Beneficiario;
using Favorecido = EBank.Solutions.Primitives.Billet.Models.Favorecido;

namespace Unimake.EBank.Solutions.Services.Pagamento.Request
{
    /// <summary>
    /// Dados para autorização do pagamento
    /// </summary>
    public class AutorizarPagamentoRequest : RequestBase
    {
        #region Public Properties

        /// <summary>
        /// Aviso ao Favorecido P006
        /// Posição: 230 até 230 (1)
        /// </summary>
        public string AvisoAoFavorecido { get; set; }

        /// <summary>
        /// Dados sobre o pagamento a ser efetuado.
        /// Dados sobre o pagamento a ser efetuado.
        /// Código de Barras G063 (44) +
        /// Nome do Beneficiário G013 (30) +
        /// Data do Vencimento (Nominal) G044 (8) +
        /// Valor do Título (Nominal) G042 (13,2) +
        /// Valor do Desconto + Abatimento L002 (13,2) +
        /// Valor da Mora + Multa L003 (13,2) +
        /// Data do Pagamento P009 (8) +
        /// Valor do Pagamento P010 (13,2) +
        /// Quantidade da Moeda G041 (10,5) +
        /// Nº do Docto Atribuído pela Empresa G064 (20)
        /// Posição: 18 até 202 (185)
        /// </summary>
        public Beneficiario Beneficiario { get; set; }

        /// <summary>
        /// Código de barras quando o pagamento for realizado por código de barras.
        /// </summary>
        public string CodigoBarras { get; set; }

        /// <summary>
        ///  A câmara pela qual transitará a transferência também poderá ser identificada a
        ///  partir do campo P001, “Código da Câmara Centralizadora”, no registro detalhe,
        ///  segmento “A”, a critério de cada banco, com preenchimento, a saber:
        ///  <br/>Forma Lançamento Código da Câmara Centralizadora
        ///  <br/>03 - 700
        ///  <br/>03 - 710 - DOC Mesma Titularidade (¹)
        ///  <br/>41 - 018
        ///  <br/>43 - 810 - TED Mesma Titularidade (¹)
        ///  <br/><br/>(¹- Código exclusivo para o banco Bradesco/Itaú)
        /// </summary>
        public CodigoDaCamaraCentralizadora CodigoDaCamaraCentralizadora { get; set; }

        /// <summary>
        /// Código da Instrução para Movimento G061
        /// Posição: 16 até 17 (2)
        /// </summary>
        [Required]
        public CodigoDaInstrucaoParaMovimento CodigoDaInstrucaoParaMovimento { get; set; }

        /// <summary>
        /// Complemento Tipo de Serviço P005
        /// Posição: 218 até 219 (2)
        /// </summary>
        public string CodigoDocumento { get; set; }

        /// <summary>
        /// Filial Destino NX37
        /// Posição: 226 até 228 (3)
        /// </summary>
        public string CodigoFilialDestino { get; set; }

        /// <summary>
        /// Código finalidade da TED/CC P011
        /// Posição: 220 até 224 (5)
        /// </summary>
        public string CodigoFinalidadeTED { get; set; }

        /// <summary>
        /// Data Real da Efetivação do Pagamento P003
        /// Posição: 155 até 162 (8)
        /// </summary>
        public DateTime DataEfetivacaoPagamento { get; set; }

        /// <summary>
        /// Data do Pagamento P009
        /// Posição: 94 até 101 (8)
        /// </summary>
        public DateTime DataPagamento { get; set; }

        /// <summary>
        /// Data do Vencimento (Nominal) G0044
        /// Posição: 128 até 135 (8)
        /// </summary>
        public DateTime DataVencimento { get; set; }

        /// <summary>
        /// Logradouro G032 (30) +
        /// Número do Local G032 (5) +
        /// Complemento G032 (15) +
        /// Cidade G033 (20) +
        /// CEP G034 (5) +
        /// Complemento CEP G035 (3) +
        /// UF G036 (2)
        /// Tamanho: 80
        /// </summary>
        [Required]
        public EnderecoEmpresa EnderecoEmpresa { get; set; }

        /// <summary>
        /// Tipo de Inscrição do Favorecido G005 (1) +
        /// Número de Inscrição do Favorecido G006 (14) +
        /// Logradouro G032 (30) +
        /// Número do Local G032 (5) +
        /// Complemento G032 (15) +
        /// Bairro G032 (15) +
        /// Cidade G033 (20) +
        /// CEP G034 (5) +
        /// Complemento CEP G035 (3) +
        /// UF G036 (2)
        /// Tamanho: 110
        /// </summary>
        [Required]
        public Favorecido Favorecido { get; set; }

        /// <summary>
        /// Define a forma de pagamento dos títulos.
        /// <para>Esta forma de pagamento interfere na geração dos segmentos. </para>
        /// <para>Veja a tabela abaixo:</para>
        /// <br/>• Forma de lançamento Segmentos
        /// <br/>• Pagamento através de Crédito em Conta Corrente,
        /// <br/>• Cheque,
        /// <br/>• Ordem de Pagamento,
        /// <br/>• DOC
        /// <br/>• Pagamento com Autenticação.
        /// <para>A(Obrigatório)
        /// <br/>B(Opcional)
        /// <br/>C(Opcional)</para>
        /// <br/>
        /// <br/>• Pagamento de Títulos de Cobrança
        /// <para>J(Obrigatório)
        /// <br/>J-52(Opcional)
        /// <br/>K(Opcional)</para>
        /// <br/>
        /// <br/>• Pagamento de Contas e Tributos com Código de Barras
        /// <para>O(Obrigatório)
        /// <br/>W* (Opcional)
        /// <br/>Z(Opcional)
        /// <br/>*obrigatório para o pagamento de FGTS, convênios 0181 e 0182 e para o pagamento de GRU, exclusivo Banco do Brasil.</para>
        /// <br/>
        /// <br/>• Pagamento de Tributos sem Código de Barras N(Obrigatório)
        /// <br/><para>B(Opcional)
        /// <br/>W(Opcional)
        /// <br/>Z(Opcional)</para>
        /// <br/>
        /// <br/>• Consulta de Tributos a Pagar.
        /// <br/><para>A utilização desse serviço deverá ser previamente acordada com o banco.N(Obrigatório)</para>
        /// <br/></summary>
        [Required]
        public FormaDeLancamento FormaDePagamento { get; set; }

        /// <summary>
        /// Outras Informações – Vide Formatação em G031 para identificação de Deposito Judicial e Pagamento Salário de servidores pelo SIAPE G031
        /// Posição: 178 até 217 (40)
        /// </summary>
        public string Informacao2 { get; set; }

        /// <summary>
        /// Número sequencial para identificar univocamente um lote de serviço. Criado e controlado pelo responsável pela geração magnética dos dados contidos no arquivo
        /// <br/>G002
        /// </summary>
        public short Lote { get; set; }

        /// <summary>
        /// Nº do Doc. Atribuído pelo Banco G043
        /// Posição: 135 até 154 (20)
        /// </summary>
        [Required]
        public string NossoNumero { get; set; }

        /// <summary>
        /// Número NF/Fatura e Duplicata NX12
        /// Posição: 231 até 240 (10)
        /// </summary>
        public string NumeroDocumento { get; set; }

        /// <summary>
        /// Quantidade de Moeda G041
        /// Posição: 105 até 119 (10,5)
        /// </summary>
        public decimal QuantidadeMoeda { get; set; }

        /// <summary>
        /// Nº do Docto Atribuído pela Empresa G064 (20)
        /// </summary>
        public string ReferenciaPagador { get; set; }

        /// <summary>
        /// Nº do Doc Atribuído pela Empresa G064
        /// Posição: 74 até 93 (20)
        /// </summary>
        public string SeuNumero { get; set; }

        /// <summary>
        /// Nº do Doc Atribuído pela Empresa G040
        /// Posição: 102 até 104 (03)
        /// <para>Se não informado, será utilizado o "REAL"</para>
        /// </summary>
        public string TipoDeMoeda { get; set; }

        /// <summary>
        /// Tipo de Movimento G060
        /// Posição: 15 até 15 (1)
        /// </summary>
        [Required]
        public TipoDeMovimento TipoDeMovimento { get; set; }

        /// <summary>
        /// Tipo de Documento NX11
        /// Posição: 229 até 230 (2)
        /// </summary>
        public TipoDeDocumento TipoDocumento { get; set; }

        /// <summary>
        /// Valor da Mora G047
        /// Posição: 181 até 195 (13,2)
        /// </summary>
        public decimal ValorDaMora { get; set; }

        /// <summary>
        /// Valor da Multa G048
        /// Posição: 196 até 210 (13,2)
        /// </summary>
        public decimal ValorDaMulta { get; set; }

        /// <summary>
        /// Valor do Abatimento G045
        /// Posição: 151 até 165 (13,2)
        /// </summary>
        public decimal ValorDoAbatimento { get; set; }

        /// <summary>
        /// Valor da Mora + Multa L003 (13,2)
        /// </summary>
        public decimal ValorDoAcrescimo { get; set; }

        /// <summary>
        /// Valor do Desconto G046
        /// Posição: 166 até 180 (13,2)
        /// </summary>
        public decimal ValorDoDesconto { get; set; }

        /// <summary>
        /// Valor do Documento (Nominal) G042
        /// Posição: 136 até 150 (13,2)
        /// </summary>
        public decimal ValorDoDocumento { get; set; }

        /// <summary>
        /// Valor do Pagamento P010
        /// Posição: 120 até 134 (13,2)
        /// </summary>
        [Required]
        public decimal ValorDoPagamento { get; set; }

        /// <summary>
        /// Valor do Título (Nominal) G042 (13,2)
        /// </summary>
        public decimal ValorDoTitulo { get; set; }

        #endregion Public Properties
    }
}