using EBank.Solutions.Primitives.Billet.Models;
using EBank.Solutions.Primitives.Enumerations;
using EBank.Solutions.Primitives.Enumerations.Billet;
using System.Text;
using Unimake.AuthServer.Authentication;
using Unimake.EBank.Solutions.Scopes.Security;
using Unimake.EBank.Solutions.Services.Billet;
using Unimake.EBank.Solutions.Services.Billet.Request;
using Unimake.EBank.Solutions.Services.Extrato;
using Unimake.EBank.Solutions.Services.Extrato.Request;
using Unimake.EBank.Solutions.Services.Pagamento;
using Unimake.EBank.Solutions.Services.Pagamento.Request;

namespace eBankTest
{
    /// <summary>
    /// https://ebank.solutions/swagger/index.html
    /// https://github.com/Unimake/EBank
    /// </summary>
    public partial class Form1 : Form
    {
        public Form1() => InitializeComponent();

        /// <summary>
        /// Consultar situa��o do boleto
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private async void BtnConsultarBoleto_ClickAsync(object sender, EventArgs e)
        {
            try
            {
                TxtResposta.Clear();

                //Consultar um boleto no Sicredi
                using var authScope = new AuthenticatedScope(new AuthenticationRequest
                {
                    AppId = Prop.TesteBoletoAppId,
                    Secret = Prop.TesteBoletoSecret
                });

                var billetService = new BilletService();

                var response = await billetService.QueryAsync(new QueryRequest
                {
                    NumeroNoBanco = "222145568",
                    Beneficiario = new Beneficiario
                    {
                        Nome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA",
                        Codigo = "94914",
                        Inscricao = "06117473000150",
                        Conta = new ContaCorrente
                        {
                            Banco = Banco.Sicredi,
                            Agencia = "0718",
                            Numero = "94914"
                        }
                    }
                }, authScope);

                TxtResposta.Text =
                    "Valor: " + response[0].Valor + "\r\n" +
                    "Data Emiss�o: " + response[0].DataEmissao + "\r\n" +
                    "Vencimento: " + response[0].DataVencimento + "\r\n" +
                    "Pagador: " + "*****************************" + "\r\n" + //response[0].Pagador.Nome
                    "Linha Digit�vel: " + response[0].LinhaDigitavel + "\r\n" +
                    "Data Liquida��o: " + response[0].DataLiquidacao + "\r\n" +
                    "Valor Desconto: " + response[0].ValorDesconto + "\r\n" +
                    "Valor Juros: " + response[0].ValorJuros + "\r\n" +
                    "Valor Liquida��o: " + response[0].ValorLiquidado + "\r\n" +
                    "Situa��o: " + response[0].Situacao.ToString();
            }
            catch (Exception ex)
            {
                TxtResposta.Text += "\r\n\r\nErro:\r\n\r\n" + ex.GetAllMessages();
            }
        }

        /// <summary>
        /// Registrar boleto
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private async void BtnRegistrarBoleto_Click(object sender, EventArgs e)
        {
            try
            {
                TxtResposta.Clear();

                //Registrar um boleto no ITAU
                using var authScope = new AuthenticatedScope(new AuthenticationRequest
                {
                    AppId = Prop.AppId,
                    Secret = Prop.Secret
                });

                var billetService = new BilletService();

                var numeroNoBanco = "00000033";
                var numeroNaEmpresa = "000000301";

                var response = await billetService.RegisterAsync(new RegisterRequest
                {
                    Especie = EspecieTitulo.DuplicataMercantil,
                    NumeroNoBanco = numeroNoBanco,
                    NumeroNaEmpresa = numeroNaEmpresa,
                    Vencimento = DateTime.Now.AddDays(30),
                    Emissao = DateTime.Now,
                    DiasParaBaixaOuDevolucao = 0,
                    TipoBaixaDevolucao = TipoBaixaDevolucao.BaixarOuDevolver,
                    Carteira = "109",
                    ValorIOF = 0,
                    ValorNominal = 100.00M,
                    ValorAbatimento = 0,
                    Pdf = true,
                    Testing = true, //Homologa��o, ambiente de teste
                    Mensagens = new string[] { "JUROS DIARIOS SOBRE ATRASO PAG.R$ 0,05" },
                    Beneficiario = new Beneficiario
                    {
                        Nome = "DIMEBRAS-DISTRIBUIDORA DE MEDICAMENTOS BRASIL LTDA",
                        Codigo = "9168/00713-7",
                        Inscricao = "76472349000198",
                        Conta = new ContaCorrente
                        {
                            Banco = Banco.Itau,
                            Agencia = "9168",
                            Numero = "7137"
                        }
                    },
                    Pagador = new Pagador
                    {
                        Nome = "WANDREY MUNDIN FERREIRA",
                        TipoInscricao = TipoInscricao.CPF,
                        Inscricao = "02138394905",
                        Email = "wandrey@unimake.com.br",
                        Endereco = new Endereco
                        {
                            Rua = "RUA DAS FLORES TESTE",
                            Numero = "2222",
                            Bairro = "CENTRO",
                            CEP = "87700000",
                            Cidade = "PARANAVAI",
                            UF = "PR"
                        },
                    }
                }, authScope);

                TxtResposta.Text =
                    "Resultado do registro\r\n\r\n" +
                    "Linha Digit�vel: " + response.LinhaDigitavel + "\r\n" +
                    "Numero no Banco: " + response.NumeroNoBanco + "\r\n\r\n" +
                    @"PDF Conte�do: d:\testenfe\boleto.pdf";

                //Gravar o PDF do Boleto
                if (!string.IsNullOrWhiteSpace(response.PDFContent))
                {
                    var pathBoleto = @"d:\testenfe\boleto.pdf";

                    if (File.Exists(pathBoleto))
                    {
                        File.Delete(pathBoleto);
                    }

                    Unimake.PDFHelper.WriteBase64ToPDFFile(response.PDFContent, pathBoleto);
                }
            }
            catch (Exception ex)
            {
                TxtResposta.Text += "\r\n\r\nErro:\r\n\r\n" + ex.GetAllMessages();
            }
        }

        /// <summary>
        /// Consultar extrato
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private async void BtnExtrato_Click(object sender, EventArgs e)
        {
            try
            {
                TxtResposta.Clear();

                using var authScope = new AuthenticatedScope(new AuthenticationRequest
                {
                    AppId = Prop.AppId,
                    Secret = Prop.Secret
                });

                var extratoService = new ExtratoService();

                var response = await extratoService.GetAsync(new ExtratoRequest
                //var response = await extratoService.ListAsJsonAsync(new ExtratoRequest
                {
                    //Registration = "04245066000283",
                    AccountNumber = "82406",
                    StartDate = new DateTime(2022, 06, 16),
                    Bank = Banco.Sicoob
                }, authScope);

                var contador = 0;
                foreach (var item in response)
                {
                    TxtResposta.Text +=
                        "---------------------------------\r\n" +
                        "Contador " + (++contador).ToString("00") + "\r\n" +
                        "---------------------------------\r\n" +
                        "Data lan�amento: " + item.Lancamento.Data + "\r\n" +
                        "Valor lan�amento: " + item.Lancamento.ValorLancamento + "\r\n" +
                        "Tipo lan�amento: " + item.Lancamento.TipoLancamento + "\r\n" +
                        "Categoria lan�amento: " + item.Lancamento.CategoriaDoLancamento + "\r\n" +
                        "Natureza: " + item.Natureza + "\r\n" +
                        "Tipo lan�amento: " + item.Lancamento.TipoLancamento + "\r\n" +
                        "Numero documento: " + item.Lancamento.NumeroDocumento + "\r\n" +
                        "Descri��o hist�rico lan�amento banco: " + item.Lancamento.DescricaoHistoricoLancamentoBanco + "\r\n" +
                        "C�digo hist�rico banco: " + item.Lancamento.CodigoHistoricoBanco + "\r\n\r\n";
                }
            }
            catch (Exception ex)
            {
                TxtResposta.Text += "\r\n\r\nErro:\r\n\r\n" + ex.GetAllMessages();
            }
        }

        /// <summary>
        /// Listar pagamentos
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private async void BtnListarPagamento_Click(object sender, EventArgs e)
        {
            try
            {
                TxtResposta.Clear();

                using var authScope = new AuthenticatedScope(new AuthenticationRequest
                {
                    AppId = Prop.AppId,
                    Secret = Prop.Secret
                });

                var pagamentoService = new PagamentoService();

                var response = await pagamentoService.GetAsync(new PagamentoRequest
                {
                    AccountNumber = "82406",
                    StartDate = new DateTime(2022, 06, 14),
                    EndDate = new DateTime(2022, 06, 14),
                    Bank = Banco.Sicoob
                }, authScope);

                var contador = 0;

                foreach (var item in response)
                {
                    if (!string.IsNullOrWhiteSpace(item.NossoNumero))
                    {
                        TxtResposta.Text +=
                            "---------------------------------\r\n" +
                            "Contador " + (++contador).ToString("00") + "\r\n" +
                            "---------------------------------\r\n" +
                            (item.NossoNumero == null ? "" : "Nosso n�mero: " + item.NossoNumero + "\r\n") +
                            (item.NomeFavorecido == null ? "" : "Nome favorecido: " + item.NomeFavorecido + "\r\n") +
                            "Valor: " + item.ValorReal + "\r\n" +
                            "Valor Pagamento: " + item.ValorPagamento + "\r\n" +
                            "Informa��es: " + item.Informacao2 + "\r\n" +
                            (item.Aviso == null ? "" : "Aviso: " + item.Aviso + "\r\n") +
                            "Banco do Favorecido: " + item.Banco.ToString() + "\r\n" +
                            "Tipo Lan�amento: " + item.TipoDeMovimento.ToString() + "\r\n" +
                            "C�digo da Instru��o para Movimento: " + item.CodigoDaInstrucaoParaMovimento.ToString() + "\r\n\r\n";
                    }
                }

                TxtResposta.Text += "FIM!!!";
            }
            catch (Exception ex)
            {
                TxtResposta.Text += "\r\n\r\nErro:\r\n\r\n" + ex.GetAllMessages();
            }
        }

        /// <summary>
        /// Autorizar pagamento
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void BtnAutorizarPagamento_Click(object sender, EventArgs e)
        {
            try
            {
                TxtResposta.Clear();

                using var authScope = new AuthenticatedScope(new AuthenticationRequest
                {
                    AppId = Prop.AppId,
                    Secret = Prop.Secret
                });

                var pagamentoService = new PagamentoService();

                pagamentoService.AutorizarPagamento(new List<AutorizarPagamentoRequest>
                {
                    new AutorizarPagamentoRequest
                    {
                        DataPagamento = new DateTime(2022,06,16),
                        CodigoBarras = "1256465465465465465465465465465465465465445",
                        NossoNumero = "1234567890"
                    }
                }, authScope);

            }
            catch (Exception ex)
            {
                TxtResposta.Text += "\r\n\r\nErro:\r\n\r\n" + ex.GetAllMessages();
            }
        }

        /// <summary>
        /// Cancelar boleto
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private async void BtnCancelarBoleto_Click(object sender, EventArgs e)
        {
            try
            {
                TxtResposta.Clear();

                using var authScope = new AuthenticatedScope(new AuthenticationRequest
                {
                    AppId = Prop.AppId,
                    Secret = Prop.Secret
                });

                var billetService = new BilletService();

                var numeroNoBanco = "00000033";

                var response = await billetService.CancelAsync(new CancelRequest
                {
                    Beneficiario = new Beneficiario
                    {
                        Nome = "DIMEBRAS-DISTRIBUIDORA DE MEDICAMENTOS BRASIL LTDA",
                        Codigo = "9168/00713-7",
                        Inscricao = "76472349000198",
                        Conta = new ContaCorrente
                        {
                            Banco = Banco.Itau,
                            Agencia = "9168",
                            Numero = "7137"
                        }
                    },
                    NumeroNoBanco = numeroNoBanco,
                    Testing = true //Ambiente de homologa��o/teste
                }, authScope);
            }
            catch (Exception ex)
            {
                TxtResposta.Text += "\r\n\r\nErro:\r\n\r\n" + ex.GetAllMessages();
            }
        }

        /// <summary>
        /// Alterar vencimento boleto
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private async void BtnAlterarVencimentoBoleto_Click(object sender, EventArgs e)
        {
            try
            {
                TxtResposta.Clear();

                using var authScope = new AuthenticatedScope(new AuthenticationRequest
                {
                    AppId = Prop.AppId,
                    Secret = Prop.Secret
                });

                var billetService = new BilletService();

                var numeroNoBanco = "00000033";

                var response = await billetService.ExtendPaymentAsync(new ExtendPaymentRequest
                {
                    Beneficiario = new Beneficiario
                    {
                        Nome = "DIMEBRAS-DISTRIBUIDORA DE MEDICAMENTOS BRASIL LTDA",
                        Codigo = "9168/00713-7",
                        Inscricao = "76472349000198",
                        Conta = new ContaCorrente
                        {
                            Banco = Banco.Itau,
                            Agencia = "9168",
                            Numero = "7137"
                        }
                    },
                    NumeroNoBanco = numeroNoBanco,
                    DataVencimento = DateTime.Now.AddDays(31),
                    Testing = true //Ambiente de homologa��o/teste
                }, authScope);
            }
            catch (Exception ex)
            {
                TxtResposta.Text += "\r\n\r\nErro:\r\n\r\n" + ex.GetAllMessages();
            }
        }

        private async void BtnInformarPagamentoBoleto_Click(object sender, EventArgs e)
        {
            try
            {
                TxtResposta.Clear();

                using var authScope = new AuthenticatedScope(new AuthenticationRequest
                {
                    AppId = Prop.AppId,
                    Secret = Prop.Secret
                });

                var billetService = new BilletService();

                var numeroNoBanco = "00000033";

                var response = await billetService.InformPaymentAsync(new InformPaymentRequest
                {
                    Beneficiario = new Beneficiario
                    {
                        Nome = "DIMEBRAS-DISTRIBUIDORA DE MEDICAMENTOS BRASIL LTDA",
                        Codigo = "9168/00713-7",
                        Inscricao = "76472349000198",
                        Conta = new ContaCorrente
                        {
                            Banco = Banco.Itau,
                            Agencia = "9168",
                            Numero = "7137"
                        }
                    },
                    NumeroNoBanco = numeroNoBanco,
                    Testing = true //Ambiente de homologa��o/teste

                }, authScope);
            }
            catch (Exception ex)
            {
                TxtResposta.Text += "\r\n\r\nErro:\r\n\r\n" + ex.GetAllMessages();
            }
        }
    }
}