using EBank.Solutions.Primitives.Billet.Models;
using EBank.Solutions.Primitives.Enumerations;
using EBank.Solutions.Primitives.Enumerations.Billet;
using EBank.Solutions.Primitives.PIX.Models.Cobranca;
using EBank.Solutions.Primitives.PIX.Request.Consulta;
using EBank.Solutions.Primitives.Security;
using Newtonsoft.Json;
using Unimake.AuthServer.Authentication;
using Unimake.AuthServer.Security.Scope;
using Unimake.EBank.Solutions.Services.Billet;
using Unimake.EBank.Solutions.Services.Billet.Request;
using Unimake.EBank.Solutions.Services.Extrato;
using Unimake.EBank.Solutions.Services.Extrato.Request;
using Unimake.EBank.Solutions.Services.Pagamento;
using Unimake.EBank.Solutions.Services.Pagamento.Request;
using Unimake.EBank.Solutions.Services.PIX;
using Unimake.EBank.Solutions.Services.Varredura;
using Unimake.EBank.Solutions.Services.Varredura.Request;
using Unimake.MessageBroker.Primitives.Model.Notifications;
using Unimake.MessageBroker.Services;
using Unimake.Primitives.UDebug;

namespace eBankTest
{
    /// <summary>
    /// https://github.com/Unimake/EBank
    /// http://209.14.24.39:58295/swagger/index.html
    /// https://unimake.app/ebank/swagger/index.html
    /// </summary>
    public partial class Form1 : Form
    {
        private string TxId { get; set; }
        private readonly string AccountNumber = Prop.UnimakeAccountNumber;
        private readonly string AppId = Prop.UnimakeAppId;
        private readonly string Secret = Prop.UnimakeSecret;
        private readonly Banco Bank = Prop.UnimakeBanco;

        public Form1() => InitializeComponent();

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
                    AppId = AppId,
                    Secret = Secret
                });

                var billetService = new BilletService();

                var numeroNoBanco = "00000033";

                var response = await billetService.ExtendPaymentAsync(new ExtendPaymentRequest
                {
                    Beneficiario = new Beneficiario
                    {
                        Nome = "EMPRESA-TESTE DE MEDICAMENTOS LTDA", //Não é obrigatório
                        Codigo = "1111/11111-1",
                        Inscricao = "11111111111111",
                        Conta = new ContaCorrente
                        {
                            Banco = Banco.Itau,
                            Agencia = "9168",
                            Numero = "1111"
                        }
                    },
                    NumeroNoBanco = numeroNoBanco,
                    DataVencimento = DateTime.Now.AddDays(31),
                    Testing = true //Ambiente de homologação/teste
                }, authScope);
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
                    AppId = AppId,
                    Secret = Secret
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
                    AppId = AppId,
                    Secret = Secret
                });

                var billetService = new BilletService();

                var numeroNoBanco = "00000033";

                var response = await billetService.CancelAsync(new CancelRequest
                {
                    Beneficiario = new Beneficiario
                    {
                        Nome = "EMPRESA-TESTE DE MEDICAMENTOS LTDA", //Não é obrigatório
                        Codigo = "1111/11111-1",
                        Inscricao = "11111111111111",
                        Conta = new ContaCorrente
                        {
                            Banco = Banco.Itau,
                            Agencia = "9168",
                            Numero = "1111"
                        }
                    },
                    NumeroNoBanco = numeroNoBanco,
                    Testing = true //Ambiente de homologação/teste
                }, authScope);
            }
            catch (Exception ex)
            {
                TxtResposta.Text += "\r\n\r\nErro:\r\n\r\n" + ex.GetAllMessages();
            }
        }

        /// <summary>
        /// Consultar situação do boleto
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
                    AppId = AppId,
                    Secret = Secret
                });

                var billetService = new BilletService();

                var response = await billetService.QueryAsync(new QueryRequest
                {
                    NumeroNoBanco = "222145568",
                    Beneficiario = new Beneficiario
                    {
                        Nome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA",  //Não é obrigatório
                        Codigo = "94914",
                        Inscricao = "06117473000150",
                        Conta = new ContaCorrente
                        {
                            Banco = Banco.Sicredi,
                            Agencia = "0718",
                            Numero = "11111"
                        }
                    }
                }, authScope);

                TxtResposta.Text =
                    "Valor: " + response[0].Valor + "\r\n" +
                    "Data Emissão: " + response[0].DataEmissao + "\r\n" +
                    "Vencimento: " + response[0].DataVencimento + "\r\n" +
                    "Pagador: " + "*****************************" + "\r\n" + //response[0].Pagador.Nome
                    "Linha Digitável: " + response[0].LinhaDigitavel + "\r\n" +
                    "Data Liquidação: " + response[0].DataLiquidacao + "\r\n" +
                    "Valor Desconto: " + response[0].ValorDesconto + "\r\n" +
                    "Valor Juros: " + response[0].ValorJuros + "\r\n" +
                    "Valor Liquidação: " + response[0].ValorLiquidado + "\r\n" +
                    "Situação: " + response[0].Situacao.ToString();
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
                    AppId = AppId,
                    Secret = Secret
                });

                var extratoService = new ExtratoService();

                var startDate = new DateTime(2022, 10, 10);
                var endDate = new DateTime(2022, 10, 17);

                var pageNumber = 1;
                while (true)
                {
                    var response = await extratoService.GetAsync(new ExtratoRequest
                    {
                        AccountNumber = AccountNumber,
                        StartDate = startDate,
                        EndDate = endDate,
                        Bank = Bank,
                        PageNumber = pageNumber, //Número de página a ser consultada
                        PageSize = 10 //Quantos itens por página
                    }, authScope);

                    if (response.PageInfo.TotalPages == 0)
                    {
                        TxtResposta.Text += "Não existe movimentação no período consultado. Período: " + startDate.ToString("d") + " à " + endDate.ToString("d") + "\r\n\r\n";
                        break;
                    }

                    if (response.PageInfo.ItemsCount > 0) //Quantidade de itens retornados
                    {
                        TxtResposta.Text += "Page: " + response.PageInfo.CurrentPage + "/" + response.PageInfo.TotalPages + "\r\n";

                        var contador = 0;
                        foreach (var item in response.Items)
                        {
                            TxtResposta.Text +=
                                "---------------------------------\r\n" +
                                "Contador " + (++contador).ToString("00") + "\r\n" +
                                "---------------------------------\r\n" +
                                "Data lançamento: " + item.Lancamento.Data + "\r\n" +
                                "Valor lançamento: " + item.Lancamento.ValorLancamento + "\r\n" +
                                "Tipo lançamento: " + item.Lancamento.TipoLancamento + "\r\n" +
                                "Categoria lançamento: " + item.Lancamento.CategoriaDoLancamento + "\r\n" +
                                "Natureza: " + item.Natureza + "\r\n" +
                                "Tipo lançamento: " + item.Lancamento.TipoLancamento + "\r\n" +
                                "Numero documento: " + item.Lancamento.NumeroDocumento + "\r\n" +
                                "Descrição histórico lançamento banco: " + item.Lancamento.DescricaoHistoricoLancamentoBanco + "\r\n" +
                                "Código histórico banco: " + item.Lancamento.CodigoHistoricoBanco + "\r\n\r\n";
                        }
                    }

                    //Pula para a próxima página
                    pageNumber++;

                    //Verifica se finalizou o numero de páginas retornadas
                    if (!response.PageInfo.HasNext)
                    {
                        break;
                    }
                }

                TxtResposta.Text += "FIM!!!";

                MessageBox.Show("Consulta extrato finalizada.");
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
                    AppId = AppId,
                    Secret = Secret
                });

                var billetService = new BilletService();

                var numeroNoBanco = "00000033";

                var response = await billetService.InformPaymentAsync(new InformPaymentRequest
                {
                    Beneficiario = new Beneficiario
                    {
                        Nome = "EMPRESA-TESTE DE MEDICAMENTOS LTDA", //Não é obrigatório
                        Codigo = "1111/11111-1",
                        Inscricao = "11111111111111",
                        Conta = new ContaCorrente
                        {
                            Banco = Banco.Itau,
                            Agencia = "9168",
                            Numero = "1111"
                        }
                    },
                    NumeroNoBanco = numeroNoBanco,
                    Testing = true //Ambiente de homologação/teste

                }, authScope);
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
                    AppId = AppId,
                    Secret = Secret
                });

                var startDate = new DateTime(2022, 07, 16);
                var endDate = new DateTime(2022, 07, 16);

                var pagamentoService = new PagamentoService();

                var response = await pagamentoService.GetAsync(new PagamentoRequest
                {
                    AccountNumber = AccountNumber,
                    StartDate = startDate,
                    EndDate = endDate,
                    Bank = Bank
                }, authScope);

                //if (response?.Count > 0)
                //{
                //    var contador = 0;

                //    foreach (var item in response)
                //    {
                //        if (!string.IsNullOrWhiteSpace(item.NossoNumero))
                //        {
                //            TxtResposta.Text +=
                //                "---------------------------------\r\n" +
                //                "Contador " + (++contador).ToString("00") + "\r\n" +
                //                "---------------------------------\r\n" +
                //                (item.NossoNumero == null ? "" : "Nosso número: " + item.NossoNumero + "\r\n") +
                //                (item.NomeFavorecido == null ? "" : "Nome favorecido: " + item.NomeFavorecido + "\r\n") +
                //                "Valor: " + item.ValorReal + "\r\n" +
                //                "Valor Pagamento: " + item.ValorPagamento + "\r\n" +
                //                "Informações: " + item.Informacao2 + "\r\n" +
                //                (item.Aviso == null ? "" : "Aviso: " + item.Aviso + "\r\n") +
                //                "Banco do Favorecido: " + item.Banco.ToString() + "\r\n" +
                //                "Conta Corrente - Agencia: " + item.ContaCorrente.Agencia + "\r\n" +
                //                "Conta Corrente - Conta: " + item.ContaCorrente.Conta + "\r\n" +
                //                "Tipo Lançamento: " + item.TipoDeMovimento.ToString() + "\r\n" +
                //                "Código da Instrução para Movimento: " + item.CodigoDaInstrucaoParaMovimento.ToString() + "\r\n\r\n";
                //        }
                //    }
                //}
                //else
                //{
                //    TxtResposta.Text += "Não existem pagamentos no período consultado. Período: " + startDate.ToString("d") + " à " + endDate.ToString("d") + "\r\n\r\n";
                //}

                TxtResposta.Text += "FIM!!!";
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
                    AppId = AppId,
                    Secret = Secret
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
                    PDFConfig = new EBank.Solutions.Primitives.PDF.Models.PDFConfig
                    {
                        TryGeneratePDF = true
                    },
                    Testing = true, //Homologação, ambiente de teste
                    Mensagens = new string[] { "JUROS DIARIOS SOBRE ATRASO PAG.R$ 0,05" },
                    Beneficiario = new Beneficiario
                    {
                        Nome = "EMPRESA-TESTE DE MEDICAMENTOS LTDA",
                        Codigo = "1111/11111-1",
                        Inscricao = "22222222222222",
                        Conta = new ContaCorrente
                        {
                            Banco = Banco.Itau,
                            Agencia = "9168",
                            Numero = "1111"
                        }
                    },
                    Pagador = new Pagador
                    {
                        Nome = "PAGADOR TESTE FERREIRA",
                        TipoInscricao = TipoDeInscricao.CPF,
                        Inscricao = "11111111111",
                        Email = "teste@teste.com.br",
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
                    "Linha Digitável: " + response.LinhaDigitavel + "\r\n" +
                    "Numero no Banco: " + response.NumeroNoBanco + "\r\n\r\n" +
                    @"PDF Conteúdo: d:\testenfe\boleto.pdf";

                //Gravar o PDF do Boleto
                if (!string.IsNullOrWhiteSpace(response.PDFContent.Content))
                {
                    var pathBoleto = @"d:\testenfe\boleto.pdf";

                    if (File.Exists(pathBoleto))
                    {
                        File.Delete(pathBoleto);
                    }

                    response.PDFContent.SaveToFile(pathBoleto);
                }
            }
            catch (Exception ex)
            {
                TxtResposta.Text += "\r\n\r\nErro:\r\n\r\n" + ex.GetAllMessages();
            }
        }

        private async void BtnVarredura_Click(object sender, EventArgs e)
        {
            try
            {
                TxtResposta.Clear();

                using var authScope = new AuthenticatedScope(new AuthenticationRequest
                {
                    AppId = AppId,
                    Secret = Secret
                });

                var varreduraService = new VarreduraService();

                var startDate = new DateTime(2022, 10, 19);
                var endDate = new DateTime(2022, 10, 26);

                var pageNumber = 1;
                while (true)
                {

                    var response = await varreduraService.GetAsync(new VarreduraRequest
                    {
                        AccountNumber = AccountNumber,
                        StartDate = startDate,
                        EndDate = endDate,
                        Bank = Bank,
                        PageNumber = pageNumber, //Número de página a ser consultada
                        PageSize = 10 //Quantos itens por página
                    }, authScope);

                    if (response.PageInfo.TotalPages == 0)
                    {
                        TxtResposta.Text += "Não existe movimentação no período consultado. Período: " + startDate.ToString("d") + " à " + endDate.ToString("d") + "\r\n\r\n";
                        break;
                    }

                    if (response.PageInfo.ItemsCount > 0) //Quantidade de itens retornados
                    {
                        TxtResposta.Text += "Page: " + response.PageInfo.CurrentPage + "/" + response.PageInfo.TotalPages + "\r\n";

                        var contador = 0;
                        foreach (var item in response.Items)
                        {
                            TxtResposta.Text +=
                                "---------------------------------\r\n" +
                                "Contador " + (++contador).ToString("00") + "\r\n" +
                                "---------------------------------\r\n" +
                                "Linha digitável: " + item.LinhaDigtavel + "\r\n" +
                                "Código Barras: " + item.CodigoBarras + "\r\n" +
                                "Especie Documento: " + item.EspecieDocumento + "\r\n" +
                                "Numero Documento: " + item.NumeroDocumento + "\r\n" +
                                "Data Vencimento: " + item.DataDeVencimento + "\r\n" +
                                "Data Emissão: " + item.DataEmissao + "\r\n" +
                                "Data Limite Pagamento: " + item.DataLimitePagamento + "\r\n" +

                                "Valor: " + item.Valor + "\r\n" +
                                "Valor Abatimento: " + item.ValorAbatimento + "\r\n" +

                                "Percentual Desconto 1: " + item.ValorPercentualDesconto + "\r\n" +
                                "Data Desconto 1: " + item.DataDesconto + "\r\n" +
                                "Percentual Desconto 2: " + item.ValorPercentualDesconto2 + "\r\n" +
                                "Data Desconto 2: " + item.DataDesconto2 + "\r\n" +
                                "Percentual Desconto 3: " + item.ValorPercentualDesconto3 + "\r\n" +
                                "Data Desconto 3: " + item.DataDesconto3 + "\r\n" +

                                "Percentual Juros: " + item.ValorPercentualJuros + "\r\n" +
                                "Data Juros: " + item.DataJuros + "\r\n" +
                                "Tipo Juros: " + item.TipoJuros + "\r\n" +

                                "Percentual Multa: " + item.ValorPercentualMulta + "\r\n" +
                                "Data Multa: " + item.DataMulta + "\r\n" +
                                "Tipo Multa: " + item.TipoMulta + "\r\n" +

                                "Código Banco: " + item.CodigoBanco + "\r\n" +
                                "Nome Banco: " + item.NomeBanco + "\r\n" +

                                "Identificação Sacado: " + item.IdentificacaoSacado + "\r\n" +
                                "Tipo Identificação Sacado: " + item.TipoIdentificacaoSacado + "\r\n" +
                                "Nome Sacado: " + item.NomeSacado + "\r\n" +

                                "Tipo Beneficiário: " + item.TipoBeneficiario + "\r\n" +
                                "CNPJ/CPF Beneficiário: " + item.CNPJCPFBeneficiario + "\r\n" +
                                "Nome Beneficiário: " + item.NomeBeneficiario + "\r\n" +

                                "Tipo Pessoa Pagador: " + item.TipoPessoaPagador + "\r\n" +
                                "Agencia Cliente Pagador: " + item.AgenciaClientePagador + "\r\n" +
                                "Conta Cliente Pagador: " + item.ContaClientePagador + "\r\n" +
                                "CNPJ/CPF Pagador: " + item.CNPJCPFPagador + "\r\n" +
                                "Nome Pagador: " + item.NomePagador + "\r\n" +

                                "Controle: " + item.Controle + "\r\n" +
                                "Serviço: " + item.Servico + "\r\n\r\n";
                        }
                    }

                    //Pula para a próxima página
                    pageNumber++;

                    //Verifica se finalizou o numero de páginas retornadas
                    if (!response.PageInfo.HasNext)
                    {
                        break;
                    }
                }

                TxtResposta.Text += "FIM!!!";

                MessageBox.Show("Consulta varredura finalizada.");
            }
            catch (Exception ex)
            {
                TxtResposta.Text += "\r\n\r\nErro:\r\n\r\n" + ex.GetAllMessages();
            }
        }

        private async void BtnConsultaPIX_Click(object sender, EventArgs e)
        {
            TxtResposta.Clear();

            //debugScope = new DebugScope<DebugStateObject>(new DebugStateObject
            //{
            //    AuthServerUrl = "http://209.14.24.39:54469/api/auth/",
            //    AnotherServerUrl = "http://209.14.24.39:58195/api/v1/"
            //});

            using var authScope = new AuthenticatedScope(new AuthenticationRequest
            {
                AppId = Prop.DreamAppId,
                Secret = Prop.DreamSecret
            });

            var pixService = new PIXService();

            var startDate = DateTime.Parse("2022-11-01");
            var endDate = DateTime.Parse("2022-11-30");

            //No response da consulta PIX tem as propriedades de paginação, mas sempre será 1:1
            var response = await pixService.GetAsync(new PIXGetRequest
            {
                StartDate = startDate,
                EndDate = endDate,
                Beneficiario = new Beneficiario
                {
                    Conta = new ContaCorrente
                    {
                        Agencia = "4340",
                        Numero = "1111111",
                        Banco = Banco.Sicoob,
                    },
                    Inscricao = "11111111111111",
                    Nome = "Teste Solutions" //Não é obrigatório
                }
            }, authScope);

            if (response.PageInfo.TotalPages == 0) //Verifica o total de páginas retornadas
            {
                TxtResposta.Text += "Não existe movimentação de PIX no período consultado. Período: " + startDate.ToString("d") + " à " + endDate.ToString("d") + "\r\n\r\n";
            }

            if (response.PageInfo.ItemsCount > 0) //Quantidade de itens retornados na página
            {
                TxtResposta.Text += "Page: " + response.PageInfo.CurrentPage + "/" + response.PageInfo.TotalPages + "\r\n";

                var contador = 0;
                foreach (var item in response.Items)
                {
                    TxtResposta.Text +=
                        "---------------------------------\r\n" +
                        "Contador " + (++contador).ToString("00") + "\r\n" +
                        "---------------------------------\r\n" +
                        "ID da transação do PIX: " + item.EndToEndId + "\r\n" +
                        "ID da transação do QRCode gerado: " + item.TxId + "\r\n" +
                        "Nome do pagador: " + item.NomePagador + "\r\n" +
                        "Nome do pagador: " + item.Pagador.Nome + "\r\n" +
                        "Inscrição do pagador: " + item.Pagador.Inscricao + "\r\n" +
                        "Valor do PIX: " + item.Valor + "\r\n" +
                        "Descrição do PIX: " + item.InformacaoDoPagador + "\r\n\r\n";

                }
            }
        }

        private async void button1_Click(object sender, EventArgs e)
        {
            TxtResposta.Clear();

            using var authScope = new AuthenticatedScope(new AuthenticationRequest
            {
                AppId = Prop.DreamAppId,
                Secret = Prop.DreamSecret
            });

            var pixService = new PIXService();

            var startDate = DateTime.Parse("2022-11-01");
            var endDate = DateTime.Parse("2022-11-30");

            //Consultar um PIX em específico pelo TxId (TxId é criado, para recebimento, na geração do QrCode, tem que fixar um TxId, não deixar o banco gerar, assim o ERP tera um Id para pesquisar se recebeu ou não)
            var txId = "cobrancatestesolutionsteste";
            var response = await pixService.GetAsync(new PIXGetRequest
            {
                StartDate = startDate,
                EndDate = endDate,
                TxId = txId,
                Beneficiario = new Beneficiario
                {
                    Conta = new ContaCorrente
                    {
                        Agencia = "4340",
                        Numero = "1111111",
                        Banco = Banco.Sicoob,
                    },
                    Inscricao = "11111111111111",
                    Nome = "Teste Solutions" //Não obrigatório
                }
            }, authScope);

            if (response.PageInfo.TotalCount == 0)
            {
                TxtResposta.Text += "Não existe movimentação de PIX para o TxId no período consultado. TxId: " + txId + " - Período: " + startDate.ToString("d") + " à " + endDate.ToString("d") + "\r\n\r\n";
            }

            if (response.PageInfo.ItemsCount > 0) //Quantidade de itens retornados na página
            {
                var contador = 0;

                foreach (var item in response.Items)
                {
                    TxtResposta.Text +=
                                "---------------------------------\r\n" +
                                "Contador " + (++contador).ToString("00") + "\r\n" +
                                "---------------------------------\r\n" +
                                "ID da transação do PIX: " + item.EndToEndId + "\r\n" +
                                "ID da transação do QRCode gerado: " + item.TxId + "\r\n" +
                                "Nome do pagador: " + item.NomePagador + "\r\n" +
                                "Nome do pagador: " + item.Pagador.Nome + "\r\n" +
                                "Inscrição do pagador: " + item.Pagador.Inscricao + "\r\n" +
                                "Valor do PIX: " + item.Valor + "\r\n" +
                                "Descrição do PIX: " + item.InformacaoDoPagador + "\r\n\r\n";
                }
            }
        }

        private async void BtnSinalizarPIX_Click(object sender, EventArgs e)
        {
            TxtResposta.Clear();

            using var authScope = new AuthenticatedScope(new AuthenticationRequest
            {
                AppId = Prop.DreamAppId,
                Secret = Prop.DreamSecret
            });

            TxId = "PixWandrey" + DateTime.Now.Year.ToString("0000") + DateTime.Now.Month.ToString("00") + DateTime.Now.Day.ToString("00") + DateTime.Now.Hour.ToString("00") + DateTime.Now.Minute.ToString("00") + DateTime.Now.Second.ToString("00") + "000001";

            var pixService = new PIXService();

            var pixResponse = await pixService.CreateCobAsync(new EBank.Solutions.Primitives.PIX.Request.Cobranca.PIXCobrancaCreateRequest
            {
                SolicitacaoPagador = "Prestação de serviços em software",
                TipoCobranca = EBank.Solutions.Primitives.Enumerations.PIX.TipoCobranca.Cob, //Cobrança simples, sem vencimento.
                Valor = 0.50, //Valor do PIX
                Chave = "+5544111111111", //Chave PIX do recebedor
                TxId = TxId, //Tem que ter entre 26 e 35 caracteres (Só pode ter letras e números, não pode ter espaço ou outro carácter especial).
                GerarQRCode = true, //Se gera a imagem de QRCode ou não
                Calendario = new Calendario //Não é obrigatório, mas vai receber valores padrões
                {
                    Criacao = DateTime.Now,
                    Expiracao = 600 //600 segundos para expirar a cobrança, ou seja, 10 minutos. (Padrão é 86400 segundos, ou seja, 24 horas)
                },
                QRCodeConfig = new EBank.Solutions.Primitives.PIX.Request.QRCodeConfig // Se é obrigatório, se não informado vai assumir padrões pré-definidos
                {
                    Width = 512,
                    Height = 512,
                    Quality = 100,
                    ImageFormat = EBank.Solutions.Primitives.Enumerations.PIX.QrCodeImageFormat.Png,
                },
                Beneficiario = new Beneficiario
                {
                    Conta = new ContaCorrente
                    {
                        Agencia = "4340",
                        Numero = "1111111",
                        Banco = Banco.Sicoob,
                    },
                    Inscricao = "11111111111111",
                    Nome = "Teste Solutions" //Não obrigatório
                }
            }, authScope);

            TxtResposta.Clear();

            //Verifica se o PIX foi ativo, se sim, podemos demonstrar o QRCode para o pagador
            if (pixResponse.Status == EBank.Solutions.Primitives.Enumerations.PIX.StatusCobranca.Ativa)
            {
                var path = Path.GetTempFileName();
                WriteBase64ToPNGFile(pixResponse.QRCodeImage, path);
                ImageQrCodePIX.SizeMode = PictureBoxSizeMode.StretchImage;
                ImageQrCodePIX.LoadAsync(path);

                TxtResposta.Text +=
                    "QRCode - PIX Copia e Cola:\r\n\r\n" + pixResponse.PixCopiaECola + "\r\n\r\n" +
                    "Revisão:\r\n\r\n" + pixResponse.Revisao;
            }
            else
            {
                TxtResposta.Text += "PIX não foi ativo. Tente novamente mais tarde.";
            }
        }

        private async void BtnVerificarPIXFoiRecebido_Click(object sender, EventArgs e)
        {
            TxtResposta.Clear();

            using var authScope = new AuthenticatedScope(new AuthenticationRequest
            {
                AppId = Prop.DreamAppId,
                Secret = Prop.DreamSecret
            });

            var pixService = new PIXService();

            var startDate = DateTime.Now.AddDays(-1);
            var endDate = DateTime.Now.AddDays(+1);

            var response = await pixService.GetAsync(new PIXGetRequest
            {
                StartDate = startDate,
                EndDate = endDate,
                TxId = TxId,
                Beneficiario = new Beneficiario
                {
                    Conta = new ContaCorrente
                    {
                        Agencia = "4340",
                        Numero = "1111111",
                        Banco = Banco.Sicoob,
                    },
                    Inscricao = "11111111111111",
                    Nome = "Teste Solutions" //Não obrigatório
                }
            }, authScope);

            if (response.PageInfo.ItemsCount == 0)
            {
                TxtResposta.Text = "Devedor ainda não efetuou o PIX!!!";
            }
            else if (response.PageInfo.ItemsCount > 0)
            {
                var item = response.Items[0];
                TxtResposta.Text = "Devedor efetuou o PIX no valor de R$ " + item.Valor;
            }
        }

        /// <summary>
        /// Escreve uma string base64 em um arquivo PNG.
        /// <para>A string já deve ser um PNG válido. Este método apenas escreve o arquivo</para>
        /// </summary>
        /// <param name="content">Conteúdo que será escrito no arquivo</param>
        /// <param name="path">Pasta e nome do arquivo onde deve ser gravado o PNG</param>
        /// <exception cref="ArgumentNullException">Se o <paramref name="content"/> for nulo</exception>
        /// <exception cref="ArgumentException">Se o <paramref name="path"/> for nulo, vazio ou espaços</exception>
        private void WriteBase64ToPNGFile(string content, string path)
        {
            if (content is null)
            {
                throw new ArgumentNullException(nameof(content));
            }

            WriteBytesToPNGFile(Convert.FromBase64String(content), path);
        }

        /// <summary>
        /// Escreve os bytes um arquivo PNG.
        /// <para>Os bytes já devem ser um PNG válido. Este método apenas escreve o arquivo</para>
        /// </summary>
        /// <param name="byteArray">Bytes que serão escritos no arquivo</param>
        /// <param name="path">Pasta e nome do arquivo onde deve ser gravado o PNG</param>
        /// <exception cref="ArgumentNullException">Se o <paramref name="content"/> for nulo</exception>
        /// <exception cref="ArgumentException">Se o <paramref name="path"/> for nulo, vazio ou espaços</exception>
        private void WriteBytesToPNGFile(byte[] byteArray, string path)
        {
            if (byteArray is null)
            {
                throw new ArgumentNullException(nameof(byteArray));
            }

            if (string.IsNullOrWhiteSpace(path))
            {
                throw new ArgumentException($"'{nameof(path)}' cannot be null or whitespace.", nameof(path));
            }

            var fi = new FileInfo(path);

            if (!fi.Directory.Exists)
            {
                fi.Directory.Create();
            }

            if (fi.Exists)
            {
                fi.Delete();
            }

            File.WriteAllBytes(fi.FullName, byteArray);
        }

        private async void BtnSinalizarPIXVencto_Click(object sender, EventArgs e)
        {
            TxtResposta.Clear();

            using var authScope = new AuthenticatedScope(new AuthenticationRequest
            {
                AppId = Prop.DreamAppId,
                Secret = Prop.DreamSecret
            });

            TxId = "PixWandrey" + DateTime.Now.Year.ToString("0000") + DateTime.Now.Month.ToString("00") + DateTime.Now.Day.ToString("00") + DateTime.Now.Hour.ToString("00") + DateTime.Now.Minute.ToString("00") + DateTime.Now.Second.ToString("00") + "000001";

            var pixService = new PIXService();

            var pixResponse = await pixService.CreateCobAsync(new EBank.Solutions.Primitives.PIX.Request.Cobranca.PIXCobrancaCreateRequest
            {
                SolicitacaoPagador = "Prestação de serviços em software",
                TipoCobranca = EBank.Solutions.Primitives.Enumerations.PIX.TipoCobranca.Cob, //Cobrança simples, sem vencimento.
                Valor = 0.50, //Valor do PIX
                Chave = "+5544999999999", //Chave PIX do recebedor
                TxId = TxId, //Tem que ter entre 26 e 35 caracteres (Só pode ter letras e números, não pode ter espaço ou outro carácter especial).
                GerarQRCode = true, //Se gera a imagem de QRCode ou não
                Calendario = new Calendario //Não é obrigatório, mas vai receber valores padrões
                {
                    Criacao = DateTime.Now,
                    DataDeVencimento = DateTime.Now.AddDays(1),
                    ValidadeAposVencimento = 3
                },
                QRCodeConfig = new EBank.Solutions.Primitives.PIX.Request.QRCodeConfig // Se é obrigatório, se não informado vai assumir padrões pré-definidos
                {
                    Width = 512,
                    Height = 512,
                    Quality = 100,
                    ImageFormat = EBank.Solutions.Primitives.Enumerations.PIX.QrCodeImageFormat.Png,
                },
                Devedor = new EBank.Solutions.Primitives.PIX.Models.Devedor
                {
                    CEP = "87701111",
                    Cidade = "Paranavaí",
                    Inscricao = "111111111111",
                    Logradouro = "Rua Teste Teste, 111, Jardim Testes Teste",
                    Nome = "Teste Teste Teste",
                    UF = "PR"
                },
                Beneficiario = new Beneficiario
                {
                    Conta = new ContaCorrente
                    {
                        Agencia = "4340",
                        Numero = "1111111",
                        Banco = Banco.Sicoob,
                    },
                    Inscricao = "11111111111111",
                    Nome = "Teste Solutions" //Não obrigatório
                }
            }, authScope);

            TxtResposta.Clear();

            //Verifica se o PIX foi ativo, se sim, podemos demonstrar o QRCode para o pagador
            if (pixResponse.Status == EBank.Solutions.Primitives.Enumerations.PIX.StatusCobranca.Ativa)
            {
                var path = Path.GetTempFileName();
                WriteBase64ToPNGFile(pixResponse.QRCodeImage, path);
                ImageQrCodePIX.SizeMode = PictureBoxSizeMode.StretchImage;
                ImageQrCodePIX.LoadAsync(path);

                TxtResposta.Text +=
                    "QRCode - PIX Copia e Cola:\r\n\r\n" + pixResponse.PixCopiaECola + "\r\n\r\n" +
                    "Revisão:\r\n\r\n" + pixResponse.Revisao;
            }
            else
            {
                TxtResposta.Text += "PIX não foi ativo. Tente novamente mais tarde.";
            }
        }

        private async void BtnMsgWhatsPIX_Click(object sender, EventArgs e)
        {
            TxtResposta.Clear();

            debugScope = new DebugScope<DebugStateObject>(new DebugStateObject
            {
                AuthServerUrl = "http://209.14.24.39:54469/api/auth/",
                AnotherServerUrl = "http://209.14.24.39:58295/api/v1/"
            });

            using var authScope = new AuthenticatedScope(new AuthenticationRequest
            {
                AppId = Prop.DreamWhatsAppId,
                Secret = Prop.DreamWhatsSecret
            });

            var service = new MessageService(Prop.DreamWhatsPublicKey);
            var copyAndPaste = "00020101021226860014BR.GOV.BCB.PIX2564qrpix.ebank.solutions/qr/v2/92cff44af-f9a2-4f84-94a6-bb2611a0ded5520406546546424354041.005802BR5925UNIMAKE SOFTWARE***6304F43E";
            var linkSigned = SignLink(copyAndPaste);
            var response = await service.NotifyPIXCollectionAsync(new PIXNotification
            {
                CopyAndPaste = copyAndPaste,
                CompanyName = "Unimake",
                ContactPhone = "5544111111111",
                CustomerName = "Wandrey",
                Description = "Melhor churrasqueiro do mundo ?",
                IssuedDate = "31/12/2050",
                QueryString = linkSigned,
                To = "554411111111",
                Value = "R$ 250,00",
                Testing = true
            }, authScope);

            TxtResposta.Text += DumpAsJson(response);
        }

        private string SignLink(string qrCode)
        {
            var claims = new List<(string Key, object Value)>
            {
                    ("minhaClaim", "Oi Claim"),
                    ("qrCode", qrCode)
                };

            var linkSigned = LinkSigner.SignLink("unimake", "PIX", claims, Prop.DreamWhatsPublicKey);

            return linkSigned;
        }

        private string SignLink(long boletoId)
        {
            var claims = new List<(string Key, object Value)>
            {
                    ("minhaClaim", "Oi Claim"),
                    ("billet", boletoId)
                };

            var linkSigned = LinkSigner.SignLink("unimake", "billet", claims, Prop.DreamWhatsPublicKey);

            return linkSigned;
        }

        private JsonSerializerSettings JsonSettings => _jsonSettings ??= _jsonSettings = new JsonSerializerSettings
        {
            Formatting = Formatting.Indented,
            MissingMemberHandling = MissingMemberHandling.Ignore,
            NullValueHandling = NullValueHandling.Include,
            ReferenceLoopHandling = ReferenceLoopHandling.Ignore
        };

        private JsonSerializerSettings _jsonSettings;

        public string DumpAsJson(object value)
        {
            var text = JsonConvert.SerializeObject(value, JsonSettings);

            return text;
        }

        public static DebugScope<DebugStateObject> debugScope;

        private async void BtnMsgWhatsBoleto_Click(object sender, EventArgs e)
        {
            TxtResposta.Clear();

            debugScope = new DebugScope<DebugStateObject>(new DebugStateObject
            {
                AuthServerUrl = "http://209.14.24.39:54469/api/auth/",
                AnotherServerUrl = "http://209.14.24.39:58295/api/v1/"
            });

            using var authScope = new AuthenticatedScope(new AuthenticationRequest
            {
                AppId = Prop.DreamWhatsAppId,
                Secret = Prop.DreamWhatsSecret
            });

            var service = new MessageService(Prop.DreamWhatsPublicKey);
            var linkSigned = SignLink(123456);
            var response = await service.NotifyBilletAsync(new BilletNotification
            {
                BarCode = "65465464646554456453456453456544565445645345654435",
                BilletNumber = "12345678",
                CompanyName = "Unimake",
                ContactPhone = "554411111111",
                CustomerName = "Wandrey",
                Description = "Melhor churrasqueiro do mundo ?",
                DueDate = "31/12/2050",
                QueryString = linkSigned,
                To = "554411111111",
                Value = "R$ 250,00",
                Testing = true
            }, authScope);

            DumpAsJson(response);

            TxtResposta.Text += DumpAsJson(response);
        }
    }
}