using EBank.Solutions.Primitives.Billet.Models;
using EBank.Solutions.Primitives.PDF.Request;
using System;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Tests.Abstractions;
using Xunit;
using Xunit.Abstractions;

namespace Unimake.EBank.Solutions.Tests.PDF
{
    public class PDFTest(ITestOutputHelper output) : TestBase(output)
    {
        #region Private Fields

        // exemplo de inicialização manual — ajuste nomes/propriedades conforme a definição real de PDFRequest
        private static readonly PDFRequest Request = new PDFRequest
        {
            Boleto = new Boleto
            {
                Especie = global::EBank.Solutions.Primitives.Enumerations.Billet.EspecieTitulo.DuplicataMercantil,
                NumeroParcela = 1,
                NumeroNaEmpresa = "1001251",
                Vencimento = DateTime.Now.AddDays(30),
                Emissao = DateTime.Now,
                DiasParaBaixaOuDevolucao = 0,
                TipoBaixaDevolucao = global::EBank.Solutions.Primitives.Enumerations.Billet.TipoBaixaDevolucao.BaixarOuDevolver,
                ValorIOF = 0,
                ValorNominal = 1.00m,
                ValorAbatimento = 0m,
                // Pessoa gerada em https://www.4devs.com.br/gerador_de_empresas
                Pagador = new Pagador
                {
                    Nome = "Heitor e Vitor Transportes ME",
                    TipoInscricao = global::EBank.Solutions.Primitives.Enumerations.TipoDeInscricao.CNPJ,
                    Inscricao = "53893424000199",
                    Endereco = new Endereco
                    {
                        Logradouro = "Rua Aurélio Buarque de Hollanda",
                        Numero = "819",
                        Bairro = "Parque Xangrilá",
                        CEP = "13098-630",
                        Cidade = "Campinas",
                        UF = "SP"
                    }
                }
            },
            
            // Todos os campos abaixo são opcionais

            PixContent = new global::EBank.Solutions.Primitives.PIX.Models.PixContent
            {
                Text = "6d5sd6sad6sadsad6s4d6s4d5ad64asd64sad646d4sa6d4as6d4s6a54d5s6a4d65s4d65a4d64as6d4sa6d4s6a5d"

            }
            //+ opcionais
            // ExibirDemonstrativo = false,
            // FormatoCarne = false,
            // MostrarCodigoCarteira = true,
            // MostrarComprovanteEntrega = false,
            // MostrarComprovanteEntregaLivre = false,
            // MostrarContraApresentacaoNaDataVencimento = false,
            // MostrarEnderecoBeneficiario = false,
            // OcultarEnderecoPagador = true,
            // OcultarInstrucoes = true,
            // OcultarReciboPagador = true
        };

        #endregion Private Fields

        #region Private Methods

        private static void OpenFile(byte[] bytes, FileInfo filePath)
        {
            filePath.Directory.Create();

            File.WriteAllBytes(filePath.FullName, bytes);

            Process.Start(new ProcessStartInfo
            {
                UseShellExecute = true,
                FileName = filePath.FullName
            });
        }

        #endregion Private Methods

        #region Public Methods

        [Fact]
        public async Task GetAsBase64()
        {
            using var scope = await CreateAuthenticatedScopeAsync();

            var service = new Services.PDF.PDFService();
            var request = CreateRequest(() => Request);
            var response = await service.GetAsBase64Async(request, scope);

            Assert.NotNull(response);
            OpenFile(Convert.FromBase64String(response.PDFContent.Content), new FileInfo("boleto.pdf"));
        }

        [Fact]
        public async Task GetAsFile()
        {
            using var scope = await CreateAuthenticatedScopeAsync();

            var service = new Services.PDF.PDFService();
            var request = CreateRequest(() => Request);
            var response = await service.GetAsFileAsync(request, scope);

            Assert.NotNull(response);
            OpenFile(response, new FileInfo("boleto.pdf"));
        }

        [Fact]
        public async Task GetAsHtml()
        {
            using var scope = await CreateAuthenticatedScopeAsync();

            var service = new Services.PDF.PDFService();
            var request = CreateRequest(() => Request);
            var response = await service.GetAsHtmlAsync(request, scope);

            Assert.NotNull(response);
            OpenFile(Encoding.UTF8.GetBytes(response), new FileInfo("boleto.html"));
        }

        #endregion Public Methods
    }
}