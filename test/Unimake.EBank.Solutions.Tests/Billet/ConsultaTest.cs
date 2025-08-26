using System;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Tests.Abstractions;
using Xunit;
using Xunit.Abstractions;

namespace Unimake.EBank.Solutions.Tests.Billet
{
    public class ConsultaTest(ITestOutputHelper output) : TestBase(output)
    {
        #region Public Methods

        [Fact]
        public async Task ConsultarBoleto()
        {
            // Cria uma instância do HttpClient
            using var client = new HttpClient();
            var dataInicio = "2024-08-09";
            var dataFinal = "2024-08-09";

            // Define a URL da API
            var url = $"https://unimake.app/ebank/api/v1/boleto/consultar?configurationId=GW54H8W65J58E5S5L";

            // Token retornado pelo servidor de autenticação
            var bearerToken = "<Token retornado pelo servidor de autenticação>";

            // Configura os cabeçalhos da requisição
            client.DefaultRequestHeaders.Add("Authorization", $"Bearer {bearerToken}");

            // json de consulta
            var json = "{\"testing\": false," +
                       $"\"dataEmissaoInicial\": \"{dataInicio}\"," +
                       $"\"dataEmissaoFinal\": \"{dataFinal}\"" +
                       "}";
            var content = new StringContent(json, Encoding.UTF8, "application/json");

            try
            {
                // Envia a requisição POST
                var response = await client.PostAsync(url, content);
                var responseBody = await response.ReadAsJsonAsync();

                // Verifica se a resposta foi bem-sucedida
                if(response.IsSuccessStatusCode())
                {
                    // Ler o conteúdo da resposta
                    Console.WriteLine("Resposta da API:");
                    Console.WriteLine(responseBody);

                    // Fique atento, aqui vai retornar paginado
                    // Veja a documentação em https://unimake.app/ebank/#Paginacao
                }
                else
                {
                    Console.WriteLine($"Falha na requisição: {response.StatusCode}{Environment.NewLine}{responseBody}");
                }
            }
            catch(Exception ex)
            {
                Console.WriteLine($"Erro ao fazer a requisição: {ex.Message}");
            }
        }

        #endregion Public Methods
    }
}