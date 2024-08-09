using EBank.Solutions.Primitives.Enumerations;
using System;
using System.Net.Http;
using System.Threading.Tasks;
using Xunit;

namespace Unimake.EBank.Solutions.Tests.Billet
{
    public class ConsultaTest
    {
        #region Public Methods

        [Fact]
        public async Task ConsultarBoleto()
        {
            // Cria uma instância do HttpClient
            using(var client = new HttpClient())
            {
                var dataInicio = "2024-08-01";
                var dataFinal = "2024-08-05";
                var banco = Banco.Sicredi;
                var contaCorrente = 94914;

                // Define a URL da API
                var url = $"https://unimake.app/ebank/api/v1/extrato?StartDate={dataInicio}&EndDate={dataFinal}&pagesize=5&pageNumber=1&bank={banco}&AccountNumber={contaCorrente}";

                // Token retornado pelo servidor de autenticação
                var bearerToken = "<Token retornado pelo servidor de autenticação>";

                // Configura os cabeçalhos da requisição
                client.DefaultRequestHeaders.Add("Authorization", $"Bearer {bearerToken}");

                try
                {
                    // Envia a requisição GET
                    var response = await client.GetAsync(url);
                    var responseBody = await response.Content.ReadAsStringAsync();

                    // Verifica se a resposta foi bem-sucedida
                    if(response.IsSuccessStatusCode)
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
        }

        #endregion Public Methods
    }
}