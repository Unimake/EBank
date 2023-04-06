using EBank.Solutions.Primitives.Billet.Models;
using EBank.Solutions.Primitives.Enumerations;
using EBank.Solutions.Primitives.Exceptions.Response.Billet;
using System;
using System.IO;
using Unimake.AuthServer.Authentication;
using Unimake.AuthServer.Security.Scope;
using Unimake.EBank.Solutions.Services.Billet;
using Unimake.EBank.Solutions.Services.Billet.Request;
using Unimake.Threading;

namespace Unimake.EBank.Solutions.QueryExample
{
    internal class Program
    {
        #region Private Methods

        private static void Main(string[] args)
        {
            Console.Title = "Consulta boletos";
            MessageCenter.Log("Lendo boletos para consulta...");
            var boletos = File.ReadAllLines(@"D:\Temp\marcelo_boletos_numeros.txt");
            var queryService = new BilletService();
            var took = new TimeSpan();

            // Para utilização do e-bank, é necessário utilizar-se de um escopo autenticado.
            // Logo, iremos criar o nosso escopo

            // Para criar o escopo, será necessário a obtenção de uma chave appId e SecretId.
            // Entre em contato com a Unimake para obtenção das chaves.

            MessageCenter.Log("Criando escopo autenticado...");

            try
            {
                using var authScope = new AuthenticatedScope(new AuthenticationRequest
                {
                    AppId = "f9ec78cfb3614056bbb720770083ef6a",
                    Secret = "c94b8ede51bb42298876113a4f0d87e2"
                });

                MessageCenter.Log("Escopo autenticado com sucesso. Iniciando consulta...");

                foreach(var boleto in boletos)
                {
                    try
                    {
                        MessageCenter.DrawLine();
                        MessageCenter.Log($"Consultando boleto '{boleto}' ...");
                        took = new TimeSpan(DateTime.Now.Ticks);

                        var response = AsyncHelper.RunSync(async () =>
                        {
                            return await queryService.QueryAsync(new QueryRequest
                            {
                                NumeroNoBanco = boleto,
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
                        });

                        foreach(var item in response)
                        {
                            MessageCenter.Log($"Resposta do boleto '{item.NumeroNoBanco}'");
                            MessageCenter.Log($"Emissão: \t\t: {item.DataEmissao:dd/MM/yyyy}");
                            MessageCenter.Log($"Situação \t\t: {item.Situacao}");
                        }
                    }
                    catch(QueryInformationResponseException)
                    {
                        MessageCenter.Log($"O boleto '{boleto}' não foi encontrado.");
                    }

                    MessageCenter.Log($"{took.Seconds} segundos.");
                }
            }
            catch(Exception ex)
            {
                MessageCenter.Error(ex);
            }

            MessageCenter.DrawLine();
            MessageCenter.Alert("Consulta concluída.");
            _ = Console.ReadLine();
        }

        #endregion Private Methods
    }
}