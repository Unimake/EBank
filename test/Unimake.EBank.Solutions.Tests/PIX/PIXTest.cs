using EBank.Solutions.Primitives.Billet.Models;
using EBank.Solutions.Primitives.PIX.Request.Consulta;
using System;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Services.PIX;
using Unimake.EBank.Solutions.Tests.Abstractions;
using Xunit;
using Xunit.Abstractions;

namespace Unimake.EBank.Solutions.Tests.PIX
{
    public class PIXTest : TestBase
    {
        #region Public Constructors

        public PIXTest(ITestOutputHelper output)
            : base(output) => StartServerDebugMode();

        #endregion Public Constructors

        #region Public Methods

        [Fact]
        public async Task ConsultarTestAsync()
        {
            try
            {
                using var scope = await CreateAuthenticatedScopeAsync();
                var service = new PIXService();
                var response = await service.GetAsync(new PIXGetRequest
                {
                    Beneficiario = new Beneficiario
                    {
                        Conta = new ContaCorrente
                        {
                            Agencia = "<<NÚMERO AGÊNCIA>>",
                            Banco = global::EBank.Solutions.Primitives.Enumerations.Banco.Sicoob,
                            Numero = "<<NUMERO CONTA>>"
                        },
                        Inscricao = "<<CNPJ>>",
                        Nome = "<<NOME DO BENEFICIÁRIO>>"
                    },
                    EndToEndId = "<<CÓDIGO END TO END>>"
                }, scope);

                DumpAsJson(response);
            }
            catch(Exception ex)
            {
                DumpAsJson(ex);
                throw;//forward
            }
        }

        #endregion Public Methods
    }
}