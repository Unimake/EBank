using System;

namespace Unimake.EBank.Solutions.Converters
{
    /// <summary>
    /// Converte uma linha digitável de 47 posições em um código de barras de 44 posições
    /// </summary>
    public static class DigitableLineToBarCodeConverter
    {
        #region Public Methods

        /// <summary>
        /// Calcula o código de barras com base na linha digitável informada em <paramref name="digitableLine"/>.
        /// </summary>
        /// <param name="digitableLine">Linha digitável com 47 posições</param>
        /// <returns></returns>
        /// <exception cref="Exception">Exceções diversas</exception>
        /// <exception cref="ArgumentNullException">Linha digitável nula ou vazia</exception>
        /// <exception cref="ArgumentOutOfRangeException">Linha digitável diferente de 47 posições</exception>
        public static string ToBarCode(string digitableLine)
        {
            var barCode = "";

            /*
              https://www.boletobancario-codigodebarras.com/2016/04/linha-digitavel.html

             Composição da Linha digitável

                 001 9 05009 ( 5 ) 401448 1606 ( 9 ) 0680935031 ( 4 ) 337370000000100 (47 dígitos)

                 Posição Tam Pos  Descrição/Conteúdo
                 01 a 03  03  00  Número do Banco na Câmara de Compensação (001 = Banco do Brasil)
                 04 a 04  01  01  Código da Moeda (9 = Real)
                 05 a 08  04  02  Campo Livre
                 09 a 09  01  03  Campo Livre
                 10 a 10  01  04  Dígito Verificador do Campo 1
                 11 a 20  10  05  Campo Livre
                 21 a 21  01  06  Dígito Verificador do Campo 2
                 22 a 31  10  07  Campo Livre
                 32 a 32  01  08  Dígito Verificador do Campo 3
                 33 a 33  01  09  Dígito Verificador do Código de Barras
                 34 a 37  04  10  Fator de Vencimento (3737 = 31/12/2007)
                 38 a 47  10  11  Valor do Documento (100 = R$1,00)

             Composição do Código de Barras

                 001 9 3 3737 0000000100 0500 9401448 1606 06809350 31 (44 números)

                 Posição Tam Descrição/Conteúdo
                 01 a 03  03 Número do Banco na Câmara de Compensação (001 = Banco do Brasil)
                 04 a 04  01 Código da Moeda (9 = Real)
                 05 a 05  01 Dígito Verificador do Código de Barras
                 06 a 09  04 Fator de Vencimento (3737 = 31/12/2007)
                 10 a 19  10 Valor do Documento (100 = R$1,00)
                 20 a 23  04 Número do Convênio fornecido pelo Banco
                 24 a 30  07 Complemento do Nosso Número, sem DV
                 31 a 34  04 Número da Agência de Relacionamento, sem DV
                 35 a 42  08 Conta Corrente de Relacionamento, sem DV
                 43 a 44  02 Tipo de Carteira/Modalidade de Cobrança
            */

            try
            {
                if(string.IsNullOrWhiteSpace(digitableLine))
                {
                    throw new ArgumentNullException($"O parâmetro '{nameof(digitableLine)}' não pode ser vazio ou nulo.", default(Exception));
                }

                if(digitableLine.Length != 47)
                {
                    throw new ArgumentOutOfRangeException($"O parâmetro '{nameof(digitableLine)}' deve ter 47 posições.", default(Exception));
                }

                //Garante que só tem números
                digitableLine = UConvert.OnlyNumbers(digitableLine, force: true).ToString();
                //                               0    1   2   3   4   5   6   7   8   9  10  11
                var slices = digitableLine.Split(03, 01, 04, 01, 01, 10, 01, 10, 01, 01, 04, 10);
                barCode = $"{slices[0]}{slices[1]}{slices[9]}{slices[10]}{slices[11]}{slices[2]}{slices[3]}{slices[5]}{slices[7]}";
                return barCode;
            }
            catch(Exception ex)
            {
                throw new Exception($"O erro '{ex.Message}' ocorreu ao validar a linha digitável '{digitableLine}'.");
            }
        }

        #endregion Public Methods
    }
}