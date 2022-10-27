using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Text;

namespace Unimake.EBank.Solutions.Services.PIX.Request
{
    /// <summary>
    /// Dados de requisição para consulta do PIX
    /// </summary>
    public class PIXGetRequest : PIXRequest
    {

        /// <summary>
        /// Código identificador da transação
        /// </summary>
        [Required]
        public string EndToEndId { get; set; }
    }
}
