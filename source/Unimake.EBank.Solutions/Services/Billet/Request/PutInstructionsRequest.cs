using EBank.Solutions.Primitives.Enumerations;
using System;
using Unimake.EBank.Solutions.Services.Billet.Abstractions.Request;

namespace Unimake.EBank.Solutions.Services.Billet.Request
{
    /// <summary>
    /// <inheritdoc cref="RequestBase"/>
    /// </summary>
    public class PutInstructionsRequest : RequestBase
    {
        #region Public Properties

        /// <summary>
        /// Se a instrução, <see cref="Instrucao"/>, for de alteração de data, informar aqui.
        /// </summary>
        public DateTime Data { get; set; }

        /// <summary>
        /// Instrução que será realizada pelo banco no boleto.
        /// </summary>
        public Instrucao Instrucao { get; set; }

        /// <summary>
        /// Algumas instruções, <see cref="Instrucao"/> depende de uma ação adicional. Informar a ação aqui.
        /// </summary>
        public InstrucaoAdicional? InstrucaoAdicional { get; set; }

        /// <summary>
        /// Número do boleto que deverá ser aplicada a instrução
        /// </summary>
        public string NossoNumero { get; set; }

        /// <summary>
        /// Número do boleto no banco que deverá ser aplicada a instrução
        /// </summary>
        public string NumeroBanco { get; set; }

        /// <summary>
        ///  Se a instrução, <see cref="Instrucao"/>, for de alteração de descontos, informar aqui.
        /// </summary>
        public TipoDesconto TipoDesconto { get; set; }

        /// <summary>
        /// Em alguns momentos, a instrução executada depende de um valor que será aplicado.
        /// <para>Logo, este campo serve para informar o valor que deverá ser aplicado de acordo com cada instrução</para>
        /// </summary>
        public object Valor { get; set; }

        #endregion Public Properties
    }
}