using System;

namespace Unimake.EBank.Solutions.Scopes.Contract
{
    /// <summary>
    /// Define um escopo de uso de um recurso na aplicação
    /// </summary>
    public interface IScope : IDisposable
    {
        #region Public Properties

        /// <summary>
        /// Se verdadeiro, o escopo foi descartado
        /// </summary>
        bool Disposed { get; }

        #endregion Public Properties

        #region Public Methods

        /// <summary>
        /// Chamado quando o escopo estiver concluído.
        /// <para>Caso não for chamado pelo desenvolvedor, pode ser chamado no <see cref="Dispose(bool)"/></para>
        /// </summary>
        void Complete();

        /// <summary>
        /// Deve ser chamado sempre que o objeto for descartado
        /// </summary>
        /// <param name="disposing">Se verdadeiro, veio do método <see cref="IDisposable.Dispose()"/>,
        /// se falso, veio do <see cref="Complete()"/> ou do destrutor</param>
        void Dispose(bool disposing);

        #endregion Public Methods
    }
}