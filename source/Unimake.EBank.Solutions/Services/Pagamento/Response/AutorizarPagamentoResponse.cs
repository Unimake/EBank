using Unimake.EBank.Solutions.Services.Abstractions.Response;

namespace Unimake.EBank.Solutions.Services.Pagamento.Response
{
    /// <summary>
    /// Retorno da autorização de pagamento
    /// </summary>
    public class AutorizarPagamentoResponse : ResponseBase
    {
        #region Public Properties

        /// <summary>
        /// Retorna verdadeiro, se ocorreu tudo OK e está aguardando a autorização ser efetivada pelo banco
        /// <br/> O resultado da autorização, pode ser recuperado pelo serviço <see cref="PagamentoService"/>.GetAsync
        /// </summary>
        public bool AguardandoEfetivacaoBancaria => true;

        #endregion Public Properties
    }
}