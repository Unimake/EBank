using Unimake.EBank.Solutions.Services.Abstractions.Response;

namespace Unimake.EBank.Solutions.Model.Varredura
{
    /// <summary>
    /// Retorno do Varredura no formato CNAB. A propriedade <see cref="FileResponseBase{TContent}.Content"/> será preenchida com o conteúdo do arquivo no formato base64
    /// </summary>
    public class ItemVarreduraCNAB : FileResponseBase<string>
    {
    }
}