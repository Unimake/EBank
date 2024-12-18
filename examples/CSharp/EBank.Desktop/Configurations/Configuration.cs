using EBank.Solutions.Primitives.Billet.Models;
using EBank.Solutions.Primitives.Enumerations;
using Unimake.MessageBroker.Primitives.Enumerations;

namespace EBank.Desktop.Configurations
{
    internal class Configuration
    {
        #region Private Fields

        private static Beneficiario beneficiario;

        #endregion Private Fields

        #region Public Classes

        public class AccountConfiguration
        {
            #region Public Properties

            public static string AccountNumber => "00001";
            public static Banco Bank => Banco.Sicoob;

            public static string BranchNumber => "4340";

            #endregion Public Properties
        }

        public class Endpoints
        {
            #region Public Fields

            public const string AuthServer = "http://localhost:54469/api/auth/";// "https://unimake.app/auth/api/auth/";
            public const string EBankServer = "http://localhost:58200/api/v1/"; //"https://unimake.app/EBank/";
            public const string UMessengerServer = "http://localhost:000/";

            #endregion Public Fields

            //"https://unimake.app/umessenger/";
        }

        public class Security
        {
            #region Public Classes

            public class EBank
            {
                #region Public Fields

                public const string AppId = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";
                public const string Secret = "11111111111111111111111111111111";

                #endregion Public Fields
            }

            public class UMessenger
            {
                #region Public Fields

                public const string AppId = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";
                public const string Secret = "11111111111111111111111111111111";

                #endregion Public Fields
            }

            #endregion Public Classes

            #region Public Properties

            public static string PublicKey => "CHAVE RECEBIDA NA CRIAÇÃO DA CONTA";

            #endregion Public Properties
        }

        public class UMessenger
        {
            #region Public Properties

            public static MessagingService MessagingService => MessagingService.WhatsApp;

            #endregion Public Properties
        }

        #endregion Public Classes

        #region Public Properties

        public static Beneficiario Beneficiario => beneficiario ??= new Beneficiario
        {
            Nome = "Unifake",  //Não é obrigatório
            Codigo = "000014340",
            Inscricao = Inscricao,
            Conta = new ContaCorrente
            {
                Banco = AccountConfiguration.Bank,
                Agencia = AccountConfiguration.BranchNumber,
                Numero = AccountConfiguration.AccountNumber
            }
        };

        public static string Inscricao => "06117473000079";
        public static bool Testing => true;

        #endregion Public Properties
    }
}