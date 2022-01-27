using System.Globalization;

namespace System.Threading.Tasks
{
    /// <summary>
    /// Helper para chamadas de tarefas síncronas
    /// </summary>
    public static class AsyncHelper
    {
        #region Private Fields

        private static readonly TaskFactory _taskFactory = new TaskFactory(CancellationToken.None,
                                                                           TaskCreationOptions.None,
                                                                           TaskContinuationOptions.None,
                                                                           TaskScheduler.Default);

        #endregion Private Fields

        #region Public Methods

        /// <summary>
        /// Executa uma tarefa assíncrona de forma síncrona.
        /// </summary>
        /// <typeparam name="TResult">Tipo de resultado esperado pela tarefa</typeparam>
        /// <param name="func">Função que será executada de forma síncrona</param>
        /// <returns></returns>
        public static TResult RunSync<TResult>(this Func<Task<TResult>> func)
        {
            var cultureUi = CultureInfo.CurrentUICulture;
            var culture = CultureInfo.CurrentCulture;

            return _taskFactory.StartNew(() =>
            {
                Thread.CurrentThread.CurrentCulture = culture;
                Thread.CurrentThread.CurrentUICulture = cultureUi;
                return func();
            }).Unwrap()
              .GetAwaiter()
              .GetResult();
        }

        /// <summary>
        /// Executa uma tarefa assíncrona de forma síncrona
        /// </summary>
        /// <param name="func">Função que será executada de forma síncrona</param>
        public static void RunSync(this Func<Task> func)
        {
            var cultureUi = CultureInfo.CurrentUICulture;
            var culture = CultureInfo.CurrentCulture;
            _taskFactory.StartNew(() =>
            {
                Thread.CurrentThread.CurrentCulture = culture;
                Thread.CurrentThread.CurrentUICulture = cultureUi;
                return func();
            }).Unwrap()
              .GetAwaiter()
              .GetResult();
        }

        #endregion Public Methods
    }
}