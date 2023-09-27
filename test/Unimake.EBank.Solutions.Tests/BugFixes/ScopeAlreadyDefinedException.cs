﻿using EBank.Solutions.Primitives.PIX.Request.Consulta;
using System;
using System.Threading;
using System.Threading.Tasks;
using Unimake.EBank.Solutions.Services.PIX;
using Unimake.EBank.Solutions.Tests.Abstractions;
using Xunit;
using Xunit.Abstractions;

namespace Unimake.EBank.Solutions.Tests.BugFixes
{
    public class ScopeAlreadyDefinedException : TestBase
    {
        #region Private Fields

        private const int NumOfThreads = 7;

        #endregion Private Fields

        #region Public Constructors

        public ScopeAlreadyDefinedException(ITestOutputHelper output)
            : base(output)
        {
        }

        #endregion Public Constructors

        #region Public Methods

        [Fact]
        public void FixDispose()
        {
            var waitHandles = new WaitHandle[NumOfThreads];

            for(int i = 0; i < NumOfThreads; i++)
            {
                var handle = new EventWaitHandle(false, EventResetMode.ManualReset);
                waitHandles[i] = handle;

                var tworker = new Thread(
                    new ThreadStart(async () =>
                    {
                        WriteLine($"Starting thread {i}");

                        var scope = await CreateAuthenticatedScopeAsync();
                        var service = new PIXService();
                        var response = await service.GetAsync(new PIXGetRequest
                        {
                            Testing = true,
                            Beneficiario = BeneficiarioDefault,
                            StartDate = DateTime.Parse("2023-09-24"),
                            EndDate = DateTime.Parse("2023-09-26")
                        }, scope);

                        DumpAsJson(response);
                        handle.Set();
                        scope.Dispose();
                    })
                );

                tworker.IsBackground = true;
                tworker.Start();
            }

            WaitHandle.WaitAll(waitHandles);
            WriteLine("All thread exits");
        }

        [Fact]
        public async Task FixLoopDisposeAsync()
        {
            for(int i = 0; i < NumOfThreads; i++)
            {
                WriteLine($"Starting thread {i}");

                var scope = await CreateAuthenticatedScopeAsync();
                var service = new PIXService();
                var response = await service.GetAsync(new PIXGetRequest
                {
                    Testing = true,
                    Beneficiario = BeneficiarioDefault,
                    StartDate = DateTime.Parse("2023-09-24"),
                    EndDate = DateTime.Parse("2023-09-26")
                }, scope);

                DumpAsJson(response);
                scope.Dispose();
            }

            WriteLine("All loop exits");
        }

        [Fact]
        public async Task FixLoopNoDisposeAsync()
        {
            for(int i = 0; i < NumOfThreads; i++)
            {
                WriteLine($"Starting thread {i}");

                var scope = await CreateAuthenticatedScopeAsync();
                var service = new PIXService();
                var response = await service.GetAsync(new PIXGetRequest
                {
                    Testing = true,
                    Beneficiario = BeneficiarioDefault,
                    StartDate = DateTime.Parse("2023-09-24"),
                    EndDate = DateTime.Parse("2023-09-26")
                }, scope);

                DumpAsJson(response);
            }

            WriteLine("All loop exits");
        }

        [Fact]
        public async Task FixLoopOneScopeNoDisposeAsync()
        {
            var scope = await CreateAuthenticatedScopeAsync();

            for(int i = 0; i < NumOfThreads; i++)
            {
                WriteLine($"Starting thread {i}");

                var service = new PIXService();
                var response = await service.GetAsync(new PIXGetRequest
                {
                    Testing = true,
                    Beneficiario = BeneficiarioDefault,
                    StartDate = DateTime.Parse("2023-09-24"),
                    EndDate = DateTime.Parse("2023-09-26")
                }, scope);

                DumpAsJson(response);
            }

            WriteLine("All loop exits");
        }

        [Fact]
        public void FixUsing()
        {
            var waitHandles = new WaitHandle[NumOfThreads];

            for(int i = 0; i < NumOfThreads; i++)
            {
                var handle = new EventWaitHandle(false, EventResetMode.ManualReset);
                waitHandles[i] = handle;

                var tworker = new Thread(
                    new ThreadStart(async () =>
                    {
                        WriteLine($"Starting thread {i}");

                        using var scope = await CreateAuthenticatedScopeAsync();
                        var service = new PIXService();
                        var response = await service.GetAsync(new PIXGetRequest
                        {
                            Testing = true,
                            Beneficiario = BeneficiarioDefault,
                            StartDate = DateTime.Parse("2023-09-24"),
                            EndDate = DateTime.Parse("2023-09-26")
                        }, scope);

                        DumpAsJson(response);
                        handle.Set();
                    })
                );

                tworker.IsBackground = true;
                tworker.Start();
            }

            WaitHandle.WaitAll(waitHandles);
            WriteLine("All thread exits");
        }

        #endregion Public Methods
    }
}