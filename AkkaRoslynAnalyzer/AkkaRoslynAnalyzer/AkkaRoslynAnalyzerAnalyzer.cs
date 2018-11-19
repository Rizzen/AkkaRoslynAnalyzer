using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace AkkaRoslynAnalyzer
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class AkkaRoslynAnalyzerAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "AkkaRoslynAnalyzer";

        private static readonly LocalizableString Title = 
            new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString MessageFormat =
            new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString Description = 
            new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof(Resources));
        private const string Category = "Argument List";

        private static readonly DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, isEnabledByDefault: true, description: Description);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

        public override void Initialize(AnalysisContext context)
        {
            // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Analyzer%20Actions%20Semantics.md for more information
            context.RegisterSyntaxNodeAction(Action, SyntaxKind.InvocationExpression);
        }

        private void Action(SyntaxNodeAnalysisContext analysisContext)
        {
            var expressionSyntax = analysisContext.Node as InvocationExpressionSyntax;
            
            if (expressionSyntax is InvocationExpressionSyntax invocationExpressionSyntax 
                && invocationExpressionSyntax.Expression is MemberAccessExpressionSyntax accessExpressionSyntax
                && accessExpressionSyntax.Name is GenericNameSyntax genericNameSyntax)
            {
                // TODO without boxing
                var argList = GetArgumentList(invocationExpressionSyntax, analysisContext.SemanticModel);
                
                // Props has only one generic parameter
                var argument = genericNameSyntax.TypeArgumentList.Arguments.First();

                var constructors = GetConstructors(argument, analysisContext.SemanticModel).First();
                var b = constructors.EqualToArgList(argList);
            }
        }

        private ITypeSymbol[] GetArgumentList(InvocationExpressionSyntax invocationExpression, SemanticModel model)
            => invocationExpression.ArgumentList.Arguments
                .Select(syntax => model.GetTypeInfo(syntax.Expression).Type)
                .Where(x => x != null)
                .ToArray();

        private List<ConstructorInfo> GetConstructors(ExpressionSyntax genericArgument, SemanticModel model)
        {
            var result = new List<ConstructorInfo>();
            if (model.GetTypeInfo(genericArgument).Type is INamedTypeSymbol typeInfo)
            {
                var ctors = typeInfo.Constructors;
                foreach (var methodSymbol in ctors)
                {
                    var @params = methodSymbol.Parameters.Select(symbol => symbol.Type).ToList();
                    result.Add(new ConstructorInfo(@params));
                }
            }

            return result;
        }
    }
}
