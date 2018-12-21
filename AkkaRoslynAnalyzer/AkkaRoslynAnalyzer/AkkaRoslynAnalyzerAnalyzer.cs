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
        
        private const string PropsClassName = "Props";
        private const string CreateFunctionName = "Create";

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
            if (!analysisContext.Node.GetFirstToken().Text.Equals(PropsClassName)) return;

            if (!(analysisContext.Node is InvocationExpressionSyntax expressionSyntax)) return;

            var nodes = expressionSyntax.DescendantNodes().ToList();
            
            var identifiers = nodes.OfType<IdentifierNameSyntax>()
                                   .ToList();
            
            // check whether generic function call or not
            var result = identifiers[1].Identifier.Text.Equals(CreateFunctionName)
                         ? HandleNonGenericFunctionInvoke(expressionSyntax, nodes, analysisContext.SemanticModel)
                         : HandleGenericFunctionInvoke(expressionSyntax, nodes, analysisContext.SemanticModel);
            
            analysisContext.ReportDiagnostic(result);
        }

        private Diagnostic HandleGenericFunctionInvoke(InvocationExpressionSyntax expressionSyntax,
                                                       List<SyntaxNode> nodes,
                                                       SemanticModel semanticModel)
        {
            var argList = GetArgumentList(expressionSyntax, semanticModel);
            
            var actorType = nodes.OfType<GenericNameSyntax>()
                                 .FirstOrDefault()
                                 ?.TypeArgumentList.Arguments
                                 .FirstOrDefault();
            
            var constructors = GetConstructors(actorType, semanticModel);
            
            return constructors.Any(x => x.EqualToArgList(argList))
                   ? null 
                   : Diagnostic.Create(Rule, expressionSyntax.GetLocation());
        }
        
        private Diagnostic HandleNonGenericFunctionInvoke(InvocationExpressionSyntax expressionSyntax, 
                                                   List<SyntaxNode> nodes,
                                                   SemanticModel semanticModel)
        {
            // TODO
            var argList = GetArgumentList(expressionSyntax, semanticModel).Skip(1).ToArray();
            
            //nodes.OfType<IdentifierNameSyntax>().Skip(2).FirstOrDefault();
            var genericTypeSyntax = nodes.OfType<IdentifierNameSyntax>()
                                         .Skip(2)
                                         .FirstOrDefault();
            
            var constructors = GetConstructors(genericTypeSyntax, semanticModel);
            
            return constructors.Any(x => x.EqualToArgList(argList))
                   ? null 
                   : Diagnostic.Create(Rule, expressionSyntax.GetLocation());
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
