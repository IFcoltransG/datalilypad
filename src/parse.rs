use proc_macro2::{TokenStream as TokenStream2, TokenTree as TokenTree2};
use syn::{
    parse::{Parse, ParseStream, Peek},
    parse2,
    punctuated::Punctuated,
    spanned::Spanned,
    Error, Expr, ExprCall, Token,
};

use crate::{Rule, Rules, Source};

/// `syn` representation of syntax of rules separated by semicolons
pub(crate) struct RulesSyntax {
    pub(crate) rules: Vec<RuleSyntax>,
}

/// `syn` representation of syntax of a single rule
pub(crate) struct RuleSyntax {
    lhs: ExprCall,
    _arrow: Token![<-],
    rhs: Vec<ExprCall>,
}

// Parse rule syntax into a rule
impl TryFrom<RuleSyntax> for Rule {
    type Error = Error;

    fn try_from(RuleSyntax { lhs, rhs, .. }: RuleSyntax) -> Result<Self, Self::Error> {
        let Expr::Path(expr) = *lhs.func else {
            return Err(Error::new(
                lhs.span(),
                "Left-hand-side call must begin with plain identifier before brackets",
            ));
        };
        let target_name = expr.path.require_ident()?.clone();
        let target_expressions = lhs.args.into_iter().collect();
        let sources: Vec<Source> = rhs
            .into_iter()
            .map(TryInto::try_into)
            .collect::<Result<_, _>>()
            .annotate_error("Rule right-hand-side parse failure")?;

        Ok(Rule {
            target_name,
            target_expressions,
            sources,
        })
    }
}

// Parse rules syntax into rules
impl TryFrom<RulesSyntax> for Rules {
    type Error = Error;

    fn try_from(RulesSyntax { rules }: RulesSyntax) -> Result<Self, Self::Error> {
        Ok(Rules {
            rules: rules
                .into_iter()
                .map(TryInto::try_into)
                .collect::<Result<Vec<Rule>, _>>()?,
        })
    }
}

// Parse source syntax into a source
impl TryFrom<ExprCall> for Source {
    type Error = Error;

    fn try_from(expression: ExprCall) -> Result<Self, Self::Error> {
        let span = expression.span();
        let Expr::Path(expr) = *expression.func else {
            return Err(Error::new(
                span,
                "Right-hand-side call must begin with plain identifier before brackets",
            ));
        };
        let name = expr.path.require_ident().cloned()?;
        let a_punctuated = expression.args.into_iter();
        let map = a_punctuated.map(|expr| {
            let Expr::Path(expr) = expr else {
                return Err(Error::new(
                    span,
                    "Right-hand-side must contain only plain identifiers",
                ));
            };
            expr.path.require_ident().cloned()
        });
        let variables = map.collect::<Result<_, Self::Error>>()?;
        Ok(Source { name, variables })
    }
}

impl Parse for RulesSyntax {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        Ok(RulesSyntax {
            rules: Punctuated::<RuleSyntax, Token![;]>::parse_terminated(input)?
                .into_iter()
                .collect(),
        })
    }
}

impl Parse for RuleSyntax {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let first = parse_until(input, Token![<-])?;
        let lhs = parse2(first).annotate_error("Rule left-hand-side parse error")?;
        Ok(RuleSyntax {
            lhs,
            _arrow: input.parse()?,
            rhs: Punctuated::<_, Token![,]>::parse_separated_nonempty(input)
                .annotate_error("Rule right-hand-side parse error")?
                .into_iter()
                .collect(),
        })
    }
}

/// Returns a new `TokenStream` out of an `input` `ParseStream` containing all tokens until `end`
fn parse_until(input: ParseStream, end: impl Peek) -> Result<TokenStream2, Error> {
    let mut tokens = TokenStream2::new();
    while !input.is_empty() && !input.peek(end) {
        let next: TokenTree2 = input.parse()?;
        tokens.extend(Some(next));
    }
    Ok(tokens)
}

/// Extension trait for extra utility
trait AnnotateExt {
    /// Convenience method that adds extra context to a `Result<T, syn::Error>`
    fn annotate_error(self, msg: &str) -> Self;
}

impl<T> AnnotateExt for Result<T, Error> {
    fn annotate_error(self, msg: &str) -> Self {
        self.map_err(|mut error| {
            let msg = msg;
            error.combine(Error::new(error.span(), msg));
            error
        })
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;
    use syn::{parse2, BinOp, ExprBinary};

    /// Ensures the correct structure for a single rule
    #[test]
    fn rule_parsing() {
        let q = quote! {
          vr(1+1, 2) <- l1(a), l2(b)
        }
        .into();

        let rule: RuleSyntax = parse2(q).unwrap();

        let Rule {
            target_name,
            target_expressions,
            sources,
        } = rule.try_into().unwrap();

        let [Source {
            name: id1,
            variables: vars1,
        }, Source {
            name: id2,
            variables: vars2,
        }] = &*sources
        else {
            panic!("Wrong number of RHS values");
        };

        let (&[ref var1], &[ref var2]) = (vars1.as_slice(), vars2.as_slice()) else {
            panic!("Wrong number of RHS vars")
        };

        let &[ref term1, ref term2] = &*target_expressions else {
            panic!("Wrong number of LHS exprs");
        };

        let Expr::Binary(ExprBinary {
            op: BinOp::Add(_), ..
        }) = term1
        else {
            panic!("Wrong first expression type")
        };

        let Expr::Lit(_) = term2 else {
            panic!("Wrong second expression type")
        };

        assert_eq!(target_name.to_string(), "vr");

        assert_eq!(id1.to_string(), "l1");
        assert_eq!(id2.to_string(), "l2");

        assert_eq!(var1.to_string(), "a");
        assert_eq!(var2.to_string(), "b");
    }

    /// Ensures several rules parse, in roughly the right order
    #[test]
    fn rules_parsing() {
        let q = quote! {
            a(1) <- foo(x);
            b(2) <- a(y);
            c(3) <- a(z), b(w);
        }
        .into();

        let rules: RulesSyntax = parse2(q).unwrap();

        let rules_parsed: Rules = rules.try_into().unwrap();
        let [_, _, Rule {
            sources: last_sources,
            ..
        }] = &*rules_parsed.rules
        else {
            panic!("Wrong number of rules")
        };

        assert_eq!(last_sources.len(), 2);
    }
}
