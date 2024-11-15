use crate::parse::RulesSyntax;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{parse_macro_input, Expr, Ident};

mod analysis;
mod parse;

/// A source from a rule, parsed
struct Source {
    name: Ident,
    variables: Vec<Ident>,
}

struct Rule {
    target_name: Ident,
    target_expressions: Vec<Expr>,
    sources: Vec<Source>,
}

struct Rules {
    rules: Vec<Rule>,
}

#[proc_macro]
pub fn rules(tt: TokenStream) -> TokenStream {
    let rules = parse_macro_input!(tt as RulesSyntax);
    let rules = rules
        .try_into()
        .expect("Couldn't parse right-hand-side or left-hand-side of rule");
    rules_impl(rules).into()
}

fn rules_impl(rule: Rules) -> TokenStream2 {
    // quote! {
    //   #temp1.from_map(
    //         &#rhs1,
    //         | &rule!(@strip #(#var1),*) | (rule!(@keys #(#var1,)* ), rule!(@values #(#var1),*))
    //     );
    //     #temp2.from_map(
    //         &#rhs2,
    //         | &rule!(@strip #(#var2),*) | (rule!(@keys #(#var1,)* ), rule!(@values #(#var2),*))
    //     );
    //     #lhs.from_join(
    //         &#temp1,
    //         &#temp2,
    //         |
    //             &rule!(@keys #(#var1,)* ),
    //             &rule!(@values #(#var1),*),
    //             &rule!(@values #(#var2),*)
    //         | (#(#output_expr),*)
    //     );
    // }
    // .into()
    quote! {
        mod rules {
            pub struct Runtime<It> {
                pub iteration: It
            }

            impl Runtime<It> {
                pub fn new(iteration: It) -> Self {
                    Self {
                        iteration
                    }
                }

                pub fn run_rules(&self) {}
            }
        }
    }
    .into()
}
