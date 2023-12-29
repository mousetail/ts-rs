use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::Type;

#[derive(Default)]
pub struct Dependencies(Vec<TokenStream>);

impl Dependencies {
    /// Adds all dependencies from the given type
    pub fn append_from(&mut self, ty: &Type) {
        self.0
            .push(quote!(#ty));
    }

    /// Adds the given type if it's *not* transparent.
    /// If it is, all it's child dependencies are added instead.
    pub fn push_or_append_from(&mut self, ty: &Type) {
        self.0.push(quote! {#ty});
        //     if <#ty as ts_rs::TS>::transparent() {
        //       dependencies.append(&mut <#ty as ts_rs::TS>::dependencies());
        //     } else {
        //         if let Some(dep) = ts_rs::Dependency::from_ty::<#ty>() {
        //             dependencies.push(dep);
        //         }
        //     }
        // });
    }

    pub fn append(&mut self, other: Dependencies) {
        self.0.extend(other.0)
    }

    pub fn iter(&self) -> std::slice::Iter<'_, proc_macro2::TokenStream>{
        self.0.iter()
    }
}

impl ToTokens for Dependencies {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let dependencies = &self.0;
        tokens.extend(quote! {
            {
                let mut dependencies = Vec::new();
                #(
                    if let Some(path) = <#dependencies as ts_rs::TS>::get_export_path() {
                        dependencies.push(ts_rs::Dependency{
                        type_id: std::any::TypeId::of::<#dependencies>(),
                        ts_name: <#dependencies as ts_rs::TS>::name(),
                        exported_to: path,
                    })
                }; );*
                dependencies
            }
        })
    }
}
