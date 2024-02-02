use gltf_derive::Validate;
use serde_derive::{Deserialize, Serialize};
#[cfg(feature = "extensions")]
use serde_json::{Map, Value};

/// The root object of a glTF 2.0 asset.
#[derive(Clone, Debug, Default, Deserialize, Serialize, Validate)]
pub struct Root {
    /// Support for the `KHR_lights_punctual` extension.
    #[cfg(feature = "KHR_lights_punctual")]
    #[serde(
        default,
        rename = "KHR_lights_punctual",
        skip_serializing_if = "Option::is_none"
    )]
    pub khr_lights_punctual: Option<KhrLightsPunctual>,

    /// Support for the `KHR_materials_variants` extension.
    #[cfg(feature = "KHR_materials_variants")]
    #[serde(
        default,
        rename = "KHR_materials_variants",
        skip_serializing_if = "Option::is_none"
    )]
    pub khr_materials_variants: Option<KhrMaterialsVariants>,

    /// Extension data unhandled by this crate version.
    #[cfg(feature = "extensions")]
    #[serde(default, flatten)]
    pub others: Map<String, Value>,
}

/// Support for the `KHR_lights_punctual` extension.
#[cfg(feature = "KHR_lights_punctual")]
#[derive(Clone, Debug, Default, Deserialize, Serialize, Validate)]
pub struct KhrLightsPunctual {
    /// Lights at this node.
    pub lights: Vec<crate::json::extensions::scene::khr_lights_punctual::Light>,
}

#[cfg(feature = "KHR_lights_punctual")]
impl crate::json::root::Get<crate::json::extensions::scene::khr_lights_punctual::Light>
    for crate::json::Root
{
    fn get(
        &self,
        id: crate::json::Index<crate::json::extensions::scene::khr_lights_punctual::Light>,
    ) -> Option<&crate::json::extensions::scene::khr_lights_punctual::Light> {
        if let Some(extensions) = self.extensions.as_ref() {
            if let Some(khr_lights_punctual) = extensions.khr_lights_punctual.as_ref() {
                khr_lights_punctual.lights.get(id.value())
            } else {
                None
            }
        } else {
            None
        }
    }
}

#[cfg(feature = "KHR_lights_punctual")]
impl AsRef<[crate::json::extensions::scene::khr_lights_punctual::Light]> for crate::json::Root {
    fn as_ref(&self) -> &[crate::json::extensions::scene::khr_lights_punctual::Light] {
        self.extensions
            .as_ref()
            .and_then(|extensions| extensions.khr_lights_punctual.as_ref())
            .map(|khr_lights_punctual| khr_lights_punctual.lights.as_slice())
            .unwrap_or(&[])
    }
}
#[cfg(feature = "KHR_lights_punctual")]
impl AsMut<Vec<crate::json::extensions::scene::khr_lights_punctual::Light>> for crate::json::Root {
    fn as_mut(&mut self) -> &mut Vec<crate::json::extensions::scene::khr_lights_punctual::Light> {
        &mut self
            .extensions
            .get_or_insert_with(Default::default)
            .khr_lights_punctual
            .get_or_insert_with(Default::default)
            .lights
    }
}

/// Support for the `KHR_materials_variants` extension.
#[cfg(feature = "KHR_materials_variants")]
#[derive(Clone, Debug, Default, Deserialize, Serialize, Validate)]
pub struct KhrMaterialsVariants {
    #[allow(missing_docs)]
    pub variants: Vec<crate::json::extensions::scene::khr_materials_variants::Variant>,
}

#[cfg(feature = "KHR_materials_variants")]
impl crate::json::root::Get<crate::json::extensions::scene::khr_materials_variants::Variant>
    for crate::json::Root
{
    fn get(
        &self,
        id: crate::json::Index<crate::json::extensions::scene::khr_materials_variants::Variant>,
    ) -> Option<&crate::json::extensions::scene::khr_materials_variants::Variant> {
        self.extensions
            .as_ref()?
            .khr_materials_variants
            .as_ref()?
            .variants
            .get(id.value())
    }
}

#[cfg(feature = "KHR_materials_variants")]
impl AsRef<[crate::json::extensions::scene::khr_materials_variants::Variant]>
    for crate::json::Root
{
    fn as_ref(&self) -> &[crate::json::extensions::scene::khr_materials_variants::Variant] {
        self.extensions
            .as_ref()
            .and_then(|extensions| extensions.khr_materials_variants.as_ref())
            .map(|khr_materials_variants| khr_materials_variants.variants.as_slice())
            .unwrap_or(&[])
    }
}
#[cfg(feature = "KHR_materials_variants")]
impl AsMut<Vec<crate::json::extensions::scene::khr_materials_variants::Variant>>
    for crate::json::Root
{
    fn as_mut(
        &mut self,
    ) -> &mut Vec<crate::json::extensions::scene::khr_materials_variants::Variant> {
        &mut self
            .extensions
            .get_or_insert_with(Default::default)
            .khr_materials_variants
            .get_or_insert_with(Default::default)
            .variants
    }
}
