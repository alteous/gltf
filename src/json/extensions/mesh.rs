use gltf_derive::Validate;
use serde_derive::{Deserialize, Serialize};
#[cfg(feature = "extensions")]
use serde_json::{Map, Value};

/// A set of primitives to be rendered.
///
/// A node can contain one or more meshes and its transform places the meshes in
/// the scene.
#[derive(Clone, Debug, Default, Deserialize, Serialize, Validate)]
pub struct Mesh {
    /// Extension data unhandled by this crate version.
    #[cfg(feature = "extensions")]
    #[serde(default, flatten)]
    pub others: Map<String, Value>,
}

/// Geometry to be rendered with the given material.
#[derive(Clone, Debug, Default, Deserialize, Serialize, Validate)]
pub struct Primitive {
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

/// Support for the `KHR_materials_variants` extension.
#[cfg(feature = "KHR_materials_variants")]
#[derive(Clone, Debug, Default, Deserialize, Serialize, Validate)]
pub struct KhrMaterialsVariants {
    #[allow(missing_docs)]
    pub mappings: Vec<Mapping>,
}

#[allow(missing_docs)]
#[cfg(feature = "KHR_materials_variants")]
#[derive(Clone, Debug, Default, Deserialize, Serialize, Validate)]
pub struct Mapping {
    #[allow(missing_docs)]
    pub material: u32,
    #[allow(missing_docs)]
    pub variants: Vec<u32>,
}
