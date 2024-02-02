use gltf_derive::Validate;
use serde_derive::{Deserialize, Serialize};
#[cfg(feature = "extensions")]
use serde_json::{Map, Value};

/// Image data used to create a texture.
#[derive(Clone, Debug, Default, Deserialize, Serialize, Validate)]
pub struct Image {
    /// Extension data unhandled by this crate version.
    #[cfg(feature = "extensions")]
    #[serde(default, flatten)]
    pub others: Map<String, Value>,
}
