use gltf_derive::Validate;
use serde_derive::{Deserialize, Serialize};
#[cfg(feature = "extensions")]
use serde_json::{Map, Value};

/// A camera's projection.
///
/// A node can reference a camera to apply a transform to place the camera in the
/// scene.
#[derive(Clone, Debug, Default, Deserialize, Serialize, Validate)]
pub struct Camera {
    /// Extension data unhandled by this crate version.
    #[cfg(feature = "extensions")]
    #[serde(default, flatten)]
    pub others: Map<String, Value>,
}

/// Values for an orthographic camera.
#[derive(Clone, Debug, Default, Deserialize, Serialize, Validate)]
pub struct Orthographic {
    /// Extension data unhandled by this crate version.
    #[cfg(feature = "extensions")]
    #[serde(default, flatten)]
    pub others: Map<String, Value>,
}

/// Values for a perspective camera.
#[derive(Clone, Debug, Default, Deserialize, Serialize, Validate)]
pub struct Perspective {
    /// Extension data unhandled by this crate version.
    #[cfg(feature = "extensions")]
    #[serde(default, flatten)]
    pub others: Map<String, Value>,
}
