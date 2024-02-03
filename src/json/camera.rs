use crate::json::validation::{Error, Validate};
use crate::json::{extensions, Extras, Path, Root};
use gltf_derive::Validate;
use serde_derive::{Deserialize, Serialize};

/// Projection matrix parameters.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum Projection {
    /// Perspective projection.
    Perspective {
        /// Perspective projection parameters.
        perspective: Perspective,
    },
    /// Orthographic projection.
    Orthographic {
        /// Orthographic projection parameters.
        orthographic: Orthographic,
    },
}

/// A viewpoint in the scene.
///
/// A node can reference a camera to apply a transform to place the camera in the
/// scene.
#[derive(Clone, Debug, Deserialize, Serialize, Validate)]
pub struct Camera {
    /// Optional user-defined name for this object.
    #[cfg(feature = "names")]
    #[cfg_attr(feature = "names", serde(skip_serializing_if = "Option::is_none"))]
    pub name: Option<String>,

    /// Projection matrix parameters.
    #[serde(flatten)]
    pub projection: Projection,

    /// Extension specific data.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub extensions: Option<extensions::camera::Camera>,

    /// Optional application specific data.
    #[serde(default)]
    #[cfg_attr(feature = "extras", serde(skip_serializing_if = "Option::is_none"))]
    #[cfg_attr(not(feature = "extras"), serde(skip_serializing))]
    pub extras: Extras,
}

/// Values for an orthographic camera.
#[derive(Clone, Debug, Deserialize, Serialize, Validate)]
pub struct Orthographic {
    /// The horizontal magnification of the view.
    pub xmag: f32,

    /// The vertical magnification of the view.
    pub ymag: f32,

    /// The distance to the far clipping plane.
    pub zfar: f32,

    /// The distance to the near clipping plane.
    pub znear: f32,

    /// Extension specific data.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub extensions: Option<extensions::camera::Orthographic>,

    /// Optional application specific data.
    #[serde(default)]
    #[cfg_attr(feature = "extras", serde(skip_serializing_if = "Option::is_none"))]
    #[cfg_attr(not(feature = "extras"), serde(skip_serializing))]
    pub extras: Extras,
}

/// Values for a perspective camera.
#[derive(Clone, Debug, Deserialize, Serialize, Validate)]
pub struct Perspective {
    /// Aspect ratio of the field of view.
    #[serde(rename = "aspectRatio")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub aspect_ratio: Option<f32>,

    /// The vertical field of view in radians.
    pub yfov: f32,

    /// The distance to the far clipping plane.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub zfar: Option<f32>,

    /// The distance to the near clipping plane.
    pub znear: f32,

    /// Extension specific data.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub extensions: Option<extensions::camera::Perspective>,

    /// Optional application specific data.
    #[serde(default)]
    #[cfg_attr(feature = "extras", serde(skip_serializing_if = "Option::is_none"))]
    #[cfg_attr(not(feature = "extras"), serde(skip_serializing))]
    pub extras: Extras,
}

impl Validate for Projection {
    fn validate<P, R>(&self, root: &Root, path: P, report: &mut R)
    where
        P: Fn() -> Path,
        R: FnMut(&dyn Fn() -> Path, Error),
    {
        match self {
            Self::Perspective { perspective } => {
                perspective.validate(root, || path().field("perspective"), report);
            }
            Self::Orthographic { orthographic } => {
                orthographic.validate(root, || path().field("orthographic"), report);
            }
        }
    }
}
