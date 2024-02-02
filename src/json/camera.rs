use crate::json::validation::{Error, Validate};
use crate::json::{extensions, Extras, Path, Root};
use gltf_derive::Validate;
use serde_derive::{Deserialize, Serialize};

/// Specifies the camera type.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum Type {
    /// A perspective projection.
    #[serde(rename = "perspective")]
    Perspective = 1,

    /// An orthographic projection.
    #[serde(rename = "orthographic")]
    Orthographic,
}
impl Validate for Type {}

/// A camera's projection.
///
/// A node can reference a camera to apply a transform to place the camera in the
/// scene.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Camera {
    /// Optional user-defined name for this object.
    #[cfg(feature = "names")]
    #[cfg_attr(feature = "names", serde(skip_serializing_if = "Option::is_none"))]
    pub name: Option<String>,

    /// An orthographic camera containing properties to create an orthographic
    /// projection matrix.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub orthographic: Option<Orthographic>,

    /// A perspective camera containing properties to create a perspective
    /// projection matrix.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub perspective: Option<Perspective>,

    /// Specifies if the camera uses a perspective or orthographic projection.
    #[serde(rename = "type")]
    pub type_: Type,

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

impl Validate for Camera {
    fn validate<P, R>(&self, root: &Root, path: P, report: &mut R)
    where
        P: Fn() -> Path,
        R: FnMut(&dyn Fn() -> Path, Error),
    {
        if self.orthographic.is_none() && self.perspective.is_none() {
            report(&path, Error::Missing);
        }

        self.orthographic
            .validate(root, || path().field("orthographic"), report);
        self.perspective
            .validate(root, || path().field("perspective"), report);
        self.type_.validate(root, || path().field("type"), report);
        self.extensions
            .validate(root, || path().field("extensions"), report);
        self.extras
            .validate(root, || path().field("extras"), report);
    }
}
