use crate::json::validation::{Error, USize64, Validate};
use crate::json::{extensions, Extras, Index, Path, Root};
use gltf_derive::Validate;
use serde_derive::{Deserialize, Serialize};
use serde_repr::{Deserialize_repr, Serialize_repr};

/// The minimum byte stride.
pub const MIN_BYTE_STRIDE: usize = 4;

/// The maximum byte stride.
pub const MAX_BYTE_STRIDE: usize = 252;

/// Specifies the target a GPU buffer should be bound to.
#[derive(Clone, Copy, Debug, Deserialize_repr, Eq, PartialEq, Serialize_repr)]
#[repr(u32)]
pub enum Target {
    /// Corresponds to `GL_ARRAY_BUFFER`.
    ArrayBuffer = 34_962,

    /// Corresponds to `GL_ELEMENT_ARRAY_BUFFER`.
    ElementArrayBuffer = 34_963,
}
impl Validate for Target {}

/// Distance between individual items in a buffer view, measured in bytes.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct Stride(pub usize);

impl Validate for Stride {
    fn validate<P, R>(&self, _root: &Root, path: P, report: &mut R)
    where
        P: Fn() -> Path,
        R: FnMut(&dyn Fn() -> Path, Error),
    {
        if self.0 < MIN_BYTE_STRIDE || self.0 > MAX_BYTE_STRIDE {
            report(&path, Error::Invalid);
        }
    }
}

/// A buffer points to binary data representing geometry, animations, or skins.
#[derive(Clone, Debug, Deserialize, Serialize, Validate)]
pub struct Buffer {
    /// The length of the buffer in bytes.
    #[serde(default, rename = "byteLength")]
    pub byte_length: USize64,

    /// Optional user-defined name for this object.
    #[cfg(feature = "names")]
    #[cfg_attr(feature = "names", serde(skip_serializing_if = "Option::is_none"))]
    pub name: Option<String>,

    /// The uri of the buffer.  Relative paths are relative to the .gltf file.
    /// Instead of referencing an external file, the uri can also be a data-uri.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub uri: Option<String>,

    /// Extension specific data.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub extensions: Option<extensions::buffer::Buffer>,

    /// Optional application specific data.
    #[serde(default)]
    #[cfg_attr(feature = "extras", serde(skip_serializing_if = "Option::is_none"))]
    #[cfg_attr(not(feature = "extras"), serde(skip_serializing))]
    pub extras: Extras,
}

/// A view into a buffer generally representing a subset of the buffer.
///
/// <https://github.com/KhronosGroup/glTF/tree/master/specification/2.0#reference-bufferview>
///
#[derive(Clone, Debug, Deserialize, Serialize, Validate)]
pub struct View {
    /// The parent `Buffer`.
    pub buffer: Index<Buffer>,

    /// The length of the `BufferView` in bytes.
    #[serde(rename = "byteLength")]
    pub byte_length: USize64,

    /// Offset into the parent buffer in bytes.
    #[serde(
        default,
        rename = "byteOffset",
        skip_serializing_if = "Option::is_none"
    )]
    pub byte_offset: Option<USize64>,

    /// The stride in bytes between vertex attributes or other interleavable data.
    ///
    /// When zero, data is assumed to be tightly packed.
    #[serde(rename = "byteStride")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub byte_stride: Option<Stride>,

    /// Optional user-defined name for this object.
    #[cfg(feature = "names")]
    #[cfg_attr(feature = "names", serde(skip_serializing_if = "Option::is_none"))]
    pub name: Option<String>,

    /// Optional target the buffer should be bound to.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub target: Option<Target>,

    /// Extension specific data.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub extensions: Option<extensions::buffer::View>,

    /// Optional application specific data.
    #[serde(default)]
    #[cfg_attr(feature = "extras", serde(skip_serializing_if = "Option::is_none"))]
    #[cfg_attr(not(feature = "extras"), serde(skip_serializing))]
    pub extras: Extras,
}
