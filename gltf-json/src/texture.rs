use crate::validation::Validate;
use crate::{extensions, image, Extras, Index};
use gltf_derive::Validate;
use serde_derive::{Deserialize, Serialize};
use serde_repr::{Deserialize_repr, Serialize_repr};

/// Magnification filter.
#[derive(Clone, Copy, Debug, Deserialize_repr, Eq, PartialEq, Serialize_repr)]
#[repr(u32)]
pub enum MagFilter {
    /// Corresponds to `GL_NEAREST`.
    Nearest = 9728,

    /// Corresponds to `GL_LINEAR`.
    Linear = 9729,
}
impl Validate for MagFilter {}

impl MagFilter {
    /// OpenGL enum
    pub fn as_gl_enum(self) -> u32 {
        self as u32
    }
}

/// Minification filter.
#[derive(Clone, Copy, Debug, Deserialize_repr, Eq, PartialEq, Serialize_repr)]
#[repr(u32)]
pub enum MinFilter {
    /// Corresponds to `GL_NEAREST`.
    Nearest = MagFilter::Nearest as u32,

    /// Corresponds to `GL_LINEAR`.
    Linear = MagFilter::Linear as u32,

    /// Corresponds to `GL_NEAREST_MIPMAP_NEAREST`.
    NearestMipmapNearest = 9984,

    /// Corresponds to `GL_LINEAR_MIPMAP_NEAREST`.
    LinearMipmapNearest = 9985,

    /// Corresponds to `GL_NEAREST_MIPMAP_LINEAR`.
    NearestMipmapLinear = 9986,

    /// Corresponds to `GL_LINEAR_MIPMAP_LINEAR`.
    LinearMipmapLinear = 9987,
}
impl Validate for MinFilter {}

impl MinFilter {
    /// Returns the corresponding OpenGL enum value.
    pub fn as_gl_enum(self) -> u32 {
        self as u32
    }
}

/// Texture co-ordinate wrapping mode.
#[derive(Clone, Copy, Debug, Default, Deserialize_repr, Eq, PartialEq, Serialize_repr)]
#[repr(u32)]
pub enum WrappingMode {
    /// Corresponds to `GL_CLAMP_TO_EDGE`.
    ClampToEdge = 33_071,

    /// Corresponds to `GL_MIRRORED_REPEAT`.
    MirroredRepeat = 33_648,

    /// Corresponds to `GL_REPEAT`.
    #[default]
    Repeat = 10_497,
}
impl Validate for WrappingMode {}

impl WrappingMode {
    /// Returns the corresponding OpenGL enum value.
    pub fn as_gl_enum(self) -> u32 {
        self as u32
    }
}

/// Texture sampler properties for filtering and wrapping modes.
#[derive(Clone, Debug, Default, Deserialize, Serialize, Validate)]
#[serde(default)]
pub struct Sampler {
    /// Magnification filter.
    #[serde(rename = "magFilter")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mag_filter: Option<MagFilter>,

    /// Minification filter.
    #[serde(rename = "minFilter")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub min_filter: Option<MinFilter>,

    /// Optional user-defined name for this object.
    #[cfg(feature = "names")]
    #[cfg_attr(feature = "names", serde(skip_serializing_if = "Option::is_none"))]
    pub name: Option<String>,

    /// `s` wrapping mode.
    #[serde(default, rename = "wrapS")]
    pub wrap_s: WrappingMode,

    /// `t` wrapping mode.
    #[serde(default, rename = "wrapT")]
    pub wrap_t: WrappingMode,

    /// Extension specific data.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub extensions: Option<extensions::texture::Sampler>,

    /// Optional application specific data.
    #[serde(default)]
    #[cfg_attr(feature = "extras", serde(skip_serializing_if = "Option::is_none"))]
    #[cfg_attr(not(feature = "extras"), serde(skip_serializing))]
    pub extras: Extras,
}

/// A texture and its sampler.
#[derive(Clone, Debug, Deserialize, Serialize, Validate)]
pub struct Texture {
    /// Optional user-defined name for this object.
    #[cfg(feature = "names")]
    #[cfg_attr(feature = "names", serde(skip_serializing_if = "Option::is_none"))]
    pub name: Option<String>,

    /// The index of the sampler used by this texture.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sampler: Option<Index<Sampler>>,

    /// The index of the image used by this texture.
    pub source: Index<image::Image>,

    /// Extension specific data.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub extensions: Option<extensions::texture::Texture>,

    /// Optional application specific data.
    #[serde(default)]
    #[cfg_attr(feature = "extras", serde(skip_serializing_if = "Option::is_none"))]
    #[cfg_attr(not(feature = "extras"), serde(skip_serializing))]
    pub extras: Extras,
}

#[derive(Clone, Debug, Deserialize, Serialize, Validate)]
/// Reference to a `Texture`.
pub struct Info {
    /// The index of the texture.
    pub index: Index<Texture>,

    /// The set index of the texture's `TEXCOORD` attribute.
    #[serde(default, rename = "texCoord")]
    pub tex_coord: u32,

    /// Extension specific data.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub extensions: Option<extensions::texture::Info>,

    /// Optional application specific data.
    #[serde(default)]
    #[cfg_attr(feature = "extras", serde(skip_serializing_if = "Option::is_none"))]
    #[cfg_attr(not(feature = "extras"), serde(skip_serializing))]
    pub extras: Extras,
}
