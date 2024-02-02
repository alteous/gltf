use crate::validation::{Error, USize64, Validate};
use crate::{buffer, extensions, Extras, Index, Path, Root};
use gltf_derive::Validate;
use serde_derive::{Deserialize, Serialize};
use serde_json::Value;
use serde_repr::{Deserialize_repr, Serialize_repr};

/// The component data type.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Deserialize_repr, Serialize_repr)]
#[repr(u32)]
pub enum ComponentType {
    /// Corresponds to `GL_BYTE`.
    I8 = 5120,
    /// Corresponds to `GL_UNSIGNED_BYTE`.
    U8 = 5121,
    /// Corresponds to `GL_SHORT`.
    I16 = 5122,
    /// Corresponds to `GL_UNSIGNED_SHORT`.
    U16 = 5123,
    /// Corresponds to `GL_UNSIGNED_INT`.
    U32 = 5125,
    /// Corresponds to `GL_FLOAT`.
    F32 = 5126,
}
crate::trivial_impl_validate!(ComponentType);

impl From<sparse::IndexType> for ComponentType {
    fn from(value: sparse::IndexType) -> Self {
        match value {
            sparse::IndexType::U8 => ComponentType::U8,
            sparse::IndexType::U16 => ComponentType::U16,
            sparse::IndexType::U32 => ComponentType::U32,
        }
    }
}

/// Specifies whether an attribute, vector, or matrix.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub enum Type {
    /// Scalar quantity.
    #[serde(rename = "SCALAR")]
    Scalar = 1,
    /// 2D vector.
    #[serde(rename = "VEC2")]
    Vec2,
    /// 3D vector.
    #[serde(rename = "VEC3")]
    Vec3,
    /// 4D vector.
    #[serde(rename = "VEC4")]
    Vec4,
    /// 2x2 matrix.
    #[serde(rename = "MAT2")]
    Mat2,
    /// 3x3 matrix.
    #[serde(rename = "MAT3")]
    Mat3,
    /// 4x4 matrix.
    #[serde(rename = "MAT4")]
    Mat4,
}
crate::trivial_impl_validate!(Type);

/// Contains data structures for sparse storage.
pub mod sparse {
    use super::*;
    use crate::extensions;
    use serde_repr::{Deserialize_repr, Serialize_repr};

    /// Data type specific to sparse indices.
    #[derive(Clone, Copy, Debug, Deserialize_repr, Eq, PartialEq, Serialize_repr)]
    #[repr(u32)]
    pub enum IndexType {
        /// Corresponds to `GL_UNSIGNED_BYTE`.
        U8 = super::ComponentType::U8 as u32,
        /// Corresponds to `GL_UNSIGNED_SHORT`.
        U16 = super::ComponentType::U16 as u32,
        /// Corresponds to `GL_UNSIGNED_INT`.
        U32 = super::ComponentType::U32 as u32,
    }
    crate::trivial_impl_validate!(IndexType);

    impl IndexType {
        /// Returns the number of bytes this value represents.
        pub fn size(self) -> usize {
            super::ComponentType::from(self).size()
        }

        /// Returns the corresponding `GLenum`.
        pub fn as_gl_enum(self) -> u32 {
            super::ComponentType::from(self).as_gl_enum()
        }
    }

    /// Indices of those attributes that deviate from their initialization value.
    #[derive(Clone, Debug, Deserialize, Serialize, Validate)]
    pub struct Indices {
        /// The parent buffer view containing the sparse indices.
        ///
        /// The referenced buffer view must not have `ARRAY_BUFFER` nor
        /// `ELEMENT_ARRAY_BUFFER` as its target.
        #[serde(rename = "bufferView")]
        pub buffer_view: Index<buffer::View>,

        /// The offset relative to the start of the parent `BufferView` in bytes.
        #[serde(default, rename = "byteOffset")]
        pub byte_offset: USize64,

        /// The data type of each index.
        #[serde(rename = "componentType")]
        pub index_type: IndexType,

        /// Extension specific data.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub extensions: Option<extensions::accessor::sparse::Indices>,

        /// Optional application specific data.
        #[serde(default)]
        #[cfg_attr(feature = "extras", serde(skip_serializing_if = "Option::is_none"))]
        #[cfg_attr(not(feature = "extras"), serde(skip_serializing))]
        pub extras: Extras,
    }

    /// Sparse storage of attributes that deviate from their initialization value.
    #[derive(Clone, Debug, Deserialize, Serialize, Validate)]
    pub struct Sparse {
        /// The number of attributes encoded in this sparse accessor.
        pub count: USize64,

        /// Index array of size `count` that points to those accessor attributes
        /// that deviate from their initialization value.
        ///
        /// Indices must strictly increase.
        pub indices: Indices,

        /// Array of size `count * number_of_components` storing the displaced
        /// accessor attributes pointed by `indices`.
        ///
        /// Substituted values must have the same `component_type` and number of
        /// components as the base `Accessor`.
        pub values: Values,

        /// Extension specific data.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub extensions: Option<extensions::accessor::sparse::Sparse>,

        /// Optional application specific data.
        #[serde(default)]
        #[cfg_attr(feature = "extras", serde(skip_serializing_if = "Option::is_none"))]
        #[cfg_attr(not(feature = "extras"), serde(skip_serializing))]
        pub extras: Extras,
    }

    /// Array of size `count * number_of_components` storing the displaced
    /// accessor attributes pointed by `accessor::sparse::Indices`.
    #[derive(Clone, Debug, Deserialize, Serialize, Validate)]
    pub struct Values {
        /// The parent buffer view containing the sparse indices.
        ///
        /// The referenced buffer view must not have `ARRAY_BUFFER` nor
        /// `ELEMENT_ARRAY_BUFFER` as its target.
        #[serde(rename = "bufferView")]
        pub buffer_view: Index<buffer::View>,

        /// The offset relative to the start of the parent buffer view in bytes.
        #[serde(default, rename = "byteOffset")]
        pub byte_offset: USize64,

        /// Extension specific data.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub extensions: Option<extensions::accessor::sparse::Values>,

        /// Optional application specific data.
        #[serde(default)]
        #[cfg_attr(feature = "extras", serde(skip_serializing_if = "Option::is_none"))]
        #[cfg_attr(not(feature = "extras"), serde(skip_serializing))]
        pub extras: Extras,
    }
}

/// A typed view into a buffer view.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Accessor {
    /// The parent buffer view this accessor reads from.
    ///
    /// This field can be omitted in sparse accessors.
    #[serde(rename = "bufferView")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub buffer_view: Option<Index<buffer::View>>,

    /// The offset relative to the start of the parent `BufferView` in bytes.
    ///
    /// This field can be omitted in sparse accessors.
    #[serde(default, rename = "byteOffset")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub byte_offset: Option<USize64>,

    /// The number of components within the buffer view - not to be confused
    /// with the number of bytes in the buffer view.
    pub count: USize64,

    /// The data type of components in the attribute.
    #[serde(rename = "componentType")]
    pub component_type: ComponentType,

    /// Extension specific data.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub extensions: Option<extensions::accessor::Accessor>,

    /// Optional application specific data.
    #[serde(default)]
    #[cfg_attr(feature = "extras", serde(skip_serializing_if = "Option::is_none"))]
    #[cfg_attr(not(feature = "extras"), serde(skip_serializing))]
    pub extras: Extras,

    /// Specifies if the attribute is a scalar, vector, or matrix.
    #[serde(rename = "type")]
    pub type_: Type,

    /// Minimum value of each component in this attribute.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub min: Option<Value>,

    /// Maximum value of each component in this attribute.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub max: Option<Value>,

    /// Optional user-defined name for this object.
    #[cfg(feature = "names")]
    #[cfg_attr(feature = "names", serde(skip_serializing_if = "Option::is_none"))]
    pub name: Option<String>,

    /// Specifies whether integer data values should be normalized.
    #[serde(default, skip_serializing_if = "is_normalized_default")]
    pub normalized: bool,

    /// Sparse storage of attributes that deviate from their initialization
    /// value.
    #[serde(default)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sparse: Option<sparse::Sparse>,
}

impl Validate for Accessor {
    fn validate<P, R>(&self, root: &Root, path: P, report: &mut R)
    where
        P: Fn() -> Path,
        R: FnMut(&dyn Fn() -> Path, Error),
    {
        if self.sparse.is_none() {
            // If sparse is missing, then bufferView must be present. Report that bufferView is
            // missing since it is the more common one to require.
            if self.buffer_view.is_none() {
                report(&|| path().field("bufferView"), Error::Missing);
            }
        }

        self.buffer_view
            .validate(root, || path().field("bufferView"), report);
        self.byte_offset
            .validate(root, || path().field("byteOffset"), report);
        self.count.validate(root, || path().field("count"), report);
        self.component_type
            .validate(root, || path().field("componentType"), report);
        self.extensions
            .validate(root, || path().field("extensions"), report);
        self.extras
            .validate(root, || path().field("extras"), report);
        self.type_.validate(root, || path().field("type"), report);
        self.min.validate(root, || path().field("min"), report);
        self.max.validate(root, || path().field("max"), report);
        self.normalized
            .validate(root, || path().field("normalized"), report);
        self.sparse
            .validate(root, || path().field("sparse"), report);
    }
}

// Help serde avoid serializing this glTF 2.0 default value.
fn is_normalized_default(b: &bool) -> bool {
    !*b
}

impl ComponentType {
    /// Returns the number of bytes this value represents.
    pub fn size(self) -> usize {
        match self {
            Self::I8 | Self::U8 => 1,
            Self::I16 | Self::U16 => 2,
            Self::F32 | Self::U32 => 4,
        }
    }

    /// Returns the corresponding `GLenum`.
    pub fn as_gl_enum(self) -> u32 {
        self as u32
    }
}

impl Type {
    /// Returns the equivalent number of scalar quantities this type represents.
    pub fn multiplicity(&self) -> usize {
        use self::Type::*;
        match *self {
            Scalar => 1,
            Vec2 => 2,
            Vec3 => 3,
            Vec4 | Mat2 => 4,
            Mat3 => 9,
            Mat4 => 16,
        }
    }
}
