use crate::json::validation::{Error, Validate};
use crate::json::{accessor, extensions, material, Extras, Index, Path, Root};
use gltf_derive::Validate;
use serde::ser;
use serde_derive::{Deserialize, Serialize};
use serde_json::from_value;
use serde_repr::{Deserialize_repr, Serialize_repr};
use serde_with::DeserializeFromStr;
use std::collections::BTreeMap;

/// All valid semantic names for Morph targets.
pub const VALID_MORPH_TARGETS: &[&str] = &["POSITION", "NORMAL", "TANGENT"];

/// The type of primitives to render.
#[derive(Clone, Copy, Debug, Default, Deserialize_repr, Eq, PartialEq, Serialize_repr)]
#[repr(u32)]
pub enum Mode {
    /// Corresponds to `GL_POINTS`.
    Points = 0,

    /// Corresponds to `GL_LINES`.
    Lines = 1,

    /// Corresponds to `GL_LINE_LOOP`.
    LineLoop = 2,

    /// Corresponds to `GL_LINE_STRIP`.
    LineStrip = 3,

    /// Corresponds to `GL_TRIANGLES`.
    #[default]
    Triangles = 4,

    /// Corresponds to `GL_TRIANGLE_STRIP`.
    TriangleStrip = 5,

    /// Corresponds to `GL_TRIANGLE_FAN`.
    TriangleFan = 6,
}
impl Validate for Mode {}

/// A set of primitives to be rendered.
///
/// A node can contain one or more meshes and its transform places the meshes in
/// the scene.
#[derive(Clone, Debug, Deserialize, Serialize, Validate)]
pub struct Mesh {
    /// Extension specific data.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub extensions: Option<extensions::mesh::Mesh>,

    /// Optional application specific data.
    #[serde(default)]
    #[cfg_attr(feature = "extras", serde(skip_serializing_if = "Option::is_none"))]
    #[cfg_attr(not(feature = "extras"), serde(skip_serializing))]
    pub extras: Extras,

    /// Optional user-defined name for this object.
    #[cfg(feature = "names")]
    #[cfg_attr(feature = "names", serde(skip_serializing_if = "Option::is_none"))]
    pub name: Option<String>,

    /// Defines the geometry to be renderered with a material.
    pub primitives: Vec<Primitive>,

    /// Defines the weights to be applied to the morph targets.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub weights: Option<Vec<f32>>,
}

/// Geometry to be rendered with the given material.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Primitive {
    /// Maps attribute semantic names to the `Accessor`s containing the
    /// corresponding attribute data.
    pub attributes: BTreeMap<Semantic, Index<accessor::Accessor>>,

    /// Extension specific data.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub extensions: Option<extensions::mesh::Primitive>,

    /// Optional application specific data.
    #[serde(default)]
    #[cfg_attr(feature = "extras", serde(skip_serializing_if = "Option::is_none"))]
    #[cfg_attr(not(feature = "extras"), serde(skip_serializing))]
    pub extras: Extras,

    /// The index of the accessor that contains the indices.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub indices: Option<Index<accessor::Accessor>>,

    /// The index of the material to apply to this primitive when rendering
    #[serde(skip_serializing_if = "Option::is_none")]
    pub material: Option<Index<material::Material>>,

    /// The type of primitives to render.
    #[serde(default, skip_serializing_if = "is_primitive_mode_default")]
    pub mode: Mode,

    /// An array of Morph Targets, each  Morph Target is a dictionary mapping
    /// attributes (only `POSITION`, `NORMAL`, and `TANGENT` supported) to their
    /// deviations in the Morph Target.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub targets: Option<Vec<MorphTarget>>,
}

fn is_primitive_mode_default(mode: &Mode) -> bool {
    *mode == Mode::Triangles
}

impl Validate for Primitive {
    fn validate<P, R>(&self, root: &Root, path: P, report: &mut R)
    where
        P: Fn() -> Path,
        R: FnMut(&dyn Fn() -> Path, Error),
    {
        // Generated part
        self.attributes
            .validate(root, || path().field("attributes"), report);
        self.extensions
            .validate(root, || path().field("extensions"), report);
        self.extras
            .validate(root, || path().field("extras"), report);
        self.indices
            .validate(root, || path().field("indices"), report);
        self.material
            .validate(root, || path().field("material"), report);
        self.mode.validate(root, || path().field("mode"), report);
        self.targets
            .validate(root, || path().field("targets"), report);

        // Custom part
        let position_path = &|| path().field("attributes").key("POSITION");
        if let Some(pos_accessor_index) = self.attributes.get(&Semantic::Positions) {
            // spec: POSITION accessor **must** have `min` and `max` properties defined.
            let pos_accessor = &root.accessors[pos_accessor_index.value()];

            let min_path = &|| position_path().field("min");
            if let Some(ref min) = pos_accessor.min {
                if from_value::<[f32; 3]>(min.clone()).is_err() {
                    report(min_path, Error::Invalid);
                }
            } else {
                report(min_path, Error::Missing);
            }

            let max_path = &|| position_path().field("max");
            if let Some(ref max) = pos_accessor.max {
                if from_value::<[f32; 3]>(max.clone()).is_err() {
                    report(max_path, Error::Invalid);
                }
            } else {
                report(max_path, Error::Missing);
            }
        } else {
            report(position_path, Error::Missing);
        }
    }
}

/// A dictionary mapping attributes to their deviations in the Morph Target.
#[derive(Clone, Debug, Deserialize, Serialize, Validate)]
pub struct MorphTarget {
    /// XYZ vertex position displacements of type `[f32; 3]`.
    #[serde(rename = "POSITION")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub positions: Option<Index<accessor::Accessor>>,

    /// XYZ vertex normal displacements of type `[f32; 3]`.
    #[serde(rename = "NORMAL")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub normals: Option<Index<accessor::Accessor>>,

    /// XYZ vertex tangent displacements of type `[f32; 3]`.
    #[serde(rename = "TANGENT")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tangents: Option<Index<accessor::Accessor>>,
}

/// Vertex attribute semantic name.
#[derive(Clone, Debug, DeserializeFromStr, Eq, Hash, PartialEq, Ord, PartialOrd)]
pub enum Semantic {
    /// Extra attribute name.
    Extras(String),

    /// Extension attribute name.
    Extensions(String),

    /// XYZ vertex positions.
    Positions,

    /// XYZ vertex normals.
    Normals,

    /// XYZW vertex tangents where the `w` component is a sign value indicating the
    /// handedness of the tangent basis.
    Tangents,

    /// RGB or RGBA vertex color.
    Colors(u32),

    /// UV texture co-ordinates.
    TexCoords(u32),

    /// Joint indices.
    Joints(u32),

    /// Joint weights.
    Weights(u32),
}
impl Validate for Semantic {}

impl Mode {
    /// Returns the equivalent `GLenum`.
    pub fn as_gl_enum(self) -> u32 {
        self as u32
    }
}

impl std::str::FromStr for Semantic {
    type Err = <u32 as std::str::FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "NORMAL" => Ok(Self::Normals),
            "POSITION" => Ok(Self::Positions),
            "TANGENT" => Ok(Self::Tangents),
            _ if s.starts_with("COLOR_") => s["COLOR_".len()..].parse().map(Self::Colors),
            _ if s.starts_with("TEXCOORD_") => s["TEXCOORD_".len()..].parse().map(Self::TexCoords),
            _ if s.starts_with("JOINTS_") => s["JOINTS_".len()..].parse().map(Self::Joints),
            _ if s.starts_with("WEIGHTS_") => s["WEIGHTS_".len()..].parse().map(Self::Weights),
            _ if s.starts_with('_') => Ok(Self::Extras(s[1..].to_owned())),
            _ => Ok(Self::Extensions(s.to_owned())),
        }
    }
}

impl ser::Serialize for Semantic {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl ToString for Semantic {
    fn to_string(&self) -> String {
        match *self {
            Self::Positions => "POSITION".into(),
            Self::Normals => "NORMAL".into(),
            Self::Tangents => "TANGENT".into(),
            Self::Colors(set) => format!("COLOR_{}", set),
            Self::TexCoords(set) => format!("TEXCOORD_{}", set),
            Self::Joints(set) => format!("JOINTS_{}", set),
            Self::Weights(set) => format!("WEIGHTS_{}", set),
            Self::Extras(ref name) => format!("_{name}"),
            Self::Extensions(ref name) => name.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Semantic;

    #[test]
    fn semantic() {
        let test_cases = [
            ("POSITION", Semantic::Positions),
            ("NORMAL", Semantic::Normals),
            ("TANGENT", Semantic::Tangents),
            ("COLOR_0", Semantic::Colors(0)),
            ("TEXCOORD_1", Semantic::TexCoords(1)),
            ("JOINTS_2", Semantic::Joints(2)),
            ("WEIGHTS_3", Semantic::Weights(3)),
            ("_EXTRA", Semantic::Extras("EXTRA".to_string())),
            ("EXTENSION", Semantic::Extensions("EXTENSION".to_string())),
        ];

        for (name, semantic) in test_cases {
            assert_eq!(Ok(semantic.clone()), name.parse());
            assert_eq!(name, &semantic.to_string());
        }
    }
}
