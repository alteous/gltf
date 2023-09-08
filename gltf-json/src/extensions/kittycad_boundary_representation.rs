/// 2D and 3D curve definitions.
pub mod curve {
    use crate::validation::{Checked, Error, Validate};
    use crate::{Path, Root};
    use gltf_derive::Validate;
    use serde::{de, ser};
    use serde_derive::{Deserialize, Serialize};
    use std::fmt;

    pub const VALID_CURVE_TYPES: &[&str] = &["circle", "linear", "nurbs"];

    /// Discriminant for `Curve` data.
    #[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq)]
    pub enum Type {
        /// Circular curve.
        Circle = 1,
        /// Line curve.
        Line,
        /// NURBS curve.
        Nurbs,
    }

    impl Type {
        pub fn as_str(self) -> &'static str {
            match self {
                Type::Circle => "circle",
                Type::Line => "line",
                Type::Nurbs => "nurbs",
            }
        }
    }

    impl<'de> de::Deserialize<'de> for Checked<Type> {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: de::Deserializer<'de>,
        {
            struct Visitor;
            impl<'de> de::Visitor<'de> for Visitor {
                type Value = Checked<Type>;

                fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    write!(f, "any of: {:?}", VALID_CURVE_TYPES)
                }

                fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
                where
                    E: de::Error,
                {
                    Ok(match value {
                        "circle" => Checked::Valid(Type::Circle),
                        "line" => Checked::Valid(Type::Line),
                        "nurbs" => Checked::Valid(Type::Nurbs),
                        _ => Checked::Invalid,
                    })
                }
            }
            deserializer.deserialize_str(Visitor)
        }
    }

    impl ser::Serialize for Type {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: ser::Serializer,
        {
            serializer.serialize_str(self.as_str())
        }
    }

    /// Circular curve definition.
    ///
    /// λ(u) := O + R(cos(u)x + sin(u)y), where:
    /// * O = `self.origin`,
    /// * R = `self.radius`,
    /// * x = `self.xbasis`,
    /// * y = `self.normal` × `self.xbasis`,
    /// * u ∈ {0, 2π}.
    ///
    /// The `xbasis` and `normal` vectors form an orthonormal set.
    #[derive(Clone, Debug, Deserialize, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    pub struct Circle {
        /// Position at the center of the circle.
        pub origin: [f32; 3],
        /// Distance from the center position to all points on the circle.
        pub radius: f32,
        /// Unit vector normal to the plane containing the circle.
        ///
        /// This serves as the Z basis in the parametric co-ordinate space.
        pub normal: [f32; 3],
        /// Unit vector in the direction from the origin to the point on
        /// the circle at λ(0).
        ///
        /// Due to floating point precision, this vector may not lie exactly
        /// in the plane. If this is the case then the X vector will be treated
        /// as the projection of this vector onto the plane.
        pub xbasis: [f32; 3],
    }

    /// Line curve definition.
    ///
    /// Either end or direction must be set.
    #[derive(Clone, Debug, Deserialize, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct Line {
        /// Origin position.
        pub start: [f32; 3],
        /// Unit vector pointing away from the origin position.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub direction: Option<[f32; 3]>,
        /// End position.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub end: Option<[f32; 3]>,
    }

    impl Validate for Line {
        fn validate<P, R>(&self, _root: &Root, path: P, report: &mut R)
        where
            P: Fn() -> Path,
            R: FnMut(&dyn Fn() -> Path, Error),
        {
            if self.direction.is_none() && self.end.is_none() {
                report(&|| path().field("end"), Error::Missing);
            }
        }
    }

    /// NURBS curve definition.
    #[derive(Clone, Debug, Deserialize, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    pub struct Nurbs {
        /// Array of control vertices.
        pub control_points: Vec<[f32; 4]>,
        /// Knot vector.
        pub knot_vector: Vec<f32>,
        /// Order of basis splines.
        pub order: u32,
    }

    /// Curve parameter domain.
    #[derive(Clone, Debug, Deserialize, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    pub struct Domain {
        /// Minimum domain value.
        pub min: f32,

        /// Maximum domain value.
        pub max: f32,
    }

    impl Default for Domain {
        fn default() -> Self {
            Self { min: 0.0, max: 1.0 }
        }
    }

    /// Abstract curve data.
    #[derive(Clone, Debug, Deserialize, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    pub struct Curve {
        /// Discriminant.
        #[serde(rename = "type")]
        pub type_: Checked<Type>,

        /// Optional name for this surface.
        #[cfg(feature = "names")]
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub name: Option<String>,

        /// Additional parameters for a circular curve.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub circle: Option<Circle>,

        /// Additional parameters for a line curve.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub line: Option<Line>,

        /// Additional parameters for a NURBS curve.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub nurbs: Option<Nurbs>,

        /// Parameter domain.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub domain: Option<Domain>,
    }
}

/// 3D surface definitions.
pub mod surface {
    use crate::validation::{Checked, Error, Validate};
    use crate::{Path, Root};
    use gltf_derive::Validate;
    use serde::{de, ser};
    use serde_derive::{Deserialize, Serialize};
    use std::fmt;

    pub const VALID_SURFACE_TYPES: &[&str] = &["cylinder", "nurbs", "plane", "torus"];

    /// Domain of surface parameters.
    #[derive(Clone, Debug, Deserialize, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    pub struct Domain {
        /// Minimum domain values.
        pub min: [f32; 2],

        /// Maximum domain values.
        pub max: [f32; 2],
    }

    impl Default for Domain {
        fn default() -> Self {
            Self {
                min: [0.0, 0.0],
                max: [1.0, 1.0],
            }
        }
    }

    /// Discriminant for `Surface` data.
    #[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq)]
    pub enum Type {
        /// Cylindrical surface.
        Cylinder = 1,
        /// NURBS surface.
        Nurbs,
        /// Planar surface.
        Plane,
        /// Torus surface.
        Torus,
    }

    impl Type {
        pub fn as_str(self) -> &'static str {
            match self {
                Type::Cylinder => "cylinder",
                Type::Nurbs => "nurbs",
                Type::Plane => "plane",
                Type::Torus => "torus",
            }
        }
    }

    impl<'de> de::Deserialize<'de> for Checked<Type> {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: de::Deserializer<'de>,
        {
            struct Visitor;
            impl<'de> de::Visitor<'de> for Visitor {
                type Value = Checked<Type>;

                fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    write!(f, "any of: {:?}", VALID_SURFACE_TYPES)
                }

                fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
                where
                    E: de::Error,
                {
                    Ok(match value {
                        "cylinder" => Checked::Valid(Type::Cylinder),
                        "nurbs" => Checked::Valid(Type::Nurbs),
                        "plane" => Checked::Valid(Type::Plane),
                        "torus" => Checked::Valid(Type::Torus),
                        _ => Checked::Invalid,
                    })
                }
            }
            deserializer.deserialize_str(Visitor)
        }
    }

    impl ser::Serialize for Type {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: ser::Serializer,
        {
            serializer.serialize_str(self.as_str())
        }
    }

    /// Parametric cylindrical surface definition.
    ///
    /// σ(u, v) := O + R(cos(u)x + sin(u)y) + vz, where:
    /// * O = `self.origin`,
    /// * R = `self.radius`,
    /// * x = `self.xbasis`,
    /// * y = `self.ybasis`,
    /// * z = `self.zbasis`.
    ///
    /// In the field documentation, the 'base circle' is
    /// defined as the cycle defined at σ(u, 0).
    ///
    /// The vectors `xbasis`, `ybasis`, and `zbasis` form
    /// an orthonormal set.
    #[derive(Clone, Debug, Deserialize, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    pub struct Cylinder {
        /// Origin of the base circle.
        pub origin: [f32; 3],
        /// Radius of the base circle.
        pub radius: f32,
        /// Normal vector in the direction from the origin of the
        /// base circle to the point on the cylinder at σ(0, 0).
        pub xbasis: [f32; 3],
        /// Normal vector in the direction from the origin of the
        /// base circle to the point on the cylinder at σ(90°, 0).
        pub ybasis: [f32; 3],
        /// Normal vector in the direction of increasing values of
        /// the parameter v.
        pub zbasis: [f32; 3],
    }

    /// NURBS surface definition.
    #[derive(Clone, Debug, Deserialize, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct Nurbs {
        /// Matrix of control point vertices.
        pub control_points: Vec<[f32; 4]>,
        /// Dimensions of control point vertex matrix.
        pub num_control_points: [u32; 2],
        /// Number of knots in U and V.
        pub num_knots: [u32; 2],
        /// Knot vector.
        pub knot_vector: Vec<f32>,
        /// Order of basis splines.
        pub order: [u32; 2],
    }

    impl Validate for Nurbs {
        fn validate<P, R>(&self, _root: &Root, path: P, report: &mut R)
        where
            P: Fn() -> Path,
            R: FnMut(&dyn Fn() -> Path, Error),
        {
            let expected_control_points = self.num_control_points[0] * self.num_control_points[1];
            if expected_control_points as usize != self.control_points.len() {
                report(&|| path().field("num_control_points"), Error::Invalid);
            }

            let expected_knots = self.num_knots[0] + self.num_knots[1];
            if expected_knots as usize != self.knot_vector.len() {
                report(&|| path().field("num_knots"), Error::Invalid);
            }
        }
    }

    /// Plane surface definition.
    #[derive(Clone, Debug, Deserialize, Serialize, gltf_derive::Validate)]
    #[serde(rename_all = "camelCase")]
    pub struct Plane {
        /// Normal vector to the plane.
        pub normal: [f32; 3],
        /// The value of `d` in the plane equation `n.r = d`.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub constant: Option<f32>,
        /// An arbitrary point that lies on the plane.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub point: Option<[f32; 3]>,
    }

    /// Toroidal surface definition.
    ///
    /// σ(u, v) := O + (R + rcos(v))(cos(u)x + sin(u)y) + rsin(v)z, where:
    /// * O = `self.origin`,
    /// * R = `self.major_radius`,
    /// * r = `self.minor_radius`,
    /// * x = `self.xbasis`,
    /// * y = `self.ybasis`,
    /// * z = `self.zbasis`.
    #[derive(Clone, Debug, Deserialize, Serialize, gltf_derive::Validate)]
    #[serde(rename_all = "camelCase")]
    pub struct Torus {
        /// The center of the torus.
        pub origin: [f32; 3],
        /// Distance from the origin to the origin of the base circle.
        pub major_radius: f32,
        /// Radius of the base circle.
        pub minor_radius: f32,
        /// Normal vector in the direction from the origin of the
        /// base circle to the point on the torus at σ(0, 0).
        pub xbasis: [f32; 3],
        /// Normal vector in the direction from the origin of the
        /// base circle to the point on the torus at σ(90°, 0).
        pub ybasis: [f32; 3],
        /// Normal vector in the direction of increasing values of
        /// the parameter v as if evaluated as a cylindrical surface
        /// starting from the base circle.
        pub zbasis: [f32; 3],
    }

    /// Abstract surface data.
    #[derive(Clone, Debug, Deserialize, Serialize, gltf_derive::Validate)]
    #[serde(rename_all = "camelCase")]
    pub struct Surface {
        /// Discriminant.
        #[serde(rename = "type")]
        pub type_: Checked<Type>,
        /// Optional name for this surface.
        #[cfg(feature = "names")]
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub name: Option<String>,
        /// Arguments for a cylindrical surface.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub cylinder: Option<Cylinder>,
        /// Arguments for a NURBS surface.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub nurbs: Option<Nurbs>,
        /// Arguments for a planar surface.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub plane: Option<Plane>,
        /// Arguments for a toroidal surface.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub torus: Option<Torus>,
        /// Surface parameter domain.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub domain: Option<Domain>,
    }
}

/// Solid boundary representations.
pub mod brep {
    use crate::validation::{Error, Validate};
    use crate::{Index, Path, Root};
    use gltf_derive::Validate;
    use serde_derive::{Deserialize, Serialize};

    /// Used to prevent serializing false boolean values.
    fn is_false(condition: &bool) -> bool {
        !*condition
    }

    /// Parameter curve in 2D space.
    #[derive(Clone, Debug, Deserialize, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    pub struct Trim {
        /* TODO:
        /// The trim curve geometry in 2D (or homogeneous 3D) space.
        pub curve: Index<super::Curve>,

        /// Trim start vertex.
        pub start: Index<Vertex>,

        /// Trim end vertex.
        pub end: Index<Vertex>,
         */
        /// Specifies whether the trim curve geometry is in the opposite
        /// direction with respect to the edge curve geometry.
        pub reverse: bool,

        /// The corresponding edge in 3D space this trim is paired with.
        pub edge: Index<Edge>,
    }

    /// Pair of vertices on a face plus an optional trim domain.
    #[derive(Clone, Debug, Deserialize, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    pub struct Edge {
        /// The edge curve geometry in 3D (or homogeneous 4D) space.
        pub curve: Index<super::Curve>,

        /// Edge start vertex.
        pub start: Index<EdgeVertex>,

        /// Edge end vertex.
        pub end: Index<EdgeVertex>,

        /// Specifies whether the orientation of the edge is reversed
        /// with respect to its associated curve.
        #[serde(default, skip_serializing_if = "is_false")]
        pub reverse: bool,

        /// Optional domain to select a subset of the edge curve geometry.
        ///
        /// When `None`, the domain is the same as the edge curve geometry
        /// domain.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub subdomain: Option<super::curve::Domain>,
    }

    /// Point in 2D space.
    #[derive(Clone, Debug, Deserialize, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct TrimVertex(pub [f32; 2]);

    impl Validate for TrimVertex {
        fn validate<P, R>(&self, _root: &Root, _path: P, _report: &mut R)
        where
            P: Fn() -> Path,
            R: FnMut(&dyn Fn() -> Path, Error),
        {
        }
    }

    /// Point in 3D space.
    #[derive(Clone, Debug, Deserialize, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct EdgeVertex(pub [f32; 3]);

    impl Validate for EdgeVertex {
        fn validate<P, R>(&self, _root: &Root, _path: P, _report: &mut R)
        where
            P: Fn() -> Path,
            R: FnMut(&dyn Fn() -> Path, Error),
        {
        }
    }

    /// Set of trim curves on a surface.
    #[derive(Clone, Debug, Deserialize, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    pub struct Loop {
        /// The trim curves forming the loop.
        pub trims: Vec<Trim>,

        /// Specifies whether the winding order of the loop should be
        /// interpreted in reverse order with respect to the face.
        #[serde(default, skip_serializing_if = "is_false")]
        pub reverse: bool,
    }

    /// Set of loops defined on an abstract surface.
    #[derive(Clone, Debug, Deserialize, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    pub struct Face {
        /// Surface the face edges and vertices reside on.
        pub surface: Index<super::Surface>,

        /// Face outer bound.
        pub outer_loop: Loop,

        /// Face inner bounds.
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub inner_loops: Vec<Loop>,

        /// Specifies whether the orientation of the face is in
        /// reverse order with respect to its surface.
        #[serde(default, skip_serializing_if = "is_false")]
        pub reverse: bool,
    }

    /// Solid boundary representation structure.
    #[derive(Clone, Debug, Deserialize, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    pub struct BRep {
        /// Array of faces forming a solid.
        pub faces: Vec<Face>,

        /// Optional name for this boundary representation.
        #[cfg(feature = "names")]
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub name: Option<String>,

        /// Optional mesh approximation of this solid.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub mesh: Option<Index<crate::Mesh>>,
    }
}

pub use brep::BRep;
pub use curve::Curve;
pub use surface::Surface;
