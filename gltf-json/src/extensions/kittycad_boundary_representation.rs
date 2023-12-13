//! Boundary representations of solid objects

use crate::validation::{Error, Validate};
use crate::{Index, Root};
use gltf_derive::Validate;
use schemars::gen::SchemaGenerator;
use schemars::schema::Schema;
use schemars::JsonSchema;
use serde_derive::{Deserialize, Serialize};
use serde_repr::{Deserialize_repr, Serialize_repr};

// Used to skip serialization of `false` values.
fn bool_is_false(b: &bool) -> bool {
    !*b
}

/// 2D and 3D curve definitions.
pub mod curve {
    use crate::validation::{Checked, Error, Validate};
    use crate::Root;
    use gltf_derive::Validate;
    use schemars::JsonSchema;
    use serde::{de, ser};
    use serde_derive::{Deserialize, Serialize};
    use std::fmt;

    pub const VALID_CURVE_TYPES: &[&str] = &["circle", "linear", "nurbs"];

    /// Discriminant for `Curve` data.
    #[derive(Clone, Copy, Debug, Deserialize, JsonSchema, Eq, PartialEq)]
    #[schemars(rename = "curve.type")]
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
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "curve.circle")]
    pub struct Circle {
        /// Position at the center of the circle.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub origin: Option<[f64; 3]>,

        /// Distance from the center position to all points on the circle.
        pub radius: f64,

        /// Unit vector normal to the plane containing the circle.
        ///
        /// This serves as the Z basis in the parametric co-ordinate space.
        pub normal: [f64; 3],

        /// Unit vector in the direction from the origin to the point on
        /// the circle evaluated at λ(0).
        ///
        /// Due to floating point precision, this vector may not lie exactly
        /// in the plane. If this is the case then the X vector will be treated
        /// as the projection of this vector onto the plane.
        pub xbasis: [f64; 3],
    }

    /// Line curve definition.
    ///
    /// Either end or direction must be set.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "curve.line")]
    pub struct Line {
        /// Origin position.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub origin: Option<[f64; 3]>,

        /// Unit vector pointing away from the origin position.
        pub direction: [f64; 3],
    }

    /// NURBS curve definition.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "curve.nurbs")]
    pub struct Nurbs {
        /// Array of control vertices.
        pub control_points: Vec<[f64; 4]>,
        /// Knot vector.
        pub knot_vector: Vec<f64>,
        /// Order of basis splines.
        pub order: u32,
    }

    /// Specific curve data.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "curve.geometry")]
    pub enum Geometry {
        /// Circle curve.
        Circle(Circle),
        /// Line curve.
        Line(Line),
        /// NURBS curve.
        Nurbs(Nurbs),
    }

    impl Geometry {
        /// Returns the corresponding type for the geometry variant.
        pub fn type_(&self) -> Type {
            match self {
                Self::Circle(_) => Type::Circle,
                Self::Line(_) => Type::Line,
                Self::Nurbs(_) => Type::Nurbs,
            }
        }
    }

    impl Validate for Geometry {
        fn validate<P, R>(&self, root: &Root, path: P, report: &mut R)
        where
            P: Fn() -> crate::Path,
            R: FnMut(&dyn Fn() -> crate::Path, Error),
        {
            match self {
                Self::Circle(circle) => circle.validate(root, || path().field("circle"), report),
                Self::Line(line) => line.validate(root, || path().field("line"), report),
                Self::Nurbs(nurbs) => nurbs.validate(root, || path().field("nurbs"), report),
            }
        }
    }

    /// Abstract curve data.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "curve")]
    pub struct Curve {
        /// Discriminant.
        #[serde(rename = "type")]
        pub type_: Checked<Type>,

        /// Optional name for this surface.
        #[cfg(feature = "names")]
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub name: Option<String>,

        /// Specific curve data.
        #[serde(flatten)]
        pub geometry: Geometry,
    }
}

/// 3D surface definitions.
pub mod surface {
    use crate::validation::{Checked, Error, Validate};
    use crate::Root;
    use gltf_derive::Validate;
    use schemars::JsonSchema;
    use serde::{de, ser};
    use serde_derive::{Deserialize, Serialize};
    use std::fmt;

    pub const VALID_SURFACE_TYPES: &[&str] = &["cylinder", "nurbs", "plane", "sphere", "torus"];

    /// Discriminant for `Surface` data.
    #[derive(Clone, Copy, Debug, Deserialize, JsonSchema, Eq, PartialEq)]
    #[schemars(rename = "surface.type")]
    pub enum Type {
        /// Cylindrical surface.
        Cylinder = 1,
        /// NURBS surface.
        Nurbs,
        /// Planar surface.
        Plane,
        /// Spherical surface.
        Sphere,
        /// Torus surface.
        Torus,
    }

    impl Type {
        pub fn as_str(self) -> &'static str {
            match self {
                Type::Cylinder => "cylinder",
                Type::Nurbs => "nurbs",
                Type::Plane => "plane",
                Type::Sphere => "sphere",
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
                        "sphere" => Checked::Valid(Type::Sphere),
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
    /// * O = `self.circle.origin`,
    /// * R = `self.circle.radius`,
    /// * x = `self.circle.xbasis`,
    /// * y = `self.circle.normal` × `self.circle.xbasis`,
    /// * z = `self.circle.normal`,
    /// * h = `self.height`,
    /// * u ∈ {0, 2π},
    /// * v ∈ {0, h}.
    ///
    /// Cylinders are defined in reference to a circle that is extruded
    /// along the circle normal vector.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "surface.cylinder")]
    pub struct Cylinder {
        /// The extruded circle.
        pub circle: super::curve::Circle,
        /// Height of the extruded circle.
        pub height: f64,
    }

    /// NURBS surface definition.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "surface.nurbs")]
    pub struct Nurbs {
        /// Matrix of control point vertices.
        pub control_points: Vec<[f64; 4]>,
        /// Dimensions of control point vertex matrix.
        pub num_control_points: [u32; 2],
        /// Number of knots in U and V.
        pub num_knots: [u32; 2],
        /// Knot vector.
        pub knot_vector: Vec<f64>,
        /// Order of basis splines.
        pub order: [u32; 2],
    }

    impl Validate for Nurbs {
        fn validate<P, R>(&self, _root: &Root, path: P, report: &mut R)
        where
            P: Fn() -> crate::Path,
            R: FnMut(&dyn Fn() -> crate::Path, Error),
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
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, gltf_derive::Validate)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "surface.plane")]
    pub struct Plane {
        /// Normal vector to the plane.
        pub normal: [f64; 3],
        /// An arbitrary point that lies on the plane.
        pub point: [f64; 3],
    }

    /// Parametric spherical surface definition.
    ///
    /// σ(u, v) := O + Rcos(v)(cos(u)x + sin(u)y) + Rsin(v)z, where:
    /// * O = `self.horizon.origin`,
    /// * R = `self.horizon.radius`,
    /// * x = `self.horizon.xbasis`,
    /// * y = `self.horizon.normal` × `self.horizon.xbasis`,
    /// * z = `self.horizon.normal`,
    /// * u ∈ {0, 2π},
    /// * v ∈ {0, 2π}.
    ///
    /// Spheres are defined in reference to a circle at zero inclination.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "surface.sphere")]
    pub struct Sphere {
        /// The circle at zero inclination.
        pub horizon: super::curve::Circle,
    }

    /// Toroidal surface definition.
    ///
    /// σ(u, v) := O + (R + rcos(v))(cos(u)x + sin(u)y) + rsin(v)z, where:
    /// * O = `self.origin`,
    /// * R = `self.radius`,
    /// * r = `self.circle.radius`,
    /// * x = `self.circle.xbasis`,
    /// * y = `self.circle.normal` × `self.circle.xbasis`,
    /// * z = `self.circle.normal`,
    /// * u, v ∈ {0, 2π}.
    ///
    /// Tori are defined in reference to a circle that is revolved about
    /// an origin at a specified distance. This distance is called the
    /// major radius. The radius of the circle of revolution is called the
    /// minor radius.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, gltf_derive::Validate)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "surface.torus")]
    pub struct Torus {
        /// The center of the torus.
        ///
        /// The axis of revolution passes through the origin of the torus.
        pub origin: [f64; 3],
        /// Circle of revolution.
        pub circle: super::curve::Circle,
        /// Distance from the torus origin to the origin of the revolved circle.
        pub radius: f64,
    }

    /// Specific surface data.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "surface.geometry")]
    pub enum Geometry {
        /// Cylindrical surface.
        Cylinder(Cylinder),
        /// NURBS surface.
        Nurbs(Nurbs),
        /// Planar surface.
        Plane(Plane),
        /// Spherical surface.
        Sphere(Sphere),
        /// Toroidal surface.
        Torus(Torus),
    }

    impl Geometry {
        /// Returns the corresponding type for the geometry variant.
        pub fn type_(&self) -> Type {
            match self {
                Self::Cylinder(_) => Type::Cylinder,
                Self::Nurbs(_) => Type::Nurbs,
                Self::Plane(_) => Type::Plane,
                Self::Sphere(_) => Type::Sphere,
                Self::Torus(_) => Type::Torus,
            }
        }
    }

    impl Validate for Geometry {
        fn validate<P, R>(&self, root: &Root, path: P, report: &mut R)
        where
            P: Fn() -> crate::Path,
            R: FnMut(&dyn Fn() -> crate::Path, Error),
        {
            match self {
                Self::Cylinder(cylinder) => {
                    cylinder.validate(root, || path().field("cylinder"), report)
                }
                Self::Nurbs(nurbs) => nurbs.validate(root, || path().field("nurbs"), report),
                Self::Plane(plane) => plane.validate(root, || path().field("plane"), report),
                Self::Sphere(sphere) => sphere.validate(root, || path().field("sphere"), report),
                Self::Torus(torus) => torus.validate(root, || path().field("torus"), report),
            }
        }
    }

    /// Abstract surface data.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, gltf_derive::Validate)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "surface")]
    pub struct Surface {
        /// Discriminant.
        #[serde(rename = "type")]
        pub type_: Checked<Type>,
        /// Optional name for this surface.
        #[cfg(feature = "names")]
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub name: Option<String>,
        /// Specific surface data.
        #[serde(flatten)]
        pub geometry: Geometry,
    }
}

/// Pair of vertices on a face with an accompanying 3D curve..
#[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
#[serde(rename_all = "camelCase")]
#[schemars(rename = "edge")]
pub struct Edge {
    /// The edge curve geometry in 3D (or homogeneous 4D) space.
    pub curve: IndexWithOrientation<Curve>,

    /// Edge start vertex.
    pub start: Option<Index<Vertex>>,

    /// Edge end vertex.
    pub end: Option<Index<Vertex>>,

    /// Marker for a closed edge.
    #[serde(default, skip_serializing_if = "bool_is_false")]
    pub closed: bool,

    /// Interval for the curve's 't' parameter.
    pub t: Interval,
}

/// Junctions of edges in 3D space.
#[derive(Clone, Debug, Deserialize, JsonSchema, Serialize)]
#[serde(rename_all = "camelCase")]
#[schemars(rename = "vertex")]
pub struct Vertex(pub [f64; 3]);

impl Validate for Vertex {
    fn validate<P, R>(&self, _root: &Root, _path: P, _report: &mut R)
    where
        P: Fn() -> crate::Path,
        R: FnMut(&dyn Fn() -> crate::Path, Error),
    {
    }
}

/// Selected orientation of an orientable item.
#[derive(
    Clone, Copy, Debug, Default, Deserialize_repr, Eq, JsonSchema, PartialEq, Serialize_repr,
)]
#[repr(i8)]
#[schemars(rename = "orientation")]
pub enum Orientation {
    /// Same-sense orientation.
    #[default]
    Same = 1,

    /// Reverse-sense orientation.
    Reverse = -1,
}

impl Orientation {
    /// Query whether the orientation is in the same-sense state.
    pub fn is_same(self) -> bool {
        matches!(self, Orientation::Same)
    }

    /// Query whether the orientation is in the reverse-sense state.
    pub fn is_reverse(self) -> bool {
        matches!(self, Orientation::Reverse)
    }
}

/// Index for orientable items.
///
/// The JSON representation is an array of two numbers: the index followed by its orientation.
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub struct IndexWithOrientation<T: Validate>(pub Index<T>, #[serde(default)] pub Orientation);

impl<T: Validate> IndexWithOrientation<T> {
    /// Explicit constructor.
    pub fn new(index: Index<T>, orientation: Orientation) -> Self {
        Self(index, orientation)
    }

    /// Create an index with same-sense orientation.
    pub fn same(index: Index<T>) -> Self {
        Self(index, Orientation::Same)
    }

    /// Create an index with reverse-sense orientation.
    pub fn reverse(index: Index<T>) -> Self {
        Self::new(index, Orientation::Reverse)
    }

    /// Returns the index.
    pub fn index(&self) -> Index<T> {
        self.0
    }

    /// Returns the orientation.
    pub fn orientation(&self) -> Orientation {
        self.1
    }

    /// Query whether the index has same-sense orientation.
    pub fn is_same(&self) -> bool {
        self.1.is_same()
    }

    /// Query whether the index has reverse-sense orientation.
    pub fn is_reverse(&self) -> bool {
        self.1.is_reverse()
    }
}

impl<T: Validate> From<(Index<T>, Orientation)> for IndexWithOrientation<T> {
    fn from((index, orientation): (Index<T>, Orientation)) -> Self {
        IndexWithOrientation(index, orientation)
    }
}

impl<T: Validate> From<IndexWithOrientation<T>> for (Index<T>, Orientation) {
    fn from(item: IndexWithOrientation<T>) -> Self {
        (item.index(), item.orientation())
    }
}

impl<T: Validate> JsonSchema for IndexWithOrientation<T> {
    fn schema_name() -> String {
        "indexWithOrientation".to_owned()
    }

    fn json_schema(generator: &mut SchemaGenerator) -> Schema {
        #[derive(Deserialize, JsonSchema, Serialize)]
        #[schemars(rename = "indexWithOrientation")]
        struct NonGenericIndexWithOrientation(pub u32, #[serde(default)] pub Orientation);
        NonGenericIndexWithOrientation::json_schema(generator)
    }
}

impl<T: Validate> Validate for IndexWithOrientation<T>
where
    crate::Root: crate::root::Get<T>,
{
    fn validate<P, R>(&self, root: &Root, path: P, report: &mut R)
    where
        P: Fn() -> crate::Path,
        R: FnMut(&dyn Fn() -> crate::Path, Error),
    {
        self.0.validate(root, path, report);
    }
}

/// A pair of endpoint arguments for a single parameter.
///
/// The interval is closed at both ends.
#[derive(Clone, Debug, Deserialize, JsonSchema, Serialize)]
#[serde(rename_all = "camelCase")]
#[schemars(rename = "interval")]
pub struct Interval(pub f64, pub f64);

impl Interval {
    /// [0.0, 1.0]
    pub fn unit() -> Self {
        Self(0.0, 1.0)
    }

    /// [0.0, 2π]
    pub fn turn() -> Self {
        Self(0.0, std::f64::consts::TAU)
    }

    /// The minimum value.
    pub fn min(&self) -> f64 {
        self.0
    }

    /// The maximum value.
    pub fn max(&self) -> f64 {
        self.1
    }
}

impl Validate for Interval {
    fn validate<P, R>(&self, _root: &Root, _path: P, _report: &mut R)
    where
        P: Fn() -> crate::Path,
        R: FnMut(&dyn Fn() -> crate::Path, Error),
    {
    }
}

/// Curve tracing the path of an edge in 2D surface space.
#[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
pub struct Trace {
    /// The trace curve geometry in 2D (or homogeneous 3D) space.
    pub curve: IndexWithOrientation<Curve>,

    /// Marker for a closed trace.
    #[serde(default, skip_serializing_if = "bool_is_false")]
    pub closed: bool,

    /// Interval for the curve 't' parameter.
    pub t: Interval,
}

/// Edge loop.
#[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
#[serde(rename_all = "camelCase")]
#[schemars(rename = "loop")]
pub struct Loop {
    /// Oriented edges forming the loop.
    pub edges: Vec<IndexWithOrientation<Edge>>,

    /// Optional 1:1 pairing of traces to edges.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub traces: Vec<Trace>,
}

/// Set of loops defined on an abstract surface.
#[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
#[serde(rename_all = "camelCase")]
#[schemars(rename = "face")]
pub struct Face {
    /// Surface the face edges and vertices reside on.
    pub surface: IndexWithOrientation<Surface>,

    /// Face bounds.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub loops: Vec<IndexWithOrientation<Loop>>,
}

/// Boundary representation volume.
#[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
#[serde(rename_all = "camelCase")]
#[schemars(rename = "shell")]
pub struct Shell {
    /// Set of connected faces forming a closed 'watertight' volume.
    pub faces: Vec<IndexWithOrientation<Face>>,

    /// Optional name for this shell.
    #[cfg(feature = "names")]
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
}

/// Solid boundary representation structure.
#[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
#[serde(rename_all = "camelCase")]
#[schemars(rename = "solid")]
pub struct Solid {
    /// The boundaries of the solid volume.
    pub shells: Vec<IndexWithOrientation<Shell>>,

    /// Optional name for this solid.
    #[cfg(feature = "names")]
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,

    /// Optional mesh approximation of this solid.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub mesh: Option<Index<crate::Mesh>>,
}

pub use curve::Curve;
pub use surface::Surface;
