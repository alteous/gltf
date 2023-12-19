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

/// Vector with 2-3 components.
#[derive(Clone, Copy, Debug, Deserialize, JsonSchema, PartialEq, Serialize)]
pub struct Vector(
    /// The 'x' component.
    pub f64,
    /// The 'y' component.
    pub f64,
    /// The 'z' component for 3D space.
    pub Option<f64>,
);

crate::impl_validate_nop!(Vector);

impl Vector {
    /// [0.0, 0.0]
    pub const ZERO_2D: Self = Self(0.0, 0.0, None);

    /// [0.0, 0.0, 0.0]
    pub const ZERO_3D: Self = Self(0.0, 0.0, Some(0.0));

    /// [1.0, 0.0]
    pub const I_2D: Self = Self(1.0, 0.0, None);

    /// [1.0, 0.0, 0.0]
    pub const I_3D: Self = Self(1.0, 0.0, Some(0.0));

    /// [0.0, 1.0]
    pub const J_2D: Self = Self(0.0, 1.0, None);

    /// [0.0, 1.0, 0.0]
    pub const J_3D: Self = Self(0.0, 1.0, Some(0.0));

    /// [0.0, 0.0, 1.0]
    pub const K_3D: Self = Self(0.0, 0.0, Some(1.0));

    /// Queries the vector dimensionality.
    pub fn is_2d(&self) -> bool {
        !self.is_3d()
    }

    /// Queries the vector dimensionality.
    pub fn is_3d(&self) -> bool {
        self.2.is_some()
    }
}

impl From<[f64; 2]> for Vector {
    fn from([x, y]: [f64; 2]) -> Self {
        Self(x, y, None)
    }
}

impl From<&[f64; 2]> for Vector {
    fn from(v: &[f64; 2]) -> Self {
        (*v).into()
    }
}

impl From<[f64; 3]> for Vector {
    fn from([x, y, z]: [f64; 3]) -> Self {
        Self(x, y, Some(z))
    }
}

impl From<&[f64; 3]> for Vector {
    fn from(v: &[f64; 3]) -> Self {
        (*v).into()
    }
}

impl From<Vector> for [f64; 2] {
    fn from(Vector(x, y, _): Vector) -> Self {
        [x, y]
    }
}

impl From<&Vector> for [f64; 2] {
    fn from(v: &Vector) -> Self {
        (*v).into()
    }
}

impl From<Vector> for [f64; 3] {
    fn from(Vector(x, y, z): Vector) -> Self {
        [x, y, z.unwrap_or_default()]
    }
}

impl From<&Vector> for [f64; 3] {
    fn from(v: &Vector) -> Self {
        (*v).into()
    }
}

/// 2D and 3D curve definitions.
pub mod curve {
    use super::{Axes, Vector};
    use crate::validation::{Error, Validate};
    use crate::Root;
    use gltf_derive::Validate;
    use schemars::JsonSchema;
    use serde_derive::{Deserialize, Serialize};

    /// Discriminant for `Curve` data.
    #[derive(Clone, Copy, Debug, Deserialize, JsonSchema, Eq, PartialEq, Serialize)]
    #[schemars(rename = "curve.type")]
    #[serde(rename_all = "camelCase")]
    pub enum Type {
        /// Circle curve.
        Circle,
        /// Line curve.
        Line,
        /// NURBS curve.
        Nurbs,
    }

    crate::impl_validate_nop!(Type);

    /// Circular curve definition.
    ///
    /// λ(u) := O + R(cos(u)x + sin(u)y), where:
    /// * O = `self.origin`,
    /// * R = `self.radius`,
    /// * x = `self.axes.x`,
    /// * y = `self.axes.y`.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "curve.circle")]
    pub struct Circle {
        /// Local co-ordinate axes.
        #[serde(flatten)]
        pub axes: Axes,

        /// Position at the center of the circle.
        pub origin: Vector,

        /// Distance from the center position to all points on the circle.
        pub radius: f64,
    }

    impl Circle {
        /// Queries the dimensionality of the geometry data.
        pub fn is_3d(&self) -> bool {
            self.origin.is_3d()
        }
    }

    /// Line curve definition.
    ///
    /// λ(u) := O + ux, where:
    /// * O = `self.origin`,
    /// * x = `self.direction`.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "curve.line")]
    pub struct Line {
        /// Origin position.
        pub origin: Vector,

        /// Unit vector pointing away from the origin position.
        pub direction: Vector,
    }

    impl Line {
        /// Queries the dimensionality of the geometry data.
        pub fn is_3d(&self) -> bool {
            self.origin.is_3d()
        }
    }

    /// Non-uniform rational basis spline curve definition.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "curve.nurbs")]
    pub struct Nurbs {
        /// Array of control vertices.
        pub control_points: Vec<Vector>,

        /// Knot vector..
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub knot_vector: Vec<f64>,

        /// Weights accompanying control points.
        ///
        /// May be omitted for non-rational B-splines.
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub weights: Vec<f64>,

        /// Order of the basis splines.
        pub order: u32,
    }

    impl Nurbs {
        /// Queries the dimensionality of the geometry data.
        pub fn is_3d(&self) -> bool {
            self.control_points.first().unwrap().is_3d()
        }
    }

    impl Validate for Nurbs {
        fn validate<P, R>(&self, _root: &Root, path: P, report: &mut R)
        where
            P: Fn() -> crate::Path,
            R: FnMut(&dyn Fn() -> crate::Path, Error),
        {
            if !self.weights.is_empty() && self.weights.len() != self.control_points.len() {
                report(&|| path().field("weights"), Error::Invalid);
            }
        }
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
        /// Queries the dimensionality of the geometry data.
        pub fn is_3d(&self) -> bool {
            match self {
                Self::Circle(circle) => circle.is_3d(),
                Self::Line(line) => line.is_3d(),
                Self::Nurbs(nurbs) => nurbs.is_3d(),
            }
        }

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
    ///
    /// The dimensionality of the curve is implicit. If the curve is in 3D space, then
    /// every vector in the curve definition must have three components. Similarly, if
    /// the curve is in 2D space, then every vector must have two components.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "curve")]
    pub struct Curve {
        /// Discriminant.
        #[serde(rename = "type")]
        pub type_: Type,

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
    use super::Axes;
    use crate::validation::{Error, Validate};
    use crate::Root;
    use gltf_derive::Validate;
    use schemars::JsonSchema;
    use serde_derive::{Deserialize, Serialize};

    /// Discriminant for `Surface` data.
    #[derive(Clone, Copy, Debug, Deserialize, JsonSchema, Eq, PartialEq, Serialize)]
    #[serde(rename_all = "camelCase")]
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

    crate::impl_validate_nop!(Type);

    /// Parametric cylindrical surface definition.
    ///
    /// σ(u, v) := O + R(cos(u)x + sin(u)y) + vz, where:
    /// * O = `self.origin`,
    /// * R = `self.radius`,
    /// * x = `self.axes.x`,
    /// * y = `self.axes.y`,
    /// * z = `self.axes.x` × `self.axes.y`.
    ///
    /// Cylinders are defined in reference to a circle that is extruded
    /// along the circle normal vector.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "surface.cylinder")]
    pub struct Cylinder {
        /// Local co-ordinate axes.
        #[serde(flatten)]
        pub axes: Axes,

        /// Position at the center of the circle.
        pub origin: [f64; 3],

        /// Distance from the center position to all points on the circle.
        pub radius: f64,
    }

    /// NURBS surface definition.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "surface.nurbs")]
    pub struct Nurbs {
        /// Matrix of control point vertices.
        pub control_points: Vec<[f64; 3]>,
        /// Dimensions of control point vertex matrix.
        pub num_control_points: [u32; 2],
        /// Number of knots in U and V.
        pub num_knots: [u32; 2],
        /// Knot vector.
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub knot_vector: Vec<f64>,
        /// Weights.
        ///
        /// May be omitted for non-rational B-splines.
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub weights: Vec<f64>,
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

            if !self.weights.is_empty() && self.weights.len() != self.control_points.len() {
                report(&|| path().field("weights"), Error::Invalid);
            }
        }
    }

    /// Plane surface definition.
    ///
    /// σ(u, v) := O + ux + vy, where:
    /// * O = `self.origin`,
    /// * x = `self.axes.x`,
    /// * y = `self.axes.y`.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, gltf_derive::Validate)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "surface.plane")]
    pub struct Plane {
        /// Local co-ordinate axes.
        #[serde(flatten)]
        pub axes: Axes,

        /// An arbitrary point that lies on the plane.
        pub origin: [f64; 3],
    }

    /// Parametric spherical surface definition.
    ///
    /// σ(u, v) := O + Rcos(v)(cos(u)x + sin(u)y) + Rsin(v)z, where:
    /// * O = `self.origin`,
    /// * R = `self.radius`,
    /// * x = `self.axes.x`,
    /// * y = `self.axes.y`,
    /// * z = `self.axes.x` × `self.axes.y`.
    ///
    /// Spheres are defined in reference to a circle at zero inclination.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "surface.sphere")]
    pub struct Sphere {
        /// Local co-ordinate axes.
        #[serde(flatten)]
        pub axes: Axes,

        /// Position at the center of the circle.
        pub origin: [f64; 3],

        /// Distance from the center position to all points on the circle.
        pub radius: f64,
    }

    /// Toroidal surface definition.
    ///
    /// σ(u, v) := O + (R + rcos(v))(cos(u)x + sin(u)y) + rsin(v)z, where:
    /// * O = `self.origin`,
    /// * R = `self.major_radius`,
    /// * r = `self.minor_radius`,
    /// * x = `self.axes.x`,
    /// * y = `self.axes.y`,
    /// * z = `self.axes.x` × `self.axes.y`.
    ///
    /// Tori are defined in reference to a circle that is revolved about
    /// an origin at a specified distance. This distance is called the
    /// major radius. The radius of the circle of revolution is called the
    /// minor radius.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, gltf_derive::Validate)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "surface.torus")]
    pub struct Torus {
        /// Local co-ordinate axes.
        #[serde(flatten)]
        pub axes: Axes,

        /// The center of the torus.
        ///
        /// The axis of revolution passes through the origin of the torus.
        pub origin: [f64; 3],

        /// Distance from the torus origin to the origin of the revolved circle.
        pub major_radius: f64,

        /// Distance of points away from the center of the revolved circle.
        pub minor_radius: f64,
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
        pub type_: Type,
        /// Optional name for this surface.
        #[cfg(feature = "names")]
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub name: Option<String>,
        /// Specific surface data.
        #[serde(flatten)]
        pub geometry: Geometry,
    }
}

/// Defines a local 2D or 3D co-ordinate system.
///
/// In 3D systems, the 'z' axis is inferred from the cross product of the 'x' and 'y' axes.
#[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
#[schemars(rename = "axes")]
pub struct Axes {
    /// The 'x' co-ordinate axis.
    #[serde(default, rename = "xAxis", skip_serializing_if = "Option::is_none")]
    pub x: Option<Vector>,

    /// The 'y' co-ordinate axis.
    #[serde(default, rename = "yAxis", skip_serializing_if = "Option::is_none")]
    pub y: Option<Vector>,
}

impl Axes {
    /// Default axes in 2D space.
    ///
    /// X: `[1.0, 0.0]`
    /// Y: `[0.0, 1.0]`
    pub const DEFAULT_2D: Self = Self {
        x: Some(Vector::I_2D),
        y: Some(Vector::J_2D),
    };

    /// Default axes in 3D space.
    ///
    /// X: `[1.0, 0.0, 0.0]`
    /// Y: `[0.0, 1.0, 0.0]`
    /// Z: `[0.0, 0.0, 1.0]` (implicit)
    pub const DEFAULT_3D: Self = Self {
        x: Some(Vector::I_3D),
        y: Some(Vector::J_3D),
    };
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
pub struct Vertex(
    /// `[x, y, z]` co-ordinate.
    pub [f64; 3],
);

crate::impl_validate_nop!(Vertex);

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
pub struct IndexWithOrientation<T: Validate>(
    /// Index referencing an orientable item.
    pub Index<T>,
    /// Selected orientation of the referenced item.
    #[serde(default)]
    pub Orientation,
);

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
pub struct Interval(
    /// Minimum value.
    pub f64,
    /// Maximum value.
    pub f64,
);

crate::impl_validate_nop!(Interval);

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
