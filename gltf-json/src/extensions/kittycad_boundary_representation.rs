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

#[doc(inline)]
pub use curve::{Curve2d, Curve3d};

#[doc(inline)]
pub use surface::Surface;

/// 2D and 3D curve definitions.
pub mod curve {
    use super::{Axes2d, Axes3d};
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
    #[schemars(rename = "curve.circle2D")]
    pub struct Circle2d {
        /// Local co-ordinate axes.
        #[serde(default, flatten, skip_serializing_if = "Option::is_none")]
        pub axes: Option<Axes2d>,

        /// Position at the center of the circle.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub origin: Option<[f64; 2]>,

        /// Distance from the center position to all points on the circle.
        pub radius: f64,
    }

    /// Circular curve definition.
    ///
    /// λ(u) := O + R(cos(u)x + sin(u)y), where:
    /// * O = `self.origin`,
    /// * R = `self.radius`,
    /// * x = `self.axes.x`,
    /// * y = `self.axes.y`.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "curve.circle3D")]
    pub struct Circle3d {
        /// Local co-ordinate axes.
        #[serde(default, flatten, skip_serializing_if = "Option::is_none")]
        pub axes: Option<Axes3d>,

        /// Position at the center of the circle.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub origin: Option<[f64; 3]>,

        /// Distance from the center position to all points on the circle.
        pub radius: f64,
    }

    /// Line curve definition.
    ///
    /// λ(u) := O + ux, where:
    /// * O = `self.origin`,
    /// * x = `self.direction`.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "curve.line2D")]
    pub struct Line2d {
        /// Origin position.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub origin: Option<[f64; 2]>,

        /// Unit vector pointing away from the origin position.
        pub direction: [f64; 2],
    }

    /// Line curve definition.
    ///
    /// λ(u) := O + ux, where:
    /// * O = `self.origin`,
    /// * x = `self.direction`.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "curve.line3D")]
    pub struct Line3d {
        /// Origin position.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub origin: Option<[f64; 3]>,

        /// Unit vector pointing away from the origin position.
        pub direction: [f64; 3],
    }

    /// Non-uniform rational basis spline curve definition.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "curve.nurbs2D")]
    pub struct Nurbs2d {
        /// Array of control vertices.
        pub control_points: Vec<[f64; 2]>,

        /// Order of the basis splines.
        pub order: u32,

        /// Knot vector.
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub knot_vector: Vec<f64>,

        /// Weights accompanying control points.
        ///
        /// May be omitted for non-rational B-splines.
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub weights: Vec<f64>,
    }

    impl Validate for Nurbs2d {
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

    /// Non-uniform rational basis spline curve definition.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "curve.nurbs3D")]
    pub struct Nurbs3d {
        /// Array of control vertices.
        pub control_points: Vec<[f64; 3]>,

        /// Order of the basis splines.
        pub order: u32,

        /// Knot vector.
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub knot_vector: Vec<f64>,

        /// Weights accompanying control points.
        ///
        /// May be omitted for non-rational B-splines.
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub weights: Vec<f64>,
    }

    impl Validate for Nurbs3d {
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
    #[schemars(rename = "curve.geometry2D")]
    pub enum Geometry2d {
        /// Circle curve.
        Circle(Circle2d),
        /// Line curve.
        Line(Line2d),
        /// NURBS curve.
        Nurbs(Nurbs2d),
    }

    impl Geometry2d {
        /// Returns the corresponding type for the geometry variant.
        pub fn type_(&self) -> Type {
            match self {
                Self::Circle(_) => Type::Circle,
                Self::Line(_) => Type::Line,
                Self::Nurbs(_) => Type::Nurbs,
            }
        }
    }

    impl Validate for Geometry2d {
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

    /// Specific curve data.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "curve.geometry3D")]
    pub enum Geometry3d {
        /// Circle curve.
        Circle(Circle3d),
        /// Line curve.
        Line(Line3d),
        /// NURBS curve.
        Nurbs(Nurbs3d),
    }

    impl Geometry3d {
        /// Returns the corresponding type for the geometry variant.
        pub fn type_(&self) -> Type {
            match self {
                Self::Circle(_) => Type::Circle,
                Self::Line(_) => Type::Line,
                Self::Nurbs(_) => Type::Nurbs,
            }
        }
    }

    impl Validate for Geometry3d {
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
    #[schemars(rename = "curve3D")]
    pub struct Curve2d {
        /// Discriminant.
        #[serde(rename = "type")]
        pub type_: Type,

        /// Optional name for this surface.
        #[cfg(feature = "names")]
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub name: Option<String>,

        /// Specific curve data.
        #[serde(flatten)]
        pub geometry: Geometry2d,
    }

    /// Abstract curve data.
    #[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
    #[serde(rename_all = "camelCase")]
    #[schemars(rename = "curve3D")]
    pub struct Curve3d {
        /// Discriminant.
        #[serde(rename = "type")]
        pub type_: Type,

        /// Optional name for this surface.
        #[cfg(feature = "names")]
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub name: Option<String>,

        /// Specific curve data.
        #[serde(flatten)]
        pub geometry: Geometry3d,
    }
}

/// 3D surface definitions.
pub mod surface {
    use super::Axes3d;
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
        #[serde(default, flatten, skip_serializing_if = "Option::is_none")]
        pub axes: Option<Axes3d>,

        /// Position at the center of the circle.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub origin: Option<[f64; 3]>,

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
        #[serde(default, flatten, skip_serializing_if = "Option::is_none")]
        pub axes: Option<Axes3d>,

        /// An arbitrary point that lies on the plane.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub origin: Option<[f64; 3]>,
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
        #[serde(default, flatten, skip_serializing_if = "Option::is_none")]
        pub axes: Option<Axes3d>,

        /// Position at the center of the circle.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub origin: Option<[f64; 3]>,

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
        #[serde(default, flatten, skip_serializing_if = "Option::is_none")]
        pub axes: Option<Axes3d>,

        /// The center of the torus.
        ///
        /// The axis of revolution passes through the origin of the torus.
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub origin: Option<[f64; 3]>,

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

/// Defines a local 2D co-ordinate system.
///
/// In 3D systems, the 'z' axis is inferred from the cross product of the 'x' and 'y' axes.
#[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
#[schemars(rename = "axes2D")]
pub struct Axes2d {
    /// The 'x' co-ordinate axis.
    #[serde(rename = "xAxis")]
    pub x: [f64; 2],

    /// The 'y' co-ordinate axis.
    #[serde(rename = "yAxis")]
    pub y: [f64; 2],
}

impl Default for Axes2d {
    fn default() -> Self {
        Self {
            x: [1.0, 0.0],
            y: [0.0, 1.0],
        }
    }
}

/// Defines a local 3D co-ordinate system.
///
/// The 'z' axis is inferred from the cross product of the 'x' and 'y' axes.
#[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
#[schemars(rename = "axes3D")]
pub struct Axes3d {
    /// The 'x' co-ordinate axis.
    #[serde(rename = "xAxis")]
    pub x: [f64; 3],

    /// The 'y' co-ordinate axis.
    #[serde(rename = "yAxis")]
    pub y: [f64; 3],
}

impl Default for Axes3d {
    fn default() -> Self {
        Self {
            x: [1.0, 0.0, 0.0],
            y: [0.0, 1.0, 0.0],
        }
    }
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

impl std::ops::Mul for Orientation {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self as i8) * (rhs as i8) {
            1 => Self::Same,
            -1 => Self::Reverse,
            _ => unreachable!(),
        }
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
    pub curve: IndexWithOrientation<Curve2d>,

    /// Marker for a closed trace.
    #[serde(default, skip_serializing_if = "bool_is_false")]
    pub closed: bool,

    /// Interval for the curve 't' parameter.
    pub t: Interval,
}

/// Pair of vertices on a face with an accompanying 3D curve..
#[derive(Clone, Debug, Deserialize, JsonSchema, Serialize, Validate)]
#[serde(rename_all = "camelCase")]
#[schemars(rename = "edge")]
pub struct Edge {
    /// The edge curve geometry in 3D (or homogeneous 4D) space.
    pub curve: IndexWithOrientation<Curve3d>,

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
