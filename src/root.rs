use crate::buffer;
use crate::texture;
use crate::validation;
use std::{fmt, marker};

use crate::path::Path;
use crate::{
    Accessor, Animation, Asset, Buffer, Camera, Extras, Image, Material, Mesh, Node, Scene, Skin,
    Texture, UnrecognizedExtensions, Wrap,
};
use validation::Validate;

/// Support for the `KHR_lights_punctual` extension.
pub mod khr_lights_punctual {
    /// Defines a set of lights that can be placed into a scene.
    #[derive(
        Clone,
        Debug,
        Default,
        gltf_derive::Deserialize,
        gltf_derive::Serialize,
        gltf_derive::Validate,
    )]
    pub struct Lights {
        /// An array of punctual light definitions.
        pub lights: Vec<crate::scene::khr_lights_punctual::Light>,

        /// Unrecognized extension data.
        pub unrecognized_extensions: crate::UnrecognizedExtensions,

        /// Optional application specific data.
        pub extras: Option<crate::Extras>,
    }
}

/// Support for the `KHR_materials_variants` extension.
pub mod khr_materials_variants {
    /// Defines an alternative material that may be applied to a mesh primitive.
    #[derive(
        Clone, Debug, gltf_derive::Deserialize, gltf_derive::Serialize, gltf_derive::Validate,
    )]
    pub struct Variant {
        /// The name of the material variant.
        pub name: String,

        /// Unrecognized extension data.
        pub unrecognized_extensions: crate::UnrecognizedExtensions,

        /// Optional application specific data.
        pub extras: Option<crate::Extras>,
    }

    /// Defines a set of alternative materials that may be applied to mesh primitives.
    #[derive(
        Clone,
        Debug,
        Default,
        gltf_derive::Deserialize,
        gltf_derive::Serialize,
        gltf_derive::Validate,
    )]
    pub struct Variants {
        /// The available material variants.
        pub variants: Vec<Variant>,

        /// Unrecognized extension data.
        pub unrecognized_extensions: crate::UnrecognizedExtensions,

        /// Optional application specific data.
        pub extras: Option<crate::Extras>,
    }
}

/// Support for the `KITTYCAD_boundary_representation` extension.
pub mod kittycad_boundary_representation {
    #[doc(inline)]
    pub use curve::{Curve2d, Curve3d};

    use crate::validation::Validate;
    use crate::{Extras, Index, UnrecognizedExtensions};
    #[doc(inline)]
    pub use surface::Surface;

    /// 2D and 3D curve definitions.
    pub mod curve {
        use super::{Axes2d, Axes3d};
        use crate::validation::{Error, Validate};
        use crate::{Extras, Root, UnrecognizedExtensions};

        /// Discriminant for `Curve` data.
        #[derive(
            Clone, Copy, Debug, serde_derive::Deserialize, Eq, PartialEq, serde_derive::Serialize,
        )]
        #[cfg_attr(
            feature = "schemars",
            derive(schemars::JsonSchema),
            schemars(rename = "curve.type", rename_all = "camelCase")
        )]
        #[serde(rename_all = "camelCase")]
        pub enum Type {
            /// Circle curve.
            Circle,
            /// Line curve.
            Line,
            /// NURBS curve.
            Nurbs,
        }

        impl Validate for Type {}

        /// Circular curve definition.
        ///
        /// λ(u) := O + R(cos(u)x + sin(u)y), where:
        /// * O = `self.origin`,
        /// * R = `self.radius`,
        /// * x = `self.axes.x`,
        /// * y = `self.axes.y`.
        #[derive(
            Clone, Debug, gltf_derive::Deserialize, gltf_derive::Serialize, gltf_derive::Validate,
        )]
        #[cfg_attr(
            feature = "schemars",
            derive(schemars::JsonSchema),
            schemars(rename = "curve.circle2D", rename_all = "camelCase")
        )]
        pub struct Circle2d {
            /// Local co-ordinate axes.
            pub axes: Option<Axes2d>,

            /// Position at the center of the circle.
            pub origin: Option<[f64; 2]>,

            /// Distance from the center position to all points on the circle.
            pub radius: f64,

            /// Unrecognized extension data.
            #[cfg_attr(
                feature = "schemars",
                schemars(rename = "extensions", schema_with = "crate::schema::extensions")
            )]
            pub unrecognized_extensions: UnrecognizedExtensions,

            /// Optional application specific data.
            #[cfg_attr(feature = "schemars", schemars(schema_with = "crate::schema::extras"))]
            pub extras: Option<Extras>,
        }

        /// Circular curve definition.
        ///
        /// λ(u) := O + R(cos(u)x + sin(u)y), where:
        /// * O = `self.origin`,
        /// * R = `self.radius`,
        /// * x = `self.axes.x`,
        /// * y = `self.axes.y`.
        #[derive(
            Clone, Debug, gltf_derive::Deserialize, gltf_derive::Serialize, gltf_derive::Validate,
        )]
        #[cfg_attr(
            feature = "schemars",
            derive(schemars::JsonSchema),
            schemars(rename = "curve.circle3D", rename_all = "camelCase")
        )]
        pub struct Circle3d {
            /// Local co-ordinate axes.
            pub axes: Option<Axes3d>,

            /// Position at the center of the circle.
            pub origin: Option<[f64; 3]>,

            /// Distance from the center position to all points on the circle.
            pub radius: f64,

            /// Unrecognized extension data.
            #[cfg_attr(
                feature = "schemars",
                schemars(rename = "extensions", schema_with = "crate::schema::extensions")
            )]
            pub unrecognized_extensions: UnrecognizedExtensions,

            /// Optional application specific data.
            #[cfg_attr(feature = "schemars", schemars(schema_with = "crate::schema::extras"))]
            pub extras: Option<Extras>,
        }

        /// Line curve definition.
        ///
        /// λ(u) := O + ux, where:
        /// * O = `self.origin`,
        /// * x = `self.direction`.
        #[derive(
            Clone, Debug, gltf_derive::Deserialize, gltf_derive::Serialize, gltf_derive::Validate,
        )]
        #[cfg_attr(
            feature = "schemars",
            derive(schemars::JsonSchema),
            schemars(rename = "curve.line2D", rename_all = "camelCase")
        )]
        pub struct Line2d {
            /// Origin position.
            pub origin: Option<[f64; 2]>,

            /// Unit vector pointing away from the origin position.
            pub direction: [f64; 2],

            /// Unrecognized extension data.
            #[cfg_attr(
                feature = "schemars",
                schemars(rename = "extensions", schema_with = "crate::schema::extensions")
            )]
            pub unrecognized_extensions: UnrecognizedExtensions,

            /// Optional application specific data.
            #[cfg_attr(feature = "schemars", schemars(schema_with = "crate::schema::extras"))]
            pub extras: Option<Extras>,
        }

        /// Line curve definition.
        ///
        /// λ(u) := O + ux, where:
        /// * O = `self.origin`,
        /// * x = `self.direction`.
        #[derive(
            Clone, Debug, gltf_derive::Deserialize, gltf_derive::Serialize, gltf_derive::Validate,
        )]
        #[cfg_attr(
            feature = "schemars",
            derive(schemars::JsonSchema),
            schemars(rename = "curve.line3D", rename_all = "camelCase")
        )]
        pub struct Line3d {
            /// Origin position.
            pub origin: Option<[f64; 3]>,

            /// Unit vector pointing away from the origin position.
            pub direction: [f64; 3],

            /// Unrecognized extension data.
            #[cfg_attr(
                feature = "schemars",
                schemars(rename = "extensions", schema_with = "crate::schema::extensions")
            )]
            pub unrecognized_extensions: UnrecognizedExtensions,

            /// Optional application specific data.
            #[cfg_attr(feature = "schemars", schemars(schema_with = "crate::schema::extras"))]
            pub extras: Option<Extras>,
        }

        /// Non-uniform rational basis spline curve definition.
        #[derive(
            Clone, Debug, gltf_derive::Deserialize, gltf_derive::Serialize, gltf_derive::Validate,
        )]
        #[cfg_attr(
            feature = "schemars",
            derive(schemars::JsonSchema),
            schemars(rename = "curve.nurbs2D", rename_all = "camelCase")
        )]
        #[gltf(validate = "nurbs2d_validate")]
        pub struct Nurbs2d {
            /// Array of control vertices.
            pub control_points: Vec<[f64; 2]>,

            /// Order of the basis splines.
            pub order: u32,

            /// Knot vector.
            pub knot_vector: Vec<f64>,

            /// Weights accompanying control points.
            ///
            /// May be omitted for non-rational B-splines.
            pub weights: Vec<f64>,

            /// Unrecognized extension data.
            #[cfg_attr(
                feature = "schemars",
                schemars(rename = "extensions", schema_with = "crate::schema::extensions")
            )]
            pub unrecognized_extensions: UnrecognizedExtensions,

            /// Optional application specific data.
            #[cfg_attr(feature = "schemars", schemars(schema_with = "crate::schema::extras"))]
            pub extras: Option<Extras>,
        }

        fn nurbs2d_validate<P, R>(this: &Nurbs2d, _root: &Root, path: P, report: &mut R)
        where
            P: Fn() -> crate::Path,
            R: FnMut(&dyn Fn() -> crate::Path, Error),
        {
            if !this.weights.is_empty() && this.weights.len() != this.control_points.len() {
                report(&|| path().field("weights"), Error::Invalid);
            }
        }

        /// Non-uniform rational basis spline curve definition.
        #[derive(
            Clone, Debug, gltf_derive::Deserialize, gltf_derive::Serialize, gltf_derive::Validate,
        )]
        #[cfg_attr(
            feature = "schemars",
            derive(schemars::JsonSchema),
            schemars(rename = "curve.nurbs3D", rename_all = "camelCase")
        )]
        #[gltf(validate = "nurbs3d_validate")]
        pub struct Nurbs3d {
            /// Array of control vertices.
            pub control_points: Vec<[f64; 3]>,

            /// Order of the basis splines.
            pub order: u32,

            /// Knot vector.
            pub knot_vector: Vec<f64>,

            /// Weights accompanying control points.
            ///
            /// May be omitted for non-rational B-splines.
            pub weights: Vec<f64>,

            /// Unrecognized extension data.
            #[cfg_attr(
                feature = "schemars",
                schemars(rename = "extensions", schema_with = "crate::schema::extensions")
            )]
            pub unrecognized_extensions: UnrecognizedExtensions,

            /// Optional application specific data.
            #[cfg_attr(feature = "schemars", schemars(schema_with = "crate::schema::extras"))]
            pub extras: Option<Extras>,
        }

        fn nurbs3d_validate<P, R>(this: &Nurbs3d, _root: &Root, path: P, report: &mut R)
        where
            P: Fn() -> crate::Path,
            R: FnMut(&dyn Fn() -> crate::Path, Error),
        {
            if !this.weights.is_empty() && this.weights.len() != this.control_points.len() {
                report(&|| path().field("weights"), Error::Invalid);
            }
        }

        /// Specific curve data.
        #[derive(Clone, Debug, serde_derive::Deserialize, serde_derive::Serialize)]
        #[serde(rename_all = "camelCase")]
        #[cfg_attr(
            feature = "schemars",
            derive(schemars::JsonSchema),
            schemars(rename = "curve.geometry2D", rename_all = "camelCase")
        )]
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
                    Self::Circle(circle) => {
                        circle.validate(root, || path().field("circle"), report)
                    }
                    Self::Line(line) => line.validate(root, || path().field("line"), report),
                    Self::Nurbs(nurbs) => nurbs.validate(root, || path().field("nurbs"), report),
                }
            }
        }

        /// Specific curve data.
        #[derive(Clone, Debug, serde_derive::Deserialize, serde_derive::Serialize)]
        #[serde(rename_all = "camelCase")]
        #[cfg_attr(
            feature = "schemars",
            derive(schemars::JsonSchema),
            schemars(rename = "curve.geometry3D", rename_all = "camelCase")
        )]
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
                    Self::Circle(circle) => {
                        circle.validate(root, || path().field("circle"), report)
                    }
                    Self::Line(line) => line.validate(root, || path().field("line"), report),
                    Self::Nurbs(nurbs) => nurbs.validate(root, || path().field("nurbs"), report),
                }
            }
        }

        /// Abstract curve data.
        #[derive(
            Clone, Debug, gltf_derive::Deserialize, gltf_derive::Serialize, gltf_derive::Validate,
        )]
        #[cfg_attr(
            feature = "schemars",
            derive(schemars::JsonSchema),
            schemars(rename = "curve3D", rename_all = "camelCase")
        )]
        pub struct Curve2d {
            /// Discriminant.
            #[serde(rename = "type")]
            pub type_: Type,

            /// Optional name for this surface.
            pub name: Option<String>,

            /// Specific curve data.
            #[serde(flatten)]
            pub geometry: Geometry2d,

            /// Unrecognized extension data.
            #[cfg_attr(
                feature = "schemars",
                schemars(rename = "extensions", schema_with = "crate::schema::extensions")
            )]
            pub unrecognized_extensions: UnrecognizedExtensions,

            /// Optional application specific data.
            #[cfg_attr(feature = "schemars", schemars(schema_with = "crate::schema::extras"))]
            pub extras: Option<Extras>,
        }

        /// Abstract curve data.
        #[derive(
            Clone, Debug, gltf_derive::Deserialize, gltf_derive::Serialize, gltf_derive::Validate,
        )]
        #[cfg_attr(
            feature = "schemars",
            derive(schemars::JsonSchema),
            schemars(rename = "curve3D", rename_all = "camelCase")
        )]
        pub struct Curve3d {
            /// Discriminant.
            #[serde(rename = "type")]
            pub type_: Type,

            /// Optional name for this surface.
            pub name: Option<String>,

            /// Specific curve data.
            #[serde(flatten)]
            pub geometry: Geometry3d,

            /// Unrecognized extension data.
            #[cfg_attr(
                feature = "schemars",
                schemars(rename = "extensions", schema_with = "crate::schema::extensions")
            )]
            pub unrecognized_extensions: UnrecognizedExtensions,

            /// Optional application specific data.
            #[cfg_attr(feature = "schemars", schemars(schema_with = "crate::schema::extras"))]
            pub extras: Option<Extras>,
        }
    }

    /// 3D surface definitions.
    pub mod surface {
        use super::Axes3d;
        use crate::validation::{Error, Validate};
        use crate::{Extras, Root, UnrecognizedExtensions};

        /// Discriminant for `Surface` data.
        #[derive(
            Clone, Copy, Debug, serde_derive::Deserialize, Eq, PartialEq, serde_derive::Serialize,
        )]
        #[serde(rename_all = "camelCase")]
        #[cfg_attr(
            feature = "schemars",
            derive(schemars::JsonSchema),
            schemars(rename = "surface.type", rename_all = "camelCase")
        )]
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

        impl Validate for Type {}

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
        #[derive(
            Clone, Debug, gltf_derive::Deserialize, gltf_derive::Serialize, gltf_derive::Validate,
        )]
        #[cfg_attr(
            feature = "schemars",
            derive(schemars::JsonSchema),
            schemars(rename = "surface.cylinder", rename_all = "camelCase")
        )]
        pub struct Cylinder {
            /// Local co-ordinate axes.
            pub axes: Option<Axes3d>,

            /// Position at the center of the circle.
            pub origin: Option<[f64; 3]>,

            /// Distance from the center position to all points on the circle.
            pub radius: f64,

            /// Unrecognized extension data.
            #[cfg_attr(
                feature = "schemars",
                schemars(rename = "extensions", schema_with = "crate::schema::extensions")
            )]
            pub unrecognized_extensions: UnrecognizedExtensions,

            /// Optional application specific data.
            #[cfg_attr(feature = "schemars", schemars(schema_with = "crate::schema::extras"))]
            pub extras: Option<Extras>,
        }

        /// NURBS surface definition.
        #[derive(
            Clone, Debug, gltf_derive::Deserialize, gltf_derive::Serialize, gltf_derive::Validate,
        )]
        #[cfg_attr(
            feature = "schemars",
            derive(schemars::JsonSchema),
            schemars(rename = "surface.nurbs", rename_all = "camelCase")
        )]
        #[gltf(validate = "nurbs_validate")]
        pub struct Nurbs {
            /// Matrix of control point vertices.
            pub control_points: Vec<[f64; 3]>,

            /// Dimensions of control point vertex matrix.
            pub num_control_points: [u32; 2],

            /// Number of knots in U and V.
            pub num_knots: [u32; 2],

            /// Knot vector.
            pub knot_vector: Vec<f64>,

            /// Weights.
            ///
            /// May be omitted for non-rational B-splines.
            pub weights: Vec<f64>,

            /// Order of basis splines.
            pub order: [u32; 2],

            /// Unrecognized extension data.
            #[cfg_attr(
                feature = "schemars",
                schemars(rename = "extensions", schema_with = "crate::schema::extensions")
            )]
            pub unrecognized_extensions: UnrecognizedExtensions,

            /// Optional application specific data.
            #[cfg_attr(feature = "schemars", schemars(schema_with = "crate::schema::extras"))]
            pub extras: Option<Extras>,
        }

        fn nurbs_validate<P, R>(this: &Nurbs, _root: &Root, path: P, report: &mut R)
        where
            P: Fn() -> crate::Path,
            R: FnMut(&dyn Fn() -> crate::Path, Error),
        {
            let expected_control_points = this.num_control_points[0] * this.num_control_points[1];
            if expected_control_points as usize != this.control_points.len() {
                report(&|| path().field("num_control_points"), Error::Invalid);
            }

            let expected_knots = this.num_knots[0] + this.num_knots[1];
            if expected_knots as usize != this.knot_vector.len() {
                report(&|| path().field("num_knots"), Error::Invalid);
            }

            if !this.weights.is_empty() && this.weights.len() != this.control_points.len() {
                report(&|| path().field("weights"), Error::Invalid);
            }
        }

        /// Plane surface definition.
        ///
        /// σ(u, v) := O + ux + vy, where:
        /// * O = `self.origin`,
        /// * x = `self.axes.x`,
        /// * y = `self.axes.y`.
        #[derive(
            Clone, Debug, gltf_derive::Deserialize, gltf_derive::Serialize, gltf_derive::Validate,
        )]
        #[cfg_attr(
            feature = "schemars",
            derive(schemars::JsonSchema),
            schemars(rename = "surface.plane", rename_all = "camelCase")
        )]
        pub struct Plane {
            /// Local co-ordinate axes.
            pub axes: Option<Axes3d>,

            /// An arbitrary point that lies on the plane.
            pub origin: Option<[f64; 3]>,

            /// Unrecognized extension data.
            #[cfg_attr(
                feature = "schemars",
                schemars(rename = "extensions", schema_with = "crate::schema::extensions")
            )]
            pub unrecognized_extensions: UnrecognizedExtensions,

            /// Optional application specific data.
            #[cfg_attr(feature = "schemars", schemars(schema_with = "crate::schema::extras"))]
            pub extras: Option<Extras>,
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
        #[derive(
            Clone, Debug, gltf_derive::Deserialize, gltf_derive::Serialize, gltf_derive::Validate,
        )]
        #[cfg_attr(
            feature = "schemars",
            derive(schemars::JsonSchema),
            schemars(rename = "surface.sphere", rename_all = "camelCase")
        )]
        pub struct Sphere {
            /// Local co-ordinate axes.
            pub axes: Option<Axes3d>,

            /// Position at the center of the circle.
            pub origin: Option<[f64; 3]>,

            /// Distance from the center position to all points on the circle.
            pub radius: f64,

            /// Unrecognized extension data.
            #[cfg_attr(
                feature = "schemars",
                schemars(rename = "extensions", schema_with = "crate::schema::extensions")
            )]
            pub unrecognized_extensions: UnrecognizedExtensions,

            /// Optional application specific data.
            #[cfg_attr(feature = "schemars", schemars(schema_with = "crate::schema::extras"))]
            pub extras: Option<Extras>,
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
        #[derive(
            Clone, Debug, gltf_derive::Deserialize, gltf_derive::Serialize, gltf_derive::Validate,
        )]
        #[cfg_attr(
            feature = "schemars",
            derive(schemars::JsonSchema),
            schemars(rename = "surface.torus", rename_all = "camelCase")
        )]
        pub struct Torus {
            /// Local co-ordinate axes.
            pub axes: Option<Axes3d>,

            /// The center of the torus.
            ///
            /// The axis of revolution passes through the origin of the torus.
            pub origin: Option<[f64; 3]>,

            /// Distance from the torus origin to the origin of the revolved circle.
            pub major_radius: f64,

            /// Distance of points away from the center of the revolved circle.
            pub minor_radius: f64,

            /// Unrecognized extension data.
            #[cfg_attr(
                feature = "schemars",
                schemars(rename = "extensions", schema_with = "crate::schema::extensions")
            )]
            pub unrecognized_extensions: UnrecognizedExtensions,

            /// Optional application specific data.
            #[cfg_attr(feature = "schemars", schemars(schema_with = "crate::schema::extras"))]
            pub extras: Option<Extras>,
        }

        /// Specific surface data.
        #[derive(Clone, Debug, serde_derive::Deserialize, serde_derive::Serialize)]
        #[serde(rename_all = "camelCase")]
        #[cfg_attr(
            feature = "schemars",
            derive(schemars::JsonSchema),
            schemars(rename = "surface.geometry", rename_all = "camelCase")
        )]
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
                    Self::Sphere(sphere) => {
                        sphere.validate(root, || path().field("sphere"), report)
                    }
                    Self::Torus(torus) => torus.validate(root, || path().field("torus"), report),
                }
            }
        }

        /// Abstract surface data.
        #[derive(
            Clone, Debug, gltf_derive::Deserialize, gltf_derive::Serialize, gltf_derive::Validate,
        )]
        #[cfg_attr(
            feature = "schemars",
            derive(schemars::JsonSchema),
            schemars(rename = "surface", rename_all = "camelCase")
        )]
        pub struct Surface {
            /// Discriminant.
            #[serde(rename = "type")]
            pub type_: Type,

            /// Optional name for this surface.
            pub name: Option<String>,

            /// Specific surface data.
            #[serde(flatten)]
            pub geometry: Geometry,

            /// Unrecognized extension data.
            #[cfg_attr(
                feature = "schemars",
                schemars(rename = "extensions", schema_with = "crate::schema::extensions")
            )]
            pub unrecognized_extensions: UnrecognizedExtensions,

            /// Optional application specific data.
            #[cfg_attr(feature = "schemars", schemars(schema_with = "crate::schema::extras"))]
            pub extras: Option<Extras>,
        }
    }

    /// Defines a local 2D co-ordinate system.
    ///
    /// In 3D systems, the 'z' axis is inferred from the cross product of the 'x' and 'y' axes.
    #[derive(
        Clone, Debug, serde_derive::Deserialize, serde_derive::Serialize, gltf_derive::Validate,
    )]
    #[cfg_attr(
        feature = "schemars",
        derive(schemars::JsonSchema),
        schemars(rename = "axes2D", rename_all = "camelCase")
    )]
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
    #[derive(
        Clone, Debug, serde_derive::Deserialize, serde_derive::Serialize, gltf_derive::Validate,
    )]
    #[cfg_attr(
        feature = "schemars",
        derive(schemars::JsonSchema),
        schemars(rename = "axes3D", rename_all = "camelCase")
    )]
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
    #[derive(Clone, Debug, serde_derive::Deserialize, serde_derive::Serialize)]
    #[serde(rename_all = "camelCase")]
    #[cfg_attr(
        feature = "schemars",
        derive(schemars::JsonSchema),
        schemars(rename = "vertex", rename_all = "camelCase")
    )]
    pub struct Vertex(
        /// `[x, y, z]` co-ordinate.
        pub [f64; 3],
    );

    impl Validate for Vertex {}

    /// Selected orientation of an orientable item.
    #[derive(
        Clone,
        Copy,
        Debug,
        Default,
        serde_repr::Deserialize_repr,
        Eq,
        PartialEq,
        serde_repr::Serialize_repr,
    )]
    #[repr(i8)]
    #[cfg_attr(
        feature = "schemars",
        derive(schemars::JsonSchema),
        schemars(rename = "orientation", rename_all = "camelCase")
    )]
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
    #[derive(Clone, Copy, Debug, serde_derive::Deserialize, serde_derive::Serialize)]
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

    #[cfg(feature = "schemars")]
    impl<T: Validate> schemars::JsonSchema for IndexWithOrientation<T> {
        fn schema_name() -> String {
            "indexWithOrientation".to_owned()
        }

        fn json_schema(generator: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
            #[derive(serde_derive::Deserialize, schemars::JsonSchema, serde_derive::Serialize)]
            struct NonGenericIndexWithOrientation(pub u32, #[serde(default)] pub Orientation);
            NonGenericIndexWithOrientation::json_schema(generator)
        }
    }

    impl<T: Validate> Validate for IndexWithOrientation<T>
    where
        crate::Root: AsRef<[T]>,
    {
        fn validate<P, R>(&self, root: &crate::Root, path: P, report: &mut R)
        where
            P: Fn() -> crate::Path,
            R: FnMut(&dyn Fn() -> crate::Path, crate::validation::Error),
        {
            self.0.validate(root, path, report);
        }
    }

    /// A pair of endpoint arguments for a single parameter.
    ///
    /// The interval is closed at both ends.
    #[derive(Clone, Debug, serde_derive::Deserialize, serde_derive::Serialize)]
    #[serde(rename_all = "camelCase")]
    #[cfg_attr(
        feature = "schemars",
        derive(schemars::JsonSchema),
        schemars(rename = "interval", rename_all = "camelCase")
    )]
    pub struct Interval(
        /// Minimum value.
        pub f64,
        /// Maximum value.
        pub f64,
    );

    impl Validate for Interval {}

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
    #[derive(
        Clone, Debug, gltf_derive::Deserialize, gltf_derive::Serialize, gltf_derive::Validate,
    )]
    #[cfg_attr(
        feature = "schemars",
        derive(schemars::JsonSchema),
        schemars(rename = "trace", rename_all = "camelCase")
    )]
    pub struct Trace {
        /// The trace curve geometry in 2D (or homogeneous 3D) space.
        pub curve: IndexWithOrientation<Curve2d>,

        /// Marker for a closed trace.
        pub closed: bool,

        /// Interval for the curve 't' parameter.
        pub t: Interval,

        /// Unrecognized extension data.
        #[cfg_attr(
            feature = "schemars",
            schemars(rename = "extensions", schema_with = "crate::schema::extensions")
        )]
        pub unrecognized_extensions: UnrecognizedExtensions,

        /// Optional application specific data.
        #[cfg_attr(feature = "schemars", schemars(schema_with = "crate::schema::extras"))]
        pub extras: Option<Extras>,
    }

    /// Pair of vertices on a face with an accompanying 3D curve.
    #[derive(
        Clone, Debug, gltf_derive::Deserialize, gltf_derive::Serialize, gltf_derive::Validate,
    )]
    #[cfg_attr(
        feature = "schemars",
        derive(schemars::JsonSchema),
        schemars(rename = "edge", rename_all = "camelCase")
    )]
    pub struct Edge {
        /// The edge curve geometry in 3D (or homogeneous 4D) space.
        pub curve: IndexWithOrientation<Curve3d>,

        /// Edge start vertex.
        pub start: Option<Index<Vertex>>,

        /// Edge end vertex.
        pub end: Option<Index<Vertex>>,

        /// Marker for a closed edge.
        pub closed: bool,

        /// Interval for the curve's 't' parameter.
        pub t: Interval,

        /// Unrecognized extension data.
        #[cfg_attr(
            feature = "schemars",
            schemars(rename = "extensions", schema_with = "crate::schema::extensions")
        )]
        pub unrecognized_extensions: UnrecognizedExtensions,

        /// Optional application specific data.
        #[cfg_attr(feature = "schemars", schemars(schema_with = "crate::schema::extras"))]
        pub extras: Option<Extras>,
    }

    /// Edge loop.
    #[derive(
        Clone, Debug, gltf_derive::Deserialize, gltf_derive::Serialize, gltf_derive::Validate,
    )]
    #[cfg_attr(
        feature = "schemars",
        derive(schemars::JsonSchema),
        schemars(rename = "loop", rename_all = "camelCase")
    )]
    pub struct Loop {
        /// Oriented edges forming the loop.
        pub edges: Vec<IndexWithOrientation<Edge>>,

        /// Optional 1:1 pairing of traces to edges.
        pub traces: Vec<Trace>,

        /// Unrecognized extension data.
        #[cfg_attr(
            feature = "schemars",
            schemars(rename = "extensions", schema_with = "crate::schema::extensions")
        )]
        pub unrecognized_extensions: UnrecognizedExtensions,

        /// Optional application specific data.
        #[cfg_attr(feature = "schemars", schemars(schema_with = "crate::schema::extras"))]
        pub extras: Option<Extras>,
    }

    /// Set of loops defined on an abstract surface.
    #[derive(
        Clone, Debug, gltf_derive::Deserialize, gltf_derive::Serialize, gltf_derive::Validate,
    )]
    #[cfg_attr(
        feature = "schemars",
        derive(schemars::JsonSchema),
        schemars(rename = "face", rename_all = "camelCase")
    )]
    pub struct Face {
        /// Surface the face edges and vertices reside on.
        pub surface: IndexWithOrientation<Surface>,

        /// Face bounds.
        pub loops: Vec<IndexWithOrientation<Loop>>,

        /// Unrecognized extension data.
        #[cfg_attr(
            feature = "schemars",
            schemars(rename = "extensions", schema_with = "crate::schema::extensions")
        )]
        pub unrecognized_extensions: UnrecognizedExtensions,

        /// Optional application specific data.
        #[cfg_attr(feature = "schemars", schemars(schema_with = "crate::schema::extras"))]
        pub extras: Option<Extras>,
    }

    /// Boundary representation volume.
    #[derive(
        Clone, Debug, gltf_derive::Deserialize, gltf_derive::Serialize, gltf_derive::Validate,
    )]
    #[cfg_attr(
        feature = "schemars",
        derive(schemars::JsonSchema),
        schemars(rename = "shell", rename_all = "camelCase")
    )]
    pub struct Shell {
        /// Set of connected faces forming a closed 'watertight' volume.
        pub faces: Vec<IndexWithOrientation<Face>>,

        /// Optional name for this shell.
        pub name: Option<String>,

        /// Unrecognized extension data.
        #[cfg_attr(
            feature = "schemars",
            schemars(rename = "extensions", schema_with = "crate::schema::extensions")
        )]
        pub unrecognized_extensions: UnrecognizedExtensions,

        /// Optional application specific data.
        #[cfg_attr(feature = "schemars", schemars(schema_with = "crate::schema::extras"))]
        pub extras: Option<Extras>,
    }

    /// Solid boundary representation structure.
    #[derive(
        Clone, Debug, gltf_derive::Deserialize, gltf_derive::Serialize, gltf_derive::Validate,
    )]
    #[cfg_attr(
        feature = "schemars",
        derive(schemars::JsonSchema),
        schemars(rename = "solid", rename_all = "camelCase")
    )]
    pub struct Solid {
        /// The boundaries of the solid volume.
        pub shells: Vec<IndexWithOrientation<Shell>>,

        /// Optional name for this solid.
        pub name: Option<String>,

        /// Optional mesh approximation of this solid.
        pub mesh: Option<Index<crate::Mesh>>,

        /// Unrecognized extension data.
        #[cfg_attr(
            feature = "schemars",
            schemars(rename = "extensions", schema_with = "crate::schema::extensions")
        )]
        pub unrecognized_extensions: UnrecognizedExtensions,

        /// Optional application specific data.
        #[cfg_attr(feature = "schemars", schemars(schema_with = "crate::schema::extras"))]
        pub extras: Option<Extras>,
    }

    /// Root container for the `KITTYCAD_boundary_representation` extension.
    #[derive(
        Clone,
        Debug,
        Default,
        gltf_derive::Deserialize,
        gltf_derive::Serialize,
        gltf_derive::Validate,
    )]
    #[cfg_attr(
        feature = "schemars",
        derive(schemars::JsonSchema),
        schemars(rename = "KITTYCAD_boundary_representation", rename_all = "camelCase")
    )]
    pub struct BRep {
        /// Solid boundary representation instances.
        pub solids: Vec<Solid>,

        /// Shell definitions.
        pub shells: Vec<Shell>,

        /// Face definitions.
        pub faces: Vec<Face>,

        /// Loop definitions.
        pub loops: Vec<Loop>,

        /// Edge definitions.
        pub edges: Vec<Edge>,

        /// Vertices in 3D space joining edges.
        pub vertices: Vec<Vertex>,

        /// Surface definitions.
        pub surfaces: Vec<Surface>,

        /// 2D curve definitions.
        pub curves_2d: Vec<Curve2d>,

        /// 3D curve definitions.
        pub curves_3d: Vec<Curve3d>,

        /// Unrecognized extension data.
        #[cfg_attr(
            feature = "schemars",
            schemars(rename = "extensions", schema_with = "crate::schema::extensions")
        )]
        pub unrecognized_extensions: UnrecognizedExtensions,

        /// Optional application specific data.
        #[cfg_attr(feature = "schemars", schemars(schema_with = "crate::schema::extras"))]
        pub extras: Option<Extras>,
    }
}

/// Represents an offset into a vector of type `T` owned by the root glTF object.
///
/// This type may be used with the following functions:
///
/// * [`Root::get()`] to retrieve objects from [`Root`].
/// * [`Root::push()`] to add new objects to [`Root`].
pub struct Index<T>(u32, marker::PhantomData<fn() -> T>);

/// The root object of a glTF 2.0 asset.
#[derive(
    Clone, Debug, Default, gltf_derive::Deserialize, gltf_derive::Serialize, gltf_derive::Validate,
)]
#[gltf(validate = "validate_root")]
pub struct Root {
    /// An array of accessors.
    pub accessors: Vec<Accessor>,

    /// An array of keyframe animations.
    pub animations: Vec<Animation>,

    /// Metadata about the glTF asset.
    pub asset: Asset,

    /// An array of buffers.
    pub buffers: Vec<Buffer>,

    /// An array of buffer views.
    pub buffer_views: Vec<buffer::View>,

    /// The default scene.
    pub scene: Option<Index<Scene>>,

    /// Names of glTF extensions used somewhere in this asset.
    pub extensions_used: Vec<String>,

    /// Names of glTF extensions required to properly load this asset.
    pub extensions_required: Vec<String>,

    /// An array of cameras.
    pub cameras: Vec<Camera>,

    /// An array of images.
    pub images: Vec<Image>,

    /// An array of materials.
    pub materials: Vec<Material>,

    /// An array of meshes.
    pub meshes: Vec<Mesh>,

    /// An array of nodes.
    pub nodes: Vec<Node>,

    /// An array of samplers.
    pub samplers: Vec<texture::Sampler>,

    /// An array of scenes.
    pub scenes: Vec<Scene>,

    /// An array of skins.
    pub skins: Vec<Skin>,

    /// An array of textures.
    pub textures: Vec<Texture>,

    /// Unrecognized extension data.
    pub unrecognized_extensions: UnrecognizedExtensions,

    /// Optional application specific data.
    pub extras: Option<Extras>,

    /// Support for the `KITTYCAD_boundary_representation` extension.
    #[gltf(extension = "KITTYCAD_boundary_representation")]
    pub brep: Option<kittycad_boundary_representation::BRep>,

    /// Support for the `KHR_lights_punctual` extension.
    #[gltf(extension = "KHR_lights_punctual")]
    pub lights: Option<khr_lights_punctual::Lights>,

    /// Support for the `KHR_materials_variants` extension.
    #[gltf(extension = "KHR_materials_variants")]
    pub variants: Option<khr_materials_variants::Variants>,
}

fn validate_root<P, R>(root: &Root, _also_root: &Root, path: P, report: &mut R)
where
    P: Fn() -> Path,
    R: FnMut(&dyn Fn() -> Path, crate::validation::Error),
{
    for (i, ext) in root.extensions_required.iter().enumerate() {
        if !crate::SUPPORTED_EXTENSIONS.contains(&ext.as_str()) {
            report(
                &|| {
                    path()
                        .field("extensionsRequired")
                        .index(i)
                        .value_str(ext.as_str())
                },
                crate::validation::Error::Unsupported,
            );
        }
    }
}

impl<T> std::ops::Index<Index<T>> for Root
where
    Root: AsRef<[T]>,
{
    type Output = T;

    fn index(&self, index: Index<T>) -> &Self::Output {
        let slice: &[T] = self.as_ref();
        &slice[index.value()]
    }
}

impl<T> std::ops::IndexMut<Index<T>> for Root
where
    Root: AsRef<[T]> + AsMut<Vec<T>>,
{
    fn index_mut(&mut self, index: Index<T>) -> &mut Self::Output {
        let slice: &mut Vec<T> = self.as_mut();
        &mut slice[index.value()]
    }
}

impl Root {
    /// Returns a single item from the root object.
    pub fn get<T>(&self, index: Index<T>) -> Option<&T>
    where
        Self: AsRef<[T]>,
    {
        self.as_ref().get(index.value())
    }

    /// Insert the given value into this (as via [`Vec::push()`]), then return the [`Index`] to it.
    ///
    /// This allows you to easily obtain [`Index`] values with the correct index and type when
    /// creating a glTF asset.
    ///
    /// If you have a mutable borrow conflict when using this method, consider using the more
    /// explicit [`Index::push()`] method, passing it only the necessary vector.
    ///
    /// # Panics
    ///
    /// Panics if there are already [`u32::MAX`] or more elements of this type,
    /// in which case an `Index` cannot be created.
    #[track_caller]
    pub fn push<T>(&mut self, value: T) -> Index<T>
    where
        Self: AsMut<Vec<T>>,
    {
        Index::push(self.as_mut(), value)
    }
}

impl<T> Index<T> {
    /// Creates a new `Index` representing an offset into an array containing `T`.
    pub fn new(value: u32) -> Self {
        Index(value, std::marker::PhantomData)
    }

    /// Given a vector of glTF objects, call [`Vec::push()`] to insert it into the vector,
    /// then return an [`Index`] for it.
    ///
    /// This allows you to easily obtain [`Index`] values with the correct index and type when
    /// creating a glTF asset. Note that for [`Root`], you can call [`Root::push()`] without
    /// needing to retrieve the correct vector first.
    ///
    /// # Panics
    ///
    /// Panics if the vector has [`u32::MAX`] or more elements, in which case an `Index` cannot be
    /// created.
    pub fn push(vec: &mut Vec<T>, value: T) -> Index<T> {
        let len = vec.len();
        let Ok(index): Result<u32, _> = len.try_into() else {
            panic!(
                "glTF vector of {ty} has {len} elements, which exceeds the Index limit",
                ty = std::any::type_name::<T>(),
            );
        };
        vec.push(value);
        Index::new(index)
    }

    /// Returns the internal offset value.
    pub fn value(&self) -> usize {
        self.0 as usize
    }
}

impl<T> serde::Serialize for Index<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ::serde::Serializer,
    {
        serializer.serialize_u64(self.value() as u64)
    }
}

impl<'de, T> serde::Deserialize<'de> for Index<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct Visitor<T>(marker::PhantomData<T>);
        impl<'de, T> serde::de::Visitor<'de> for Visitor<T> {
            type Value = Index<T>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("index into child of root")
            }

            fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Index::new(value as u32))
            }
        }
        deserializer.deserialize_u64(Visitor::<T>(marker::PhantomData))
    }
}

impl<T> Clone for Index<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Index<T> {}

impl<T> Ord for Index<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}
impl<T> PartialOrd for Index<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Eq for Index<T> {}
impl<T> PartialEq for Index<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> std::hash::Hash for Index<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T> fmt::Debug for Index<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<T> fmt::Display for Index<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<T: Validate> Validate for Index<T>
where
    Root: AsRef<[T]>,
{
    fn validate<P, R>(&self, root: &Root, path: P, report: &mut R)
    where
        P: Fn() -> Path,
        R: FnMut(&dyn Fn() -> Path, validation::Error),
    {
        if root.get(*self).is_none() {
            report(&path, validation::Error::IndexOutOfBounds);
        }
    }
}

#[cfg(feature = "schemars")]
impl<T> schemars::JsonSchema for Index<T> {
    fn schema_name() -> String {
        "index".to_owned()
    }

    fn json_schema(generator: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        u32::json_schema(generator)
    }
}

impl<'a, T> Wrap<'a> for Index<T>
where
    T: 'a + Wrap<'a>,
    Root: AsRef<[T]>,
{
    type Wrapped = <T as Wrap<'a>>::Wrapped;

    fn wrap(&'a self, root: &'a Root) -> Self::Wrapped {
        let items = root.as_ref();
        items[self.value()].wrap_indexed(root, self.value())
    }
}

impl<'a, T> Wrap<'a> for std::boxed::Box<T>
where
    T: 'a + Wrap<'a>,
{
    type Wrapped = <T as Wrap<'a>>::Wrapped;

    fn wrap(&'a self, root: &'a Root) -> Self::Wrapped {
        use std::ops::Deref;
        self.deref().wrap(root)
    }
}

impl<'a, K: 'a, V: 'a> Wrap<'a> for serde_json::Map<K, V> {
    type Wrapped = &'a Self;

    fn wrap(&'a self, _root: &'a Root) -> Self::Wrapped {
        self
    }
}

impl<'a> Wrap<'a> for serde_json::Value {
    type Wrapped = &'a Self;

    fn wrap(&'a self, _root: &'a Root) -> Self::Wrapped {
        self
    }
}

macro_rules! impl_as_ref {
    ($field:ident, $ty:ty) => {
        impl AsRef<[$ty]> for Root {
            fn as_ref(&self) -> &[$ty] {
                &self.$field
            }
        }

        impl AsMut<Vec<$ty>> for Root {
            fn as_mut(&mut self) -> &mut Vec<$ty> {
                &mut self.$field
            }
        }
    };

    ($extension:ident, $field:ident, $ty:ty) => {
        impl AsRef<[$ty]> for Root {
            fn as_ref(&self) -> &[$ty] {
                self.$extension
                    .as_ref()
                    .map(|extension| extension.$field.as_slice())
                    .unwrap_or(&[])
            }
        }

        impl AsMut<Vec<$ty>> for Root {
            fn as_mut(&mut self) -> &mut Vec<$ty> {
                &mut self.$extension.get_or_insert_with(Default::default).$field
            }
        }
    };
}

impl_as_ref!(accessors, Accessor);
impl_as_ref!(animations, Animation);
impl_as_ref!(buffers, Buffer);
impl_as_ref!(buffer_views, buffer::View);
impl_as_ref!(cameras, Camera);
impl_as_ref!(images, Image);
impl_as_ref!(materials, Material);
impl_as_ref!(meshes, Mesh);
impl_as_ref!(nodes, Node);
impl_as_ref!(samplers, texture::Sampler);
impl_as_ref!(scenes, Scene);
impl_as_ref!(skins, Skin);
impl_as_ref!(textures, Texture);
impl_as_ref!(lights, lights, crate::scene::khr_lights_punctual::Light);
impl_as_ref!(variants, variants, khr_materials_variants::Variant);
impl_as_ref!(brep, solids, kittycad_boundary_representation::Solid);
impl_as_ref!(brep, shells, kittycad_boundary_representation::Shell);
impl_as_ref!(brep, faces, kittycad_boundary_representation::Face);
impl_as_ref!(brep, loops, kittycad_boundary_representation::Loop);
impl_as_ref!(brep, edges, kittycad_boundary_representation::Edge);
impl_as_ref!(brep, vertices, kittycad_boundary_representation::Vertex);
impl_as_ref!(brep, surfaces, kittycad_boundary_representation::Surface);
impl_as_ref!(brep, curves_2d, kittycad_boundary_representation::Curve2d);
impl_as_ref!(brep, curves_3d, kittycad_boundary_representation::Curve3d);

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn index_is_partialeq() {
        assert_eq!(Index::<Node>::new(1), Index::new(1));
        assert_ne!(Index::<Node>::new(1), Index::new(2));
    }

    #[test]
    fn index_is_hash() {
        let set = HashSet::from([Index::<Node>::new(1), Index::new(1234)]);
        assert!(set.contains(&Index::new(1234)));
        assert!(!set.contains(&Index::new(999)));
        assert_eq!(set.len(), 2);
    }

    #[test]
    fn index_is_ord() {
        assert!(Index::<Node>::new(1) < Index::new(1234));
    }

    fn _index_is_send_sync()
    where
        Index<Material>: Send + Sync,
    {
    }

    #[test]
    fn index_push() {
        let some_object = "hello";

        let mut vec = Vec::new();
        assert_eq!(Index::push(&mut vec, some_object), Index::new(0));
        assert_eq!(Index::push(&mut vec, some_object), Index::new(1));
    }

    #[test]
    fn root_push() {
        let some_object = Buffer {
            length: validation::USize64(1),
            name: None,
            uri: None,
            extras: Default::default(),
            unrecognized_extensions: Default::default(),
        };

        let mut root = Root::default();
        assert_eq!(root.push(some_object.clone()), Index::new(0));
        assert_eq!(root.push(some_object), Index::new(1));
    }

    #[test]
    fn root_extensions() {
        use crate::validation::Error;
        use crate::Path;

        let mut root = super::Root {
            extensions_required: vec!["KHR_lights_punctual".to_owned()],
            ..Default::default()
        };

        let mut errors = Vec::new();
        root.validate(&root, Path::new, &mut |path, error| {
            errors.push((path(), error));
        });
        assert!(errors.is_empty());

        root.extensions_required = vec!["KHR_mesh_quantization".to_owned()];
        errors.clear();
        root.validate(&root, Path::new, &mut |path, error| {
            errors.push((path(), error));
        });
        assert_eq!(1, errors.len());
        let (path, error) = errors.get(0).unwrap();
        assert_eq!(
            path.as_str(),
            "extensionsRequired[0] = \"KHR_mesh_quantization\""
        );
        assert_eq!(*error, Error::Unsupported);
    }

    #[cfg(feature = "schemars")]
    #[test]
    fn test_generate_schema() {
        let schema = schemars::schema_for!(super::kittycad_boundary_representation::BRep);
        let json = serde_json::to_string_pretty(&schema).unwrap();
        println!("{json}");
    }
}
