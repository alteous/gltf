use crate::{Document, Mesh};
use json::extensions::kittycad_boundary_representation as kcad;

#[doc(inline)]
pub use kcad::{Axes2d, Axes3d, Interval, Orientation};

#[doc(inline)]
pub use curve::{Curve2d, Curve3d};

#[doc(inline)]
pub use surface::Surface;

fn slice_truncated<T>(slice: &[T], range: std::ops::Range<usize>) -> &[T] {
    &slice[range.start.min(slice.len())..range.end.min(slice.len())]
}

/// Iterates over `n` equally spaced steps between the interval endpoints,
/// beginning and ending with the endpoints themselves.
///
/// ```
/// # use IntervalExt;
/// let interval = Interval(0.0, 4.0);
/// let mut iter = interval.steps(4);
/// assert_eq!(iter.next(), Some(0.0));
/// assert_eq!(iter.next(), Some(1.0));
/// assert_eq!(iter.next(), Some(2.0));
/// assert_eq!(iter.next(), Some(3.0));
/// assert_eq!(iter.next(), Some(4.0));
/// assert_eq!(iter.next(), None);
/// ```
#[derive(Clone, Debug)]
pub struct Steps {
    tmax: f64,
    t: f64,
    dt: f64,
    n: usize,
    i: usize,
}

impl Iterator for Steps {
    type Item = f64;
    fn next(&mut self) -> Option<Self::Item> {
        if self.i == self.n + 1 {
            None
        } else if self.i < self.n {
            let ti = self.t;
            self.t += self.dt;
            self.i += 1;
            Some(ti)
        } else {
            self.i += 1;
            Some(self.tmax)
        }
    }
}

/// Extra functions for the `Interval` type.
pub trait IntervalExt {
    /// Iterates over `n` equally spaced steps between the interval endpoints,
    /// beginning and ending with the endpoints themselves.
    fn steps(&self, n: usize) -> Steps;
}

impl IntervalExt for Interval {
    fn steps(&self, n: usize) -> Steps {
        Steps {
            tmax: self.1,
            t: self.0,
            dt: (self.1 - self.0) / (n as f64),
            i: 0,
            n,
        }
    }
}

/// Curves.
pub mod curve {
    use euler::{DVec2, DVec3, DVec4};
    use json::extensions::kittycad_boundary_representation as kcad;

    /// Circle in 2D space.
    #[derive(Clone, Debug)]
    pub struct Circle2d<'a> {
        /// The corresponding JSON struct.
        pub(crate) json: &'a kcad::curve::Circle2d,
    }

    impl<'a> Circle2d<'a> {
        /// Position at the center of the circle.
        pub fn origin(&self) -> [f64; 2] {
            self.json.origin.unwrap_or_default()
        }

        /// Distance from the center position to all points on the circle.
        pub fn radius(&self) -> f64 {
            self.json.radius
        }

        /// Unit vector in the direction from the origin to the point on
        /// the circle at λ(0).
        ///
        /// Due to floating point precision, this vector may not lie exactly
        /// in the plane. If this is the case then the X vector is treated
        /// as the projection of this vector onto the plane.
        pub fn xaxis(&self) -> [f64; 2] {
            self.json.axes.clone().unwrap_or_default().x
        }

        /// Unit vector in the direction from the origin to the point on
        /// the circle at λ(π / 2).
        pub fn yaxis(&self) -> [f64; 2] {
            self.json.axes.clone().unwrap_or_default().y
        }

        /// Evaluate the curve at parameter value `t`.
        pub fn evaluate(&self, t: f64) -> [f64; 2] {
            let radius = self.json.radius;
            let origin = DVec2::from(self.origin());
            let xaxis = DVec2::from(self.xaxis());
            let yaxis = DVec2::from(self.yaxis());
            let (sine, cosine) = t.sin_cos();
            (origin + (xaxis * cosine + yaxis * sine) * radius).into()
        }
    }

    /// Circle in 3D space.
    #[derive(Clone, Debug)]
    pub struct Circle3d<'a> {
        /// The corresponding JSON struct.
        pub(crate) json: &'a kcad::curve::Circle3d,
    }

    impl<'a> Circle3d<'a> {
        /// Position at the center of the circle.
        pub fn origin(&self) -> [f64; 3] {
            self.json.origin.unwrap_or_default()
        }

        /// Distance from the center position to all points on the circle.
        pub fn radius(&self) -> f64 {
            self.json.radius
        }

        /// Unit vector in the direction from the origin to the point on
        /// the circle at λ(0).
        ///
        /// Due to floating point precision, this vector may not lie exactly
        /// in the plane. If this is the case then the X vector is treated
        /// as the projection of this vector onto the plane.
        pub fn xaxis(&self) -> [f64; 3] {
            self.json.axes.clone().unwrap_or_default().x
        }

        /// Unit vector in the direction from the origin to the point on
        /// the circle at λ(π / 2).
        pub fn yaxis(&self) -> [f64; 3] {
            self.json.axes.clone().unwrap_or_default().y
        }

        /// Normal vector to the plane containing the circle.
        ///
        /// This serves as the Z basis in the parametric co-ordinate space.
        pub fn zaxis(&self) -> [f64; 3] {
            DVec3::from(self.xaxis())
                .cross(DVec3::from(self.yaxis()))
                .into()
        }

        /// Evaluate the curve at parameter value `t`.
        pub fn evaluate(&self, t: f64) -> [f64; 3] {
            let radius = self.radius();
            let origin = DVec3::from(self.origin());
            let xaxis = DVec3::from(self.xaxis());
            let yaxis = DVec3::from(self.yaxis());
            let (sine, cosine) = t.sin_cos();
            (origin + (xaxis * cosine + yaxis * sine) * radius).into()
        }
    }

    /// Defines a line curve.
    #[derive(Clone, Debug)]
    pub struct Line2d<'a> {
        /// The corresponding JSON struct.
        pub(crate) json: &'a kcad::curve::Line2d,
    }

    impl<'a> Line2d<'a> {
        /// Returns the line origin point.
        pub fn origin(&self) -> [f64; 2] {
            self.json.origin.unwrap_or_default()
        }

        /// Evaluate the curve at parameter value `t`.
        pub fn evaluate(&self, t: f64) -> [f64; 2] {
            let origin = DVec2::from(self.origin());
            let direction = DVec2::from(self.direction());
            (origin + t * direction).into()
        }

        /// Returns the line direction.
        ///
        /// If `end` was set, this will be computed.
        pub fn direction(&self) -> [f64; 2] {
            self.json.direction
        }
    }

    /// Defines a line curve.
    #[derive(Clone, Debug)]
    pub struct Line3d<'a> {
        /// The corresponding JSON struct.
        pub(crate) json: &'a kcad::curve::Line3d,
    }

    impl<'a> Line3d<'a> {
        /// Returns the line origin point.
        pub fn origin(&self) -> [f64; 3] {
            self.json.origin.unwrap_or_default()
        }

        /// Evaluate the curve at parameter value `t`.
        pub fn evaluate(&self, t: f64) -> [f64; 3] {
            let origin = DVec3::from(self.origin());
            let direction = DVec3::from(self.direction());
            (origin + t * direction).into()
        }

        /// Returns the line direction.
        ///
        /// If `end` was set, this will be computed.
        pub fn direction(&self) -> [f64; 3] {
            self.json.direction
        }
    }

    /// Defines a non-uniform rational B-spline (NURBS) curve.
    #[derive(Clone, Debug)]
    pub struct Nurbs2d<'a> {
        /// The corresponding JSON struct.
        pub(crate) json: &'a kcad::curve::Nurbs2d,
    }

    impl<'a> Nurbs2d<'a> {
        /// Evaluate the curve at parameter value `t`.
        pub fn evaluate(&self, t: f64) -> [f64; 2] {
            let control_points_3d = self
                .json
                .control_points
                .iter()
                .map(|v| [v[0], v[1], 0.0])
                .collect();
            let nurbs_3d = Nurbs3d {
                json: &kcad::curve::Nurbs3d {
                    control_points: control_points_3d,
                    weights: self.json.weights.clone(),
                    knot_vector: self.json.knot_vector.clone(),
                    order: self.json.order,
                },
            };
            let [x, y, _] = nurbs_3d.evaluate(t);
            [x, y]
        }

        /// Returns the curve start point, i.e., the first control point.
        pub fn start(&self) -> [f64; 2] {
            *self.json.control_points.first().unwrap()
        }

        /// Returns the curve end point, i.e., the last control point.
        pub fn end(&self) -> [f64; 2] {
            *self.json.control_points.last().unwrap()
        }

        /// Returns the NURBS control points.
        pub fn control_points(&self) -> &[[f64; 2]] {
            &self.json.control_points
        }

        /// Returns the NURBS knot vector.
        pub fn knot_vector(&self) -> &[f64] {
            &self.json.knot_vector
        }

        /// Returns the order of the basis splines.
        ///
        /// # Notes
        ///
        /// The degree of the basis splines is one less than the order.
        pub fn order(&self) -> u32 {
            self.json.order
        }
    }

    /// Defines a non-uniform rational B-spline (NURBS) curve.
    #[derive(Clone, Debug)]
    pub struct Nurbs3d<'a> {
        /// The corresponding JSON struct.
        pub(crate) json: &'a kcad::curve::Nurbs3d,
    }

    impl<'a> Nurbs3d<'a> {
        /// Evaluate the curve at parameter value `t`.
        pub fn evaluate(&self, t: f64) -> [f64; 3] {
            // Min/max knot value
            let (umin, umax) = {
                let u = self.knot_vector();
                (*u.first().unwrap(), *u.last().unwrap())
            };

            if t == umin {
                self.start()
            } else if t == umax {
                self.end()
            } else {
                // Degree
                let d = (self.json.order - 1) as usize;

                // Padded knot vector
                let mut u = self.knot_vector().to_vec();
                for _ in 0..d {
                    u.insert(0, umin);
                    u.push(umax);
                }

                // Index of knot interval
                let k = u
                    .windows(2)
                    .position(|ui| t >= ui[0] && t < ui[1])
                    .expect("t does not lie between any knot");

                // Multiplicity
                let m = u
                    .windows(2)
                    .rev()
                    .position(|ui| t >= ui[0] && t < ui[1])
                    .map(|ui| k - ui)
                    .expect("t does not lie between any knot");

                // Weight iterator.
                let weights = self.weights().iter().copied().chain(std::iter::repeat(1.0));

                // New control points
                let mut p = self
                    .control_points()
                    .iter()
                    .copied()
                    .zip(weights)
                    .map(|([x, y, z], w)| DVec4::new(x * w, y * w, z * w, w))
                    .collect::<Vec<_>>();

                let h = d - m;
                for r in 1..=h {
                    for i in (r..=h).rev() {
                        let upper = t - u[k + i - d];
                        let lower = u[k + i + 1 - r] - u[k + i - d];
                        let a = if lower == 0.0 { 0.0 } else { upper / lower };
                        p[i] = (1.0 - a) * p[i - 1] + a * p[i];
                    }
                }

                (p[d].xyz() / p[d].w).into()
            }
        }

        /// Returns the curve start point, i.e., the first control point.
        pub fn start(&self) -> [f64; 3] {
            *self.json.control_points.first().unwrap()
        }

        /// Returns the curve end point, i.e., the last control point.
        pub fn end(&self) -> [f64; 3] {
            *self.json.control_points.last().unwrap()
        }

        /// Returns the NURBS control points.
        pub fn control_points(&self) -> &[[f64; 3]] {
            &self.json.control_points
        }

        /// Returns the NURBS control point weights.
        pub fn weights(&self) -> &[f64] {
            &self.json.weights
        }

        /// Returns the NURBS knot vector.
        pub fn knot_vector(&self) -> &[f64] {
            &self.json.knot_vector
        }

        /// Returns the order of the basis splines.
        ///
        /// # Notes
        ///
        /// The degree of the basis splines is one less than the order.
        pub fn order(&self) -> u32 {
            self.json.order
        }
    }

    /// Curve kind.
    #[derive(Clone, Debug)]
    pub enum Geometry2d<'a> {
        /// Circular curve.
        Circle(Circle2d<'a>),
        /// Linear curve.
        Line(Line2d<'a>),
        /// Non-uniform rational B-spline (NURBS) curve.
        Nurbs(Nurbs2d<'a>),
    }

    /// Abstract curve.
    #[derive(Clone, Debug)]
    pub struct Curve2d<'a> {
        /// The corresponding JSON index.
        index: usize,

        /// The corresponding JSON struct.
        json: &'a kcad::Curve2d,
    }

    impl<'a> Curve2d<'a> {
        /// Constructs a `Curve`.
        pub fn new(index: usize, json: &'a kcad::Curve2d) -> Self {
            Self { index, json }
        }

        /// Returns the internal JSON index.
        pub fn index(&self) -> usize {
            self.index
        }

        /// Optional user-defined name for this object.
        #[cfg(feature = "names")]
        pub fn name(&self) -> Option<&'a str> {
            self.json.name.as_deref()
        }

        /// Evaluate the curve at parameter value `t`.
        pub fn evaluate(&self, t: f64) -> [f64; 2] {
            match self.geometry() {
                Geometry2d::Circle(circle) => circle.evaluate(t),
                Geometry2d::Line(line) => line.evaluate(t),
                Geometry2d::Nurbs(nurbs) => nurbs.evaluate(t),
            }
        }

        /// Returns the specific underlying curve geometry.
        pub fn geometry(&self) -> Geometry2d<'a> {
            match self.json.geometry {
                kcad::curve::Geometry2d::Circle(ref json) => Geometry2d::Circle(Circle2d { json }),
                kcad::curve::Geometry2d::Line(ref json) => Geometry2d::Line(Line2d { json }),
                kcad::curve::Geometry2d::Nurbs(ref json) => Geometry2d::Nurbs(Nurbs2d { json }),
            }
        }
    }

    /// Curve kind.
    #[derive(Clone, Debug)]
    pub enum Geometry3d<'a> {
        /// Circular curve.
        Circle(Circle3d<'a>),
        /// Linear curve.
        Line(Line3d<'a>),
        /// Non-uniform rational B-spline (NURBS) curve.
        Nurbs(Nurbs3d<'a>),
    }

    /// Abstract curve.
    #[derive(Clone, Debug)]
    pub struct Curve3d<'a> {
        /// The corresponding JSON index.
        index: usize,

        /// The corresponding JSON struct.
        json: &'a kcad::Curve3d,
    }

    impl<'a> Curve3d<'a> {
        /// Constructs a `Curve`.
        pub fn new(index: usize, json: &'a kcad::Curve3d) -> Self {
            Self { index, json }
        }

        /// Returns the internal JSON index.
        pub fn index(&self) -> usize {
            self.index
        }

        /// Optional user-defined name for this object.
        #[cfg(feature = "names")]
        pub fn name(&self) -> Option<&'a str> {
            self.json.name.as_deref()
        }

        /// Evaluate the curve at parameter value `t`.
        pub fn evaluate(&self, t: f64) -> [f64; 3] {
            match self.geometry() {
                Geometry3d::Circle(circle) => circle.evaluate(t),
                Geometry3d::Line(line) => line.evaluate(t),
                Geometry3d::Nurbs(nurbs) => nurbs.evaluate(t),
            }
        }

        /// Returns the specific underlying curve geometry.
        pub fn geometry(&self) -> Geometry3d<'a> {
            match self.json.geometry {
                kcad::curve::Geometry3d::Circle(ref json) => Geometry3d::Circle(Circle3d { json }),
                kcad::curve::Geometry3d::Line(ref json) => Geometry3d::Line(Line3d { json }),
                kcad::curve::Geometry3d::Nurbs(ref json) => Geometry3d::Nurbs(Nurbs3d { json }),
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use gltf_json::extensions::kittycad_boundary_representation as kcad_json;
        use std::f64::consts::{FRAC_1_SQRT_2, PI};

        macro_rules! all_relative_eq {
            ($expected:expr, $actual:expr) => {{
                $expected
                    .iter()
                    .copied()
                    .zip($actual.iter().copied())
                    .all(|(a, b)| approx::relative_eq!(a, b, epsilon = 0.001))
            }};

            ($expected:expr, $actual:expr, epsilon = $epsilon:expr) => {{
                $expected
                    .iter()
                    .copied()
                    .zip($actual.iter().copied())
                    .all(|(a, b)| approx::relative_eq!(a, b, epsilon = $epsilon))
            }};
        }

        #[test]
        fn evaluate_nurbs_3d_arc_quadratic() {
            let inner = kcad_json::curve::Nurbs3d {
                control_points: vec![[1.0, 0.0, 0.0], [1.0, 1.0, 0.0], [0.0, 1.0, 0.0]],
                weights: vec![1.0, FRAC_1_SQRT_2, 1.0],
                knot_vector: vec![0.0, 0.0, 0.0, 1.0, 1.0, 1.0],
                order: 3,
            };
            let curve = super::Nurbs3d { json: &inner };

            let test_points = [
                (0.0, [1.0, 0.0, 0.0]),
                (0.25, [(PI * 0.125).cos(), (PI * 0.125).sin(), 0.0]),
                (0.5, [(PI * 0.25).cos(), (PI * 0.25).sin(), 0.0]),
                (0.75, [(PI * 0.375).cos(), (PI * 0.375).sin(), 0.0]),
                (1.0, [0.0, 1.0, 0.0]),
            ];

            for (i, (a, b)) in test_points.iter().copied().enumerate() {
                if !all_relative_eq!(curve.evaluate(a), b, epsilon = 0.1) {
                    panic!(
                        "test_points[{i}]: curve.evaluate({a:?}) = {:?} != {b:?}",
                        curve.evaluate(a)
                    );
                }
            }
        }

        #[test]
        fn evaluate_nurbs_3d_half_circle_cubic() {
            let inner = kcad_json::curve::Nurbs3d {
                control_points: vec![
                    [1.0, 0.0, 0.0],
                    [1.0, 2.0, 0.0],
                    [-1.0, 2.0, 0.0],
                    [-1.0, 0.0, 0.0],
                ],
                weights: vec![1.0, 1.0 / 3.0, 1.0 / 3.0, 1.0],
                knot_vector: vec![0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0],
                order: 4,
            };
            let curve = super::Nurbs3d { json: &inner };

            let test_points = [
                (0.0, [1.0, 0.0, 0.0]),
                (0.25, [0.8, 0.6, 0.0]),
                (0.5, [0.0, 1.0, 0.0]),
                (0.75, [-0.8, 0.6, 0.0]),
                (1.0, [-1.0, 0.0, 0.0]),
            ];

            for (i, (a, b)) in test_points.iter().copied().enumerate() {
                if !all_relative_eq!(curve.evaluate(a), b, epsilon = 0.1) {
                    panic!(
                        "test_points[{i}]: curve.evaluate({a:?}) = {:.2?} != {b:.2?}",
                        curve.evaluate(a)
                    );
                }
            }
        }

        #[test]
        fn evaluate_circle_3d_basic() {
            let curve = super::Circle3d {
                json: &kcad_json::curve::Circle3d {
                    axes: None,
                    origin: None,
                    radius: 2.0,
                },
            };

            let test_points = [
                (0.0, [2.0, 0.0, 0.0]),
                (0.5 * PI, [0.0, 2.0, 0.0]),
                (PI, [-2.0, 0.0, 0.0]),
                (-0.5 * PI, [0.0, -2.0, 0.0]),
            ];

            for (i, (a, b)) in test_points.iter().copied().enumerate() {
                if !all_relative_eq!(curve.evaluate(a), b) {
                    panic!(
                        "test_points[{i}]: curve.evaluate({a:?}) = {:?} != {b:?}",
                        curve.evaluate(a)
                    );
                }
            }
        }

        #[test]
        fn evaluate_circle_3d_offset() {
            let curve = super::Circle3d {
                json: &kcad_json::curve::Circle3d {
                    axes: None,
                    origin: Some([1.2, 3.4, 5.6]),
                    radius: 2.0,
                },
            };

            let test_points = [
                (0.0, [3.2, 3.4, 5.6]),
                (0.5 * PI, [1.2, 5.4, 5.6]),
                (PI, [-0.8, 3.4, 5.6]),
                (-0.5 * PI, [1.2, 1.4, 5.6]),
            ];

            for (i, (a, b)) in test_points.iter().copied().enumerate() {
                if !all_relative_eq!(curve.evaluate(a), b) {
                    panic!(
                        "test_points[{i}]: curve.evaluate({a:?}) = {:?} != {b:?}",
                        curve.evaluate(a)
                    );
                }
            }
        }
    }
}

/// Surfaces.
pub mod surface {
    use euler::DVec3;
    use json::extensions::kittycad_boundary_representation as kcad;

    /// Parametric cylindrical surface definition.
    ///
    /// σ(u, v) := O + R(cos(u)x + sin(u)y) + vz, where:
    /// * O = `self.circle().origin()`,
    /// * R = `self.circle().radius()`,
    /// * x = `self.circle().xbasis()`,
    /// * y = `self.circle().ybasis()`,
    /// * z = `self.circle().zbasis()`.
    ///
    /// In the field documentation, the 'base circle' is
    /// defined as the cycle defined at σ(u, 0).
    ///
    /// The vectors `xbasis`, `ybasis`, and `zbasis` form
    /// an orthonormal set.
    #[derive(Clone, Debug)]
    pub struct Cylinder<'a> {
        /// The corresponding JSON struct.
        pub(crate) json: &'a kcad::surface::Cylinder,
    }

    impl<'a> Cylinder<'a> {
        /// Local 'x' axis.
        pub fn xaxis(&self) -> [f64; 3] {
            self.json.axes.clone().unwrap_or_default().x
        }

        /// Local 'y' axis.
        pub fn yaxis(&self) -> [f64; 3] {
            self.json.axes.clone().unwrap_or_default().y
        }

        /// Local 'z' axis.
        pub fn zaxis(&self) -> [f64; 3] {
            DVec3::from(self.xaxis())
                .cross(DVec3::from(self.yaxis()))
                .into()
        }

        /// Origin of the base circle.
        pub fn origin(&self) -> [f64; 3] {
            self.json.origin.unwrap_or_default()
        }

        /// Radius of the base circle.
        pub fn radius(&self) -> f64 {
            self.json.radius
        }
    }

    /// Defines a planar surface.
    #[derive(Clone, Debug)]
    pub struct Plane<'a> {
        /// The corresponding JSON struct.
        pub(crate) json: &'a kcad::surface::Plane,
    }

    impl<'a> Plane<'a> {
        /// Local 'x' axis.
        pub fn xaxis(&self) -> [f64; 3] {
            self.json.axes.clone().unwrap_or_default().x
        }

        /// Local 'y' axis.
        pub fn yaxis(&self) -> [f64; 3] {
            self.json.axes.clone().unwrap_or_default().y
        }

        /// Local 'z' axis.
        pub fn zaxis(&self) -> [f64; 3] {
            DVec3::from(self.xaxis())
                .cross(DVec3::from(self.yaxis()))
                .into()
        }

        /// Arbitrary origin point on the plane.
        pub fn origin(&self) -> [f64; 3] {
            self.json.origin.unwrap_or_default()
        }

        /// Evaluate the plane at (u, v).
        pub fn evaluate(&self, [u, v]: [f64; 2]) -> [f64; 3] {
            let origin = DVec3::from(self.origin());
            let xaxis = DVec3::from(self.xaxis());
            let yaxis = DVec3::from(self.yaxis());
            (origin + xaxis * u + yaxis * v).into()
        }

        /// Find (u, v) for a point (x, y, z) on the plane.
        ///
        /// The result is unspecified if (x, y, z) does not lie on the plane
        /// within a reasonable tolerance.
        pub fn evaluate_inverse(&self, point: [f64; 3]) -> [f64; 2] {
            let origin = DVec3::from(self.origin());
            let offset = DVec3::from(point) - origin;
            let xaxis = DVec3::from(self.xaxis());
            let yaxis = DVec3::from(self.yaxis());
            [offset.dot(xaxis), offset.dot(yaxis)]
        }
    }

    /// Parametric spherical surface definition.
    #[derive(Clone, Debug)]
    pub struct Sphere<'a> {
        /// The corresponding JSON struct.
        pub(crate) json: &'a kcad::surface::Sphere,
    }

    impl<'a> Sphere<'a> {
        /// Local 'x' axis.
        pub fn xaxis(&self) -> [f64; 3] {
            self.json.axes.clone().unwrap_or_default().x
        }

        /// Local 'y' axis.
        pub fn yaxis(&self) -> [f64; 3] {
            self.json.axes.clone().unwrap_or_default().y
        }

        /// Local 'z' axis.
        pub fn zaxis(&self) -> [f64; 3] {
            DVec3::from(self.xaxis())
                .cross(DVec3::from(self.yaxis()))
                .into()
        }

        /// Origin (center) of the sphere.
        pub fn origin(&self) -> [f64; 3] {
            self.json.origin.unwrap_or_default()
        }

        /// Radius of the sphere.
        pub fn radius(&self) -> f64 {
            self.json.radius
        }

        /// Evaluate the sphere at (u, v).
        pub fn evaluate(&self, [u, v]: [f64; 2]) -> [f64; 3] {
            let r = self.radius();
            let a = DVec3::from(self.origin());
            let x = DVec3::from(self.xaxis());
            let y = DVec3::from(self.yaxis());
            let z = DVec3::from(self.zaxis());
            let (sin_u, cos_u) = u.sin_cos();
            let (sin_v, cos_v) = v.sin_cos();
            let b = a + r * cos_v * (cos_u * x + sin_u * y) + r * sin_v * z;
            b.into()
        }

        /// Find (u, v) for a point (x, y, z) on the sphere.
        ///
        /// The result is unspecified if (x, y, z) does not lie on the sphere
        /// within a reasonable tolerance.
        pub fn evaluate_inverse(&self, point: [f64; 3]) -> [f64; 2] {
            let origin = self.origin();
            let radius = self.radius();
            let xaxis = DVec3::from(self.xaxis());
            let yaxis = DVec3::from(self.yaxis());
            let zaxis = DVec3::from(self.zaxis());
            let ab = DVec3::from(point) - DVec3::from(origin);
            let u = ab.dot(yaxis).atan2(ab.dot(xaxis));
            let v = (ab.dot(zaxis) / radius).asin();
            [u, v]
        }
    }

    /// Toroidal surface definition.
    #[derive(Clone, Debug)]
    pub struct Torus<'a> {
        /// The corresponding JSON struct.
        pub(crate) json: &'a kcad::surface::Torus,
    }

    impl<'a> Torus<'a> {
        /// Local 'x' axis.
        pub fn xaxis(&self) -> [f64; 3] {
            self.json.axes.clone().unwrap_or_default().x
        }

        /// Local 'y' axis.
        pub fn yaxis(&self) -> [f64; 3] {
            self.json.axes.clone().unwrap_or_default().y
        }

        /// Local 'z' axis.
        pub fn zaxis(&self) -> [f64; 3] {
            DVec3::from(self.xaxis())
                .cross(DVec3::from(self.yaxis()))
                .into()
        }

        /// The center of the torus.
        pub fn origin(&self) -> [f64; 3] {
            self.json.origin.unwrap_or_default()
        }

        /// Distance from the torus origin to the origin of the revolved circle.
        pub fn major_radius(&self) -> f64 {
            self.json.major_radius
        }

        /// Distance of points away from the center of the revolved circle.
        pub fn minor_radius(&self) -> f64 {
            self.json.minor_radius
        }
    }

    /// Defines a non-uniform rational B-spline (NURBS) surface.
    #[derive(Clone, Debug)]
    pub struct Nurbs<'a> {
        /// The corresponding JSON struct.
        pub(crate) json: &'a kcad::surface::Nurbs,
    }

    impl<'a> Nurbs<'a> {
        /// Returns the matrix of control points.
        pub fn control_points(&self) -> &[[f64; 3]] {
            &self.json.control_points
        }

        /// Returns the matrix of control point weights.
        pub fn weights(&self) -> &[f64] {
            &self.json.weights
        }

        /// Returns the dimensions of the control point matrix.
        pub fn num_control_points(&self) -> [u32; 2] {
            self.json.num_control_points
        }

        /// Returns the number of knots in the U and V curves respectively.
        pub fn num_knots(&self) -> [u32; 2] {
            self.json.num_knots
        }

        /// Returns the knot vectors for the U and V curves respectively.
        pub fn knot_vectors(&self) -> (&[f64], &[f64]) {
            self.json
                .knot_vector
                .split_at(self.json.num_knots[0] as usize)
        }

        /// Returns the order of basis splines for the U and V curves respectively.
        pub fn orders(&self) -> [u32; 2] {
            self.json.order
        }

        /// Evaluate the surface at parameters `[u, v]`.
        pub fn evaluate(&self, [u, v]: [f64; 2]) -> [f64; 3] {
            use gltf_json::extensions::kittycad_boundary_representation as kcad_json;
            let p = self.control_points();
            let w = self.weights();
            let [un, _] = self.num_control_points().map(|n| n as usize);
            let (uk, vk) = self.knot_vectors();
            let [up, vp] = self.orders().map(|n| n as usize);
            let mut intermediates = Vec::new();
            for i in 0..un {
                let vstart = i * (vk.len() - vp);
                let vend = (i + 1) * (vk.len() - vp);
                let vslice = super::slice_truncated(p, vstart..vend);
                let wslice = super::slice_truncated(w, vstart..vend);
                let subcurve = kcad_json::curve::Nurbs3d {
                    control_points: vslice.to_vec(),
                    order: vp as u32,
                    knot_vector: vk.to_vec(),
                    weights: wslice.to_vec(),
                };
                let intermediate_point = super::curve::Nurbs3d { json: &subcurve }.evaluate(v);
                intermediates.push(intermediate_point);
            }
            let subcurve = kcad_json::curve::Nurbs3d {
                control_points: intermediates,
                order: up as u32,
                knot_vector: uk.to_vec(),
                weights: Vec::new(),
            };
            super::curve::Nurbs3d { json: &subcurve }.evaluate(u)
        }
    }

    /// Specific surface geometry.
    #[derive(Clone, Debug)]
    pub enum Geometry<'a> {
        /// Cylindrical surface.
        Cylinder(Cylinder<'a>),
        /// Non-uniform rational B-spline (NURBS) surface.
        Nurbs(Nurbs<'a>),
        /// Planar surface.
        Plane(Plane<'a>),
        /// Spherical surface.
        Sphere(Sphere<'a>),
        /// Toroidal surface.
        Torus(Torus<'a>),
    }

    impl<'a> Geometry<'a> {
        /// Evaluate the surface at parameters `[u, v]`.
        pub fn evaluate(&self, uv: [f64; 2]) -> [f64; 3] {
            match self {
                Geometry::Cylinder(_cylinder) => unimplemented!(),
                Geometry::Nurbs(nurbs) => nurbs.evaluate(uv),
                Geometry::Plane(plane) => plane.evaluate(uv),
                Geometry::Sphere(sphere) => sphere.evaluate(uv),
                Geometry::Torus(_torus) => unimplemented!(),
            }
        }
    }

    /// Abstract surface.
    #[derive(Clone, Debug)]
    pub struct Surface<'a> {
        /// The corresponding JSON index.
        index: usize,

        /// The corresponding JSON struct.
        json: &'a kcad::Surface,
    }

    impl<'a> Surface<'a> {
        /// Constructs a `Surface`.
        pub(crate) fn new(index: usize, json: &'a kcad::Surface) -> Self {
            Self { index, json }
        }

        /// Returns the internal JSON index.
        pub fn index(&self) -> usize {
            self.index
        }

        /// Optional user-defined name for this object.
        #[cfg(feature = "names")]
        pub fn name(&self) -> Option<&'a str> {
            self.json.name.as_deref()
        }

        /// Evaluate the surface at parameters `[u, v]`.
        pub fn evaluate(&self, uv: [f64; 2]) -> [f64; 3] {
            self.geometry().evaluate(uv)
        }

        /// Returns the specific underlying surface geometry.
        pub fn geometry(&self) -> Geometry<'a> {
            match self.json.geometry {
                kcad::surface::Geometry::Cylinder(ref json) => {
                    Geometry::Cylinder(Cylinder { json })
                }
                kcad::surface::Geometry::Nurbs(ref json) => Geometry::Nurbs(Nurbs { json }),
                kcad::surface::Geometry::Plane(ref json) => Geometry::Plane(Plane { json }),
                kcad::surface::Geometry::Sphere(ref json) => Geometry::Sphere(Sphere { json }),
                kcad::surface::Geometry::Torus(ref json) => Geometry::Torus(Torus { json }),
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use gltf_json::extensions::kittycad_boundary_representation as kcad_json;
        use std::f64::consts::PI;

        macro_rules! all_relative_eq {
            ($expected:expr, $actual:expr) => {{
                $expected
                    .iter()
                    .copied()
                    .zip($actual.iter().copied())
                    .all(|(a, b)| approx::relative_eq!(a, b, epsilon = 0.001))
            }};
        }

        #[test]
        fn evaluate_nurbs_basic() {
            let inner = kcad_json::surface::Nurbs {
                control_points: vec![
                    [20.0, 0.0, 0.0],
                    [20.0, 10.0, 0.0],
                    [20.0, 0.0, 0.0],
                    [20.0, 10.0, 0.0],
                    [0.0, 0.0, -50.0],
                    [0.0, 10.0, -50.0],
                    [-20.0, 0.0, 0.0],
                    [-20.0, 10.0, 0.0],
                ],
                num_control_points: [4, 2],
                num_knots: [8, 4],
                knot_vector: vec![0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 10.0, 10.0],
                order: [4, 2],
                weights: Default::default(),
            };
            let nurbs = super::Nurbs { json: &inner };

            let test_points = [
                ([0.0, 0.0], [20.0, 0.0, 0.0]),
                ([0.0, 2.5], [20.0, 2.5, 0.0]),
                ([0.0, 5.0], [20.0, 5.0, 0.0]),
                ([0.0, 7.5], [20.0, 7.5, 0.0]),
                ([0.0, 10.0], [20.0, 10.0, 0.0]),
                ([0.25, 0.0], [16.5625, 0.0, -7.03125]),
                ([0.25, 2.5], [16.5625, 2.5, -7.03125]),
                ([0.25, 5.0], [16.5625, 5.0, -7.03125]),
                ([0.25, 7.5], [16.5625, 7.5, -7.03125]),
                ([0.25, 10.0], [16.5625, 10.0, -7.03125]),
                ([0.5, 0.0], [7.5, 0.0, -18.75]),
                ([0.5, 2.5], [7.5, 2.5, -18.75]),
                ([0.5, 5.0], [7.5, 5.0, -18.75]),
                ([0.5, 7.5], [7.5, 7.5, -18.75]),
                ([0.5, 10.0], [7.5, 10.0, -18.75]),
                ([0.75, 0.0], [-5.3125, 0.0, -21.09375]),
                ([0.75, 2.5], [-5.3125, 2.5, -21.09375]),
                ([0.75, 5.0], [-5.3125, 5.0, -21.09375]),
                ([0.75, 7.5], [-5.3125, 7.5, -21.09375]),
                ([0.75, 10.0], [-5.3125, 10.0, -21.09375]),
                ([1.0, 0.0], [-20.0, 0.0, 0.0]),
                ([1.0, 2.5], [-20.0, 2.5, 0.0]),
                ([1.0, 5.0], [-20.0, 5.0, 0.0]),
                ([1.0, 7.5], [-20.0, 7.5, 0.0]),
                ([1.0, 10.0], [-20.0, 10.0, 0.0]),
            ];

            for (i, (a, b)) in test_points.iter().copied().enumerate() {
                if !all_relative_eq!(nurbs.evaluate(a), b) {
                    panic!(
                        "test_points[{i}]: nurbs.evaluate({a:?}) = {:?} != {b:?}",
                        nurbs.evaluate(a)
                    );
                }
            }
        }

        #[test]
        fn evaluate_plane_basic() {
            let plane = super::Plane {
                json: &kcad_json::surface::Plane {
                    axes: Some(kcad_json::Axes3d {
                        x: [0.0, 1.0, 0.0],
                        y: [0.0, 0.0, -1.0],
                    }),
                    origin: Some([1.2, 3.4, 5.6]),
                },
            };

            let test_points = [
                ([0.0, 0.0], [1.2, 3.4, 5.6]),
                ([1.0, 0.0], [1.2, 4.4, 5.6]),
                ([0.0, 1.0], [1.2, 3.4, 4.6]),
                ([2.0, 2.0], [1.2, 5.4, 3.6]),
                ([-1.0, 0.0], [1.2, 2.4, 5.6]),
                ([0.0, -1.0], [1.2, 3.4, 6.6]),
            ];

            for (i, (a, b)) in test_points.iter().copied().enumerate() {
                if !all_relative_eq!(plane.evaluate(a), b) {
                    panic!(
                        "test_points[{i}]: plane.evaluate({a:?}) = {:?} != {b:?}",
                        plane.evaluate(a)
                    );
                }
                if !all_relative_eq!(plane.evaluate_inverse(b), a) {
                    panic!(
                        "test_points[{i}]: plane.evaluate_inverse({b:?}) = {:?} != {a:?}",
                        plane.evaluate_inverse(b)
                    );
                }
            }
        }

        #[test]
        fn evaluate_sphere_basic() {
            let sphere = super::Sphere {
                json: &kcad_json::surface::Sphere {
                    axes: None,
                    origin: None,
                    radius: 2.0,
                },
            };

            let test_points = [
                ([0.0, 0.0], [2.0, 0.0, 0.0]),
                ([0.5 * PI, 0.0], [0.0, 2.0, 0.0]),
                ([PI, 0.0], [-2.0, 0.0, 0.0]),
                ([-0.5 * PI, 0.0], [0.0, -2.0, 0.0]),
                ([0.0, 0.5 * PI], [0.0, 0.0, 2.0]),
                ([0.0, -0.5 * PI], [0.0, 0.0, -2.0]),
            ];

            for (i, (a, b)) in test_points.iter().copied().enumerate() {
                if !all_relative_eq!(sphere.evaluate(a), b) {
                    panic!(
                        "test_points[{i}]: sphere.evaluate({a:?}) = {:?} != {b:?}",
                        sphere.evaluate(a)
                    );
                }
                if !all_relative_eq!(sphere.evaluate_inverse(b), a) {
                    panic!(
                        "test_points[{i}]: sphere.evaluate_inverse({b:?}) = {:?} != {a:?}",
                        sphere.evaluate_inverse(b)
                    );
                }
            }
        }

        #[test]
        fn evaluate_sphere_offset() {
            let offset = [1.2, 3.4, 5.6];
            let sphere = super::Sphere {
                json: &kcad_json::surface::Sphere {
                    axes: None,
                    origin: Some(offset),
                    radius: 2.0,
                },
            };

            let test_points = [
                ([0.0, 0.0], [3.2, 3.4, 5.6]),
                ([0.5 * PI, 0.0], [1.2, 5.4, 5.6]),
                ([PI, 0.0], [-0.8, 3.4, 5.6]),
                ([-0.5 * PI, 0.0], [1.2, 1.4, 5.6]),
                ([0.0, 0.5 * PI], [1.2, 3.4, 7.6]),
                ([0.0, -0.5 * PI], [1.2, 3.4, 3.6]),
            ];

            for (i, (a, b)) in test_points.iter().copied().enumerate() {
                if !all_relative_eq!(sphere.evaluate(a), b) {
                    panic!(
                        "test_points[{i}]: sphere.evaluate({a:?}) = {:?} != {b:?}",
                        sphere.evaluate(a)
                    );
                }
                if !all_relative_eq!(sphere.evaluate_inverse(b), a) {
                    panic!(
                        "test_points[{i}]: sphere.evaluate_inverse({b:?}) = {:?} != {a:?}",
                        sphere.evaluate_inverse(b)
                    );
                }
            }
        }
    }
}

/// Solid boundary representation structure.
#[derive(Clone, Debug)]
pub struct Solid<'a> {
    /// The parent `Document` struct.
    pub(crate) document: &'a Document,

    /// The corresponding JSON index.
    index: usize,

    /// The corresponding JSON struct.
    json: &'a kcad::Solid,
}

impl<'a> Solid<'a> {
    /// Constructs a `BRep`.
    pub(crate) fn new(document: &'a Document, index: usize, json: &'a kcad::Solid) -> Self {
        Self {
            document,
            index,
            json,
        }
    }

    /// Returns the internal JSON index.
    pub fn index(&self) -> usize {
        self.index
    }

    /// Optional user-defined name for this object.
    #[cfg(feature = "names")]
    pub fn name(&self) -> Option<&'a str> {
        self.json.name.as_deref()
    }

    /// Returns an `Iterator` that visits the solid's shells.
    pub fn shells(&self) -> impl ExactSizeIterator<Item = (Shell<'a>, Orientation)> {
        self.json
            .shells
            .iter()
            .map(|kcad::IndexWithOrientation(index, orientation)| {
                let shell = self.document.shells().unwrap().nth(index.value()).unwrap();
                (shell, *orientation)
            })
    }

    /// Returns the mesh approximation of this solid if defined.
    pub fn mesh(&self) -> Option<Mesh<'a>> {
        self.json
            .mesh
            .map(|index| self.document.meshes().nth(index.value()).unwrap())
    }
}

/// Closed boundary representation volume.
#[derive(Clone, Debug)]
pub struct Shell<'a> {
    /// The parent `Document` struct.
    pub(crate) document: &'a Document,

    /// The corresponding JSON index.
    index: usize,

    /// The corresponding JSON struct.
    json: &'a kcad::Shell,
}

impl<'a> Shell<'a> {
    /// Constructs a `Shell`.
    pub(crate) fn new(document: &'a Document, index: usize, json: &'a kcad::Shell) -> Self {
        Self {
            document,
            index,
            json,
        }
    }

    /// Returns the internal JSON index.
    pub fn index(&self) -> usize {
        self.index
    }

    /// Optional user-defined name for this object.
    #[cfg(feature = "names")]
    pub fn name(&self) -> Option<&'a str> {
        self.json.name.as_deref()
    }

    /// Returns an `Iterator` that visits the faces of the shell.
    pub fn faces(&self) -> impl ExactSizeIterator<Item = (Face<'a>, Orientation)> {
        self.json
            .faces
            .iter()
            .map(|kcad::IndexWithOrientation(index, orientation)| {
                let face = self.document.faces().unwrap().nth(index.value()).unwrap();
                (face, *orientation)
            })
    }
}

/// Set of vertices on a face plus trim curves.
#[derive(Clone, Debug)]
pub struct Loop<'a> {
    /// The parent `Document` struct.
    document: &'a Document,

    /// The corresponding JSON index.
    index: usize,

    /// The corresponding JSON struct.
    json: &'a kcad::Loop,
}

impl<'a> Loop<'a> {
    /// Constructs a `Loop`.
    pub(crate) fn new(document: &'a Document, index: usize, json: &'a kcad::Loop) -> Self {
        Self {
            document,
            index,
            json,
        }
    }

    /// Returns the internal JSON index.
    pub fn index(&self) -> usize {
        self.index
    }

    /// Returns an iterator that visits the 3D edges of the loop.
    pub fn edges(&self) -> impl ExactSizeIterator<Item = (Edge<'a>, Orientation)> {
        self.json
            .edges
            .iter()
            .map(|kcad::IndexWithOrientation(index, orientation)| {
                let edge = self.document.edges().unwrap().nth(index.value()).unwrap();
                (edge, *orientation)
            })
    }

    /// Returns an iterator that visits the corresponding 2D traces of the loop.
    pub fn traces(&self) -> impl ExactSizeIterator<Item = Trace<'a>> {
        self.json
            .traces
            .iter()
            .map(|json| Trace::new(self.document, json))
    }
}

/// Boundary representation of a solid.
#[derive(Clone, Debug)]
pub struct Face<'a> {
    /// The parent `Document` struct.
    document: &'a Document,

    /// The corresponding JSON index.
    index: usize,

    /// The corresponding JSON struct.
    json: &'a kcad::Face,
}

impl<'a> Face<'a> {
    /// Constructs a `Face`.
    pub(crate) fn new(document: &'a Document, index: usize, json: &'a kcad::Face) -> Self {
        Self {
            document,
            index,
            json,
        }
    }

    /// Returns the internal JSON index.
    pub fn index(&self) -> usize {
        self.index
    }

    /// Returns the face bounds.
    pub fn loops(&self) -> impl ExactSizeIterator<Item = (Loop<'a>, Orientation)> {
        self.json
            .loops
            .iter()
            .map(|kcad::IndexWithOrientation(index, orientation)| {
                let loop_ = self.document.loops().unwrap().nth(index.value()).unwrap();
                (loop_, *orientation)
            })
    }

    /// The surface this face is defined upon.
    pub fn surface(&self) -> (Surface<'a>, Orientation) {
        let surface = self
            .document
            .surfaces()
            .unwrap()
            .nth(self.json.surface.index().value())
            .unwrap();
        (surface, self.json.surface.orientation())
    }
}

/// Vertex in 3D space, joining edges.
#[derive(Clone, Debug)]
pub struct Vertex<'a> {
    /// The parent `Document` struct.
    #[allow(dead_code)]
    document: &'a Document,

    /// The corresponding JSON index.
    index: usize,

    /// The corresponding JSON struct.
    json: &'a kcad::Vertex,
}

impl<'a> Vertex<'a> {
    /// Constructs a `Vertex`.
    pub(crate) fn new(document: &'a Document, index: usize, json: &'a kcad::Vertex) -> Self {
        Self {
            document,
            index,
            json,
        }
    }

    /// Returns the internal JSON index.
    pub fn index(&self) -> usize {
        self.index
    }

    /// Returns the vertex position in 3D space.
    pub fn position(&self) -> [f64; 3] {
        self.json.0
    }
}

/// Face bound.
#[derive(Clone, Debug)]
pub struct Edge<'a> {
    /// The parent `Document` struct.
    document: &'a Document,

    /// The corresponding JSON index.
    index: usize,

    /// The corresponding JSON struct.
    json: &'a kcad::Edge,
}

/// Edge geometry.
pub enum Endpoints<'a> {
    /// This edge forms a loop.
    Closed,
    /// This edge has a distinct start and end vertex.
    Open {
        /// Edge start vertex.
        start: Vertex<'a>,
        /// Edge end vertex.
        end: Vertex<'a>,
    },
}

impl<'a> Edge<'a> {
    /// Constructs an `Edge`.
    pub(crate) fn new(document: &'a Document, index: usize, json: &'a kcad::Edge) -> Self {
        Self {
            document,
            index,
            json,
        }
    }

    /// Returns the internal JSON index.
    pub fn index(&self) -> usize {
        self.index
    }

    /// Returns the edge curve geometry in 3D (or homogeneous 4D) space.
    pub fn curve(&self) -> (Curve3d<'a>, Orientation) {
        let kcad::IndexWithOrientation(index, orientation) = self.json.curve;
        let curve = self
            .document
            .curves_3d()
            .unwrap()
            .nth(index.value())
            .unwrap();
        (curve, orientation)
    }

    /// Edge endpoints.
    ///
    /// Returns `None` if the edge is closed.
    pub fn endpoints(&self) -> Endpoints<'a> {
        if self.json.closed {
            Endpoints::Closed
        } else {
            let start = {
                let index = self.json.start.unwrap().value();
                self.document.vertices().unwrap().nth(index).unwrap()
            };
            let end = {
                let index = self.json.end.unwrap().value();
                self.document.vertices().unwrap().nth(index).unwrap()
            };
            Endpoints::Open { start, end }
        }
    }

    /// Returns the interval for the edge curve parameter 't'.
    pub fn t(&self) -> Interval {
        self.json.t.clone()
    }
}

/// A 2D curve across a surface.
#[derive(Clone, Debug)]
pub struct Trace<'a> {
    /// The parent `Document` struct.
    document: &'a Document,

    /// The corresponding JSON struct.
    json: &'a kcad::Trace,
}

impl<'a> Trace<'a> {
    /// Constructs a `Trace`.
    pub(crate) fn new(document: &'a Document, json: &'a kcad::Trace) -> Self {
        Self { document, json }
    }

    /// Returns the edge curve geometry in 3D (or homogeneous 4D) space.
    pub fn curve(&self) -> (Curve2d<'a>, Orientation) {
        let kcad::IndexWithOrientation(index, orientation) = self.json.curve;
        let curve = self
            .document
            .curves_2d()
            .unwrap()
            .nth(index.value())
            .unwrap();
        (curve, orientation)
    }

    /// Returns the interval for the trace curve parameter 't'.
    pub fn t(&self) -> Interval {
        self.json.t.clone()
    }
}
