use crate::{json, Document, Mesh};

/// Curves.
pub mod curve {
    use crate::math::Vector3;
    use crate::{json, Document};

    #[doc(inline)]
    pub use json::extensions::kittycad_boundary_representation::curve::Domain;

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
    #[derive(Clone, Debug)]
    pub struct Circle<'a> {
        /// The corresponding JSON struct.
        pub(crate) json: &'a json::extensions::kittycad_boundary_representation::curve::Circle,

        /// The curve domain.
        pub(crate) domain: Option<Domain>,
    }

    impl<'a> Circle<'a> {
        /// Position at the center of the circle.
        pub fn origin(&self) -> Option<[f32; 3]> {
            self.json.origin.clone()
        }

        /// Distance from the center position to all points on the circle.
        pub fn radius(&self) -> f32 {
            self.json.radius
        }

        /// Normal vector to the plane containing the circle.
        ///
        /// This serves as the Z basis in the parametric co-ordinate space.
        pub fn normal(&self) -> [f32; 3] {
            self.json.normal
        }

        /// Unit vector in the direction from the origin to the point on
        /// the circle at λ(0).
        ///
        /// Due to floating point precision, this vector may not lie exactly
        /// in the plane. If this is the case then the X vector is treated
        /// as the projection of this vector onto the plane.
        pub fn xbasis(&self) -> [f32; 3] {
            self.json.xbasis
        }

        /// Evaluate the curve at parameter value `t`.
        pub fn at(&self, t: f32) -> [f32; 3] {
            let radius = self.json.radius;
            let origin = Vector3::from(self.json.origin.unwrap_or_default());
            let xbasis = Vector3::from(self.json.xbasis);
            let ybasis = Vector3::from(self.json.normal).cross(xbasis);
            let (cosine, sine) = t.sin_cos();
            (origin + (xbasis * cosine + ybasis * sine) * radius).into()
        }

        /// Point evaluated at the domain minimum value.
        pub fn start(&self) -> [f32; 3] {
            if let Some(Domain { min, .. }) = self.domain {
                self.at(min)
            } else {
                self.at(0.0)
            }
        }

        /// Point evaluated at the domain maximum value.
        pub fn end(&self) -> [f32; 3] {
            if let Some(Domain { max, .. }) = self.domain {
                self.at(max)
            } else {
                self.start()
            }
        }
    }

    /// Defines a line curve.
    #[derive(Clone, Debug)]
    pub struct Line<'a> {
        /// The corresponding JSON struct.
        pub(crate) json: &'a json::extensions::kittycad_boundary_representation::curve::Line,

        /// The curve domain.
        pub(crate) domain: Domain,
    }

    impl<'a> Line<'a> {
        /// Returns the line origin.
        pub fn start(&self) -> [f32; 3] {
            self.json.start
        }

        /// Returns the line end point.
        ///
        /// If `direction` was set, this will be computed from the trim domain.
        pub fn end(&self) -> [f32; 3] {
            use crate::math::*;
            if let Some(end) = self.json.end {
                end
            } else {
                let start = {
                    let v = self.start();
                    Vector3::new(v[0], v[1], v[2])
                };
                let direction = {
                    let v = self.json.direction.unwrap();
                    Vector3::new(v[0], v[1], v[2])
                };
                let end = start + direction * (self.domain.max - self.domain.min);
                [end.x, end.y, end.z]
            }
        }

        /// Returns the line direction.
        ///
        /// If `end` was set, this will be computed.
        pub fn direction(&self) -> [f32; 3] {
            use crate::math::*;
            if let Some(direction) = self.json.direction {
                direction
            } else {
                let start = {
                    let v = self.start();
                    Vector3::new(v[0], v[1], v[2])
                };
                let end = {
                    let v = self.json.end.unwrap();
                    Vector3::new(v[0], v[1], v[2])
                };
                let difference = end + start * -1.0;
                let direction = difference.normalize();
                [direction.x, direction.y, direction.z]
            }
        }
    }

    /// Defines a non-uniform rational B-spline (NURBS) curve.
    #[derive(Clone, Debug)]
    pub struct Nurbs<'a> {
        /// The corresponding JSON struct.
        #[allow(dead_code)]
        pub(crate) json: &'a json::extensions::kittycad_boundary_representation::curve::Nurbs,
    }

    impl<'a> Nurbs<'a> {
        /// Returns the curve start point, i.e., the first control point.
        pub fn start(&self) -> [f32; 3] {
            // TODO: evaluate for domain.
            let v = self.json.control_points[0];
            [v[0], v[1], v[2]]
        }

        /// Returns the curve end point, i.e., the last control point.
        pub fn end(&self) -> [f32; 3] {
            // TODO: evaluate for domain.
            let v = self.json.control_points[self.json.control_points.len() - 1];
            [v[0], v[1], v[2]]
        }

        /// Returns the NURBS control points.
        pub fn control_points(&self) -> &[[f32; 4]] {
            &self.json.control_points
        }

        /// Returns the NURBS knot vector.
        pub fn knot_vector(&self) -> &[f32] {
            &self.json.knot_vector
        }

        /// Returns the order of the basis splines.
        ///
        /// # Notes
        ///
        /// The degree of the basis splines is one greater than the order.
        pub fn order(&self) -> u32 {
            self.json.order
        }
    }

    /// Curve kind.
    #[derive(Clone, Debug)]
    pub enum Geometry<'a> {
        /// Circular curve.
        Circle(Circle<'a>),
        /// Linear curve.
        Line(Line<'a>),
        /// Non-uniform rational B-spline (NURBS) curve.
        Nurbs(Nurbs<'a>),
    }

    /// Abstract curve..
    #[derive(Clone, Debug)]
    pub struct Curve<'a> {
        /// The parent `Document` struct.
        #[allow(unused)]
        document: &'a Document,

        /// The corresponding JSON index.
        index: usize,

        /// The corresponding JSON struct.
        json: &'a json::extensions::kittycad_boundary_representation::Curve,
    }

    impl<'a> Curve<'a> {
        /// Constructs a `Curve`.
        pub(crate) fn new(
            document: &'a Document,
            index: usize,
            json: &'a json::extensions::kittycad_boundary_representation::Curve,
        ) -> Self {
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

        /// Evaluates the curve start point.
        pub fn start(&self) -> [f32; 3] {
            match self.geometry() {
                Geometry::Circle(circle) => circle.start(),
                Geometry::Line(line) => line.start(),
                Geometry::Nurbs(nurbs) => nurbs.start(),
            }
        }

        /// Evaluates the curve end point.
        pub fn end(&self) -> [f32; 3] {
            match self.geometry() {
                Geometry::Circle(circle) => circle.end(),
                Geometry::Line(line) => line.end(),
                Geometry::Nurbs(nurbs) => nurbs.end(),
            }
        }

        /// Returns the specific curve parameters.
        pub fn geometry(&self) -> Geometry<'a> {
            match self.json.type_.unwrap() {
                json::extensions::kittycad_boundary_representation::curve::Type::Circle => {
                    let json = self.json.circle.as_ref().unwrap();
                    let domain = self.json.domain.clone();
                    Geometry::Circle(Circle { json, domain })
                }
                json::extensions::kittycad_boundary_representation::curve::Type::Line => {
                    let json = self.json.line.as_ref().unwrap();
                    let domain = self.json.domain.clone().unwrap_or_default();
                    Geometry::Line(Line { json, domain })
                }
                json::extensions::kittycad_boundary_representation::curve::Type::Nurbs => {
                    let json = self.json.nurbs.as_ref().unwrap();
                    Geometry::Nurbs(Nurbs { json })
                }
            }
        }

        /// Returns the range of the curve evaluation parameter.
        ///
        /// When the domain is `None`, assume 0 <= t <= 1.
        pub fn domain(&self) -> Option<Domain> {
            self.json.domain.clone()
        }
    }
}

/// Surfaces.
pub mod surface {
    use crate::{json, Document};

    #[doc(inline)]
    pub use json::extensions::kittycad_boundary_representation::surface::Domain;

    /// Parametric cylindrical surface definition.
    ///
    /// σ(u, v) := O + R(cos(u)x + sin(u)y) + vz, where:
    /// * O = `self.origin()`,
    /// * R = `self.radius()`,
    /// * x = `self.xbasis()`,
    /// * y = `self.ybasis()`,
    /// * z = `self.zbasis()`.
    ///
    /// In the field documentation, the 'base circle' is
    /// defined as the cycle defined at σ(u, 0).
    ///
    /// The vectors `xbasis`, `ybasis`, and `zbasis` form
    /// an orthonormal set.
    #[derive(Clone, Debug)]
    pub struct Cylinder<'a> {
        /// The corresponding JSON struct.
        pub(crate) json: &'a json::extensions::kittycad_boundary_representation::surface::Cylinder,
    }

    impl<'a> Cylinder<'a> {
        /// The extruded circle.
        pub fn circle(&self) -> super::curve::Circle<'a> {
            super::curve::Circle {
                json: &self.json.circle,
                domain: None,
            }
        }

        /// Height of the extruded circle.
        pub fn height(&self) -> f32 {
            self.json.height
        }
    }

    /// Defines a planar surface.
    #[derive(Clone, Debug)]
    pub struct Plane<'a> {
        /// The corresponding JSON struct.
        pub(crate) json: &'a json::extensions::kittycad_boundary_representation::surface::Plane,
    }

    impl<'a> Plane<'a> {
        /// Returns the normal vector to the plane.
        pub fn normal(&self) -> [f32; 3] {
            self.json.normal
        }

        /// Returns the value of `d` in the plane equation `n.r = d`.
        pub fn constant(&self) -> f32 {
            // TODO: compute constant where not provided.
            self.json.constant.unwrap()
        }

        /// Returns an arbitrary point that lies on the plane.
        pub fn point(&self) -> [f32; 3] {
            // TODO: compute point where not provided.
            self.json.point.unwrap()
        }
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
    #[derive(Clone, Debug)]
    pub struct Torus<'a> {
        /// The corresponding JSON struct.
        pub(crate) json: &'a json::extensions::kittycad_boundary_representation::surface::Torus,
    }

    impl<'a> Torus<'a> {
        /// The center of the torus.
        pub fn origin(&self) -> [f32; 3] {
            self.json.origin
        }

        /// The revolved circle.
        pub fn circle(&self) -> super::curve::Circle<'a> {
            super::curve::Circle {
                json: &self.json.circle,
                domain: None,
            }
        }

        /// Distance from the origin to the origin of the base circle.
        pub fn radius(&self) -> f32 {
            self.json.radius
        }
    }

    /// Defines a non-uniform rational B-spline (NURBS) surface.
    #[derive(Clone, Debug)]
    pub struct Nurbs<'a> {
        /// The corresponding JSON struct.
        #[allow(dead_code)]
        pub(crate) json: &'a json::extensions::kittycad_boundary_representation::surface::Nurbs,
    }

    impl<'a> Nurbs<'a> {
        /// Returns the matrix of control points.
        pub fn control_points(&self) -> &[[f32; 4]] {
            &self.json.control_points
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
        pub fn knot_vectors(&self) -> (&[f32], &[f32]) {
            self.json
                .knot_vector
                .split_at(self.json.num_knots[0] as usize)
        }

        /// Returns the order of basis splines for the U and V curves respectively.
        pub fn orders(&self) -> [u32; 2] {
            self.json.order
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
        /// Toroidal surface.
        Torus(Torus<'a>),
    }

    /// Abstract surface.
    #[derive(Clone, Debug)]
    pub struct Surface<'a> {
        /// The parent `Document` struct.
        #[allow(unused)]
        document: &'a Document,

        /// The corresponding JSON index.
        index: usize,

        /// The corresponding JSON struct.
        json: &'a json::extensions::kittycad_boundary_representation::Surface,
    }

    impl<'a> Surface<'a> {
        /// Constructs a `Surface`.
        pub(crate) fn new(
            document: &'a Document,
            index: usize,
            json: &'a json::extensions::kittycad_boundary_representation::Surface,
        ) -> Self {
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

        /// Returns the specific surface geometry.
        pub fn geometry(&self) -> Geometry<'a> {
            match self.json.type_.unwrap() {
                json::extensions::kittycad_boundary_representation::surface::Type::Cylinder => {
                    let json = self.json.cylinder.as_ref().unwrap();
                    Geometry::Cylinder(Cylinder { json })
                }
                json::extensions::kittycad_boundary_representation::surface::Type::Nurbs => {
                    let json = self.json.nurbs.as_ref().unwrap();
                    Geometry::Nurbs(Nurbs { json })
                }
                json::extensions::kittycad_boundary_representation::surface::Type::Plane => {
                    let json = self.json.plane.as_ref().unwrap();
                    Geometry::Plane(Plane { json })
                }
                json::extensions::kittycad_boundary_representation::surface::Type::Torus => {
                    let json = self.json.torus.as_ref().unwrap();
                    Geometry::Torus(Torus { json })
                }
            }
        }

        /// Returns the range of the surface evaluation parameters.
        ///
        /// When the domain is `None`, assume 0 <= u <= 1 and 0 <= v <= 1.
        pub fn domain(&self) -> Option<Domain> {
            self.json.domain.clone()
        }
    }
}

pub use curve::Curve;
pub use surface::Surface;

/// Boundary representation of a solid.
#[derive(Clone, Debug)]
pub struct BRep<'a> {
    /// The parent `Document` struct.
    pub(crate) document: &'a Document,

    /// The corresponding JSON index.
    index: usize,

    /// The corresponding JSON struct.
    json: &'a json::extensions::kittycad_boundary_representation::BRep,
}

impl<'a> BRep<'a> {
    /// Constructs a `BRep`.
    pub(crate) fn new(
        document: &'a Document,
        index: usize,
        json: &'a json::extensions::kittycad_boundary_representation::BRep,
    ) -> Self {
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

    /// Returns an `Iterator` that visits the faces of the B-rep.
    pub fn faces(&self) -> impl Iterator<Item = Face> {
        self.json
            .faces
            .iter()
            .map(|json| Face::new(self.document, json))
    }

    /// Returns the mesh approximation of this solid if defined.
    pub fn mesh(&self) -> Option<Mesh<'a>> {
        self.json
            .mesh
            .map(|index| self.document.meshes().nth(index.value()).unwrap())
    }
}

/// Trim curve across a surface in 2D space.
#[derive(Clone, Debug)]
pub struct Trim<'a> {
    /// The parent `Document` struct.
    document: &'a Document,

    /// The corresponding JSON struct.
    json: &'a json::extensions::kittycad_boundary_representation::brep::Trim,
}

impl<'a> Trim<'a> {
    /// Constructs a `Trim`.
    pub(crate) fn new(
        document: &'a Document,
        json: &'a json::extensions::kittycad_boundary_representation::brep::Trim,
    ) -> Self {
        Self { document, json }
    }

    /// Returns the edge this trim influences.
    pub fn edge(&self) -> Edge<'a> {
        let index = self.json.edge.value();
        self.document.edges().unwrap().nth(index).unwrap()
    }

    /// Specifies whether the trim curve orientation is in the reverse direction
    /// to its corresponding edge curve.
    pub fn reverse(&self) -> bool {
        self.json.reverse
    }
}

/// Set of vertices on a face plus trim curves.
#[derive(Clone, Debug)]
pub struct Loop<'a> {
    /// The parent `Document` struct.
    document: &'a Document,

    /// The corresponding JSON struct.
    json: &'a json::extensions::kittycad_boundary_representation::brep::Loop,
}

impl<'a> Loop<'a> {
    /// Constructs a `Loop`.
    pub(crate) fn new(
        document: &'a Document,
        json: &'a json::extensions::kittycad_boundary_representation::brep::Loop,
    ) -> Self {
        Self { document, json }
    }

    /// Returns the trim curves of the loop.
    pub fn trims(&self) -> impl Iterator<Item = Trim> {
        self.json
            .trims
            .iter()
            .map(|json| Trim::new(self.document, json))
    }

    /// Specifies whether the winding order of the loop should be
    /// interpreted in reverse order with respect to the face.
    pub fn reverse(&self) -> bool {
        self.json.reverse
    }
}

/// Boundary representation of a solid.
#[derive(Clone, Debug)]
pub struct Face<'a> {
    /// The parent `Document` struct.
    document: &'a Document,

    /// The corresponding JSON struct.
    json: &'a json::extensions::kittycad_boundary_representation::brep::Face,
}

impl<'a> Face<'a> {
    /// Constructs a `Face`.
    pub(crate) fn new(
        document: &'a Document,
        json: &'a json::extensions::kittycad_boundary_representation::brep::Face,
    ) -> Self {
        Self { document, json }
    }

    /// Returns the face outer loop.
    pub fn outer_loop(&self) -> Loop<'a> {
        Loop::new(self.document, &self.json.outer_loop)
    }

    /// Returns the inner loops of the face.
    pub fn inner_loops(&self) -> impl Iterator<Item = Loop> {
        self.json
            .inner_loops
            .iter()
            .map(|json| Loop::new(self.document, json))
    }

    /// The surface this face is defined upon.
    pub fn surface(&self) -> Surface<'a> {
        self.document
            .surfaces()
            .unwrap()
            .nth(self.json.surface.value())
            .unwrap()
    }

    /// Specifies whether the orientation of the face should
    /// be reversed.
    pub fn reverse(&self) -> bool {
        self.json.reverse
    }
}

/// Edge vertex.
#[derive(Clone, Debug)]
pub struct EdgeVertex<'a> {
    /// The parent `Document` struct.
    #[allow(dead_code)]
    document: &'a Document,

    /// The corresponding JSON index.
    index: usize,

    /// The corresponding JSON struct.
    json: &'a json::extensions::kittycad_boundary_representation::brep::EdgeVertex,
}

impl<'a> EdgeVertex<'a> {
    /// Constructs an `EdgeVertex`.
    pub(crate) fn new(
        document: &'a Document,
        index: usize,
        json: &'a json::extensions::kittycad_boundary_representation::brep::EdgeVertex,
    ) -> Self {
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
    pub fn position(&self) -> [f32; 3] {
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
    json: &'a json::extensions::kittycad_boundary_representation::brep::Edge,
}

impl<'a> Edge<'a> {
    /// Constructs an `Edge`.
    pub(crate) fn new(
        document: &'a Document,
        index: usize,
        json: &'a json::extensions::kittycad_boundary_representation::brep::Edge,
    ) -> Self {
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
    pub fn curve(&self) -> Curve<'a> {
        let index = self.json.curve.value();
        self.document.curves().unwrap().nth(index).unwrap()
    }

    /// Returns the edge start vertex.
    pub fn start(&self) -> EdgeVertex<'a> {
        let index = self.json.start.value();
        self.document.edge_vertices().unwrap().nth(index).unwrap()
    }

    /// Returns the edge end vertex.
    pub fn end(&self) -> EdgeVertex<'a> {
        let index = self.json.end.value();
        self.document.edge_vertices().unwrap().nth(index).unwrap()
    }

    /// Specifies whether the orientation of the edge curve should
    /// be reversed.
    pub fn reverse(&self) -> bool {
        self.json.reverse
    }

    /// Returns the optional subdomain that selects a subset of the curve.
    ///
    /// If this is `None` the the edge spans the whole domain of the curve.
    pub fn subdomain(&self) -> Option<curve::Domain> {
        self.json.subdomain.clone()
    }
}
