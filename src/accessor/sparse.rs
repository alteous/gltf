use crate::{buffer, Document};

#[doc(inline)]
pub use json::accessor::sparse::IndexType;

/// Indices of those attributes that deviate from their initialization value.
pub struct Indices<'a> {
    /// The parent `Document` struct.
    document: &'a Document,

    /// The corresponding JSON struct.
    json: &'a json::accessor::sparse::Indices,
}

impl<'a> Indices<'a> {
    /// Constructs `sparse::Indices`.
    pub(crate) fn new(document: &'a Document, json: &'a json::accessor::sparse::Indices) -> Self {
        Self { document, json }
    }

    /// Returns the buffer view containing the sparse indices.
    pub fn view(&self) -> buffer::View<'a> {
        self.document
            .views()
            .nth(self.json.buffer_view.value())
            .unwrap()
    }

    /// The offset relative to the start of the parent buffer view in bytes.
    pub fn offset(&self) -> usize {
        self.json.byte_offset.0 as usize
    }

    /// The data type of each index.
    pub fn index_type(&self) -> IndexType {
        self.json.index_type
    }

    /// Optional application specific data.
    pub fn extras(&self) -> &'a json::Extras {
        &self.json.extras
    }
}

/// Sparse storage of attributes that deviate from their initialization value.
pub struct Sparse<'a> {
    /// The parent `Document` struct.
    document: &'a Document,

    /// The corresponding JSON struct.
    json: &'a json::accessor::sparse::Sparse,
}

impl<'a> Sparse<'a> {
    /// Constructs `Sparse`.
    pub(crate) fn new(document: &'a Document, json: &'a json::accessor::sparse::Sparse) -> Self {
        Self { document, json }
    }

    /// Returns the number of attributes encoded in this sparse accessor.
    pub fn count(&self) -> usize {
        self.json.count.0 as usize
    }

    /// Returns an index array of size `count` that points to those accessor
    /// attributes that deviate from their initialization value.
    pub fn indices(&self) -> Indices<'a> {
        Indices::new(self.document, &self.json.indices)
    }

    /// Returns an array of size `count * number_of_components`, storing the
    /// displaced accessor attributes pointed by `indices`.
    pub fn values(&self) -> Values<'a> {
        Values::new(self.document, &self.json.values)
    }

    /// Optional application specific data.
    pub fn extras(&self) -> &'a json::Extras {
        &self.json.extras
    }
}

/// Array of size `count * number_of_components` storing the displaced accessor
/// attributes pointed by `accessor::sparse::Indices`.
pub struct Values<'a> {
    /// The parent `Document` struct.
    document: &'a Document,

    /// The corresponding JSON struct.
    json: &'a json::accessor::sparse::Values,
}

impl<'a> Values<'a> {
    /// Constructs `sparse::Values`.
    pub(crate) fn new(document: &'a Document, json: &'a json::accessor::sparse::Values) -> Self {
        Self { document, json }
    }

    /// Returns the buffer view containing the sparse values.
    pub fn view(&self) -> buffer::View<'a> {
        self.document
            .views()
            .nth(self.json.buffer_view.value())
            .unwrap()
    }

    /// The offset relative to the start of the parent buffer view in bytes.
    pub fn offset(&self) -> usize {
        self.json.byte_offset.0 as usize
    }

    /// Optional application specific data.
    pub fn extras(&self) -> &'a json::Extras {
        &self.json.extras
    }
}
