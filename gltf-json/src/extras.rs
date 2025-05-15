use gltf_derive::Validate;
use serde_derive::{Deserialize, Serialize};
use std::fmt;

/// Data type of the `extras` attribute on all glTF objects.
#[cfg(feature = "extras")]
pub type Extras = std::collections::BTreeMap<String, serde_json::Value>;

/// Data type of the `extras` attribute on all glTF objects.
#[cfg(not(feature = "extras"))]
pub type Extras = Void;

/// Type representing no user-defined data.
#[derive(Clone, Default, Serialize, Deserialize, Validate)]
pub struct Void {
    #[serde(default, skip_serializing)]
    _allow_unknown_fields: (),
}

#[cfg(not(feature = "extras"))]
impl schemars::JsonSchema for Void {
    fn schema_name() -> String {
        "extras".to_owned()
    }

    fn json_schema(generator: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
        #[derive(Deserialize, schemars::JsonSchema, Serialize)]
        #[schemars(rename = "extras")]
        struct ExtrasSchema(
            #[serde(default)] std::collections::BTreeMap<String, serde_json::Value>,
        );
        ExtrasSchema::json_schema(generator)
    }
}

impl fmt::Debug for Void {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{}}")
    }
}

impl fmt::Display for Void {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{}}")
    }
}
