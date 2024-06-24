use schemars::JsonSchema;

/// Provides a JSON schema for the extensions object.
pub fn extensions(generator: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
    #[derive(schemars::JsonSchema, serde_derive::Serialize)]
    struct Extensions {}
    Extensions::json_schema(generator)
}

///// Provides a JSON schemas for the `Extras` type.
pub fn extras(generator: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
    #[derive(schemars::JsonSchema, serde_derive::Serialize)]
    struct Extras {}
    Extras::json_schema(generator)
}
