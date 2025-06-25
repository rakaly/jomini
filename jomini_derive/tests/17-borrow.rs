use jomini_derive::JominiDeserialize;
use std::borrow::Cow;

// Basic borrowing with multiple lifetimes (as per original spec)
#[derive(Debug, PartialEq, JominiDeserialize)]
struct Inner<'a, 'b> {
    // &str and &[u8] are implicitly borrowed.
    username: &'a str,

    // Other types must be borrowed explicitly.
    #[jomini(borrow)]
    comment: Cow<'b, str>,
}

// Composition with nested borrowed structs (as per original spec)
#[derive(Debug, PartialEq, JominiDeserialize)]
struct Gamestate<'a> {
    #[jomini(duplicated, borrow)]
    active_war: Vec<ActiveWar<'a>>,
}

#[derive(Debug, PartialEq, JominiDeserialize)]
struct ActiveWar<'a> {
    name: &'a str,
}

// Comprehensive test struct covering all attribute combinations
#[derive(Debug, PartialEq, JominiDeserialize)]
struct ComprehensiveStruct<'a> {
    // Basic implicit borrowing
    name: &'a str,

    // Explicit borrowing
    #[jomini(borrow)]
    description: Cow<'a, str>,

    // Borrow + alias
    #[jomini(borrow, alias = "alt_name")]
    alternative: Cow<'a, str>,

    // Borrow + duplicated
    #[jomini(borrow, duplicated)]
    tags: Vec<Cow<'a, str>>,

    // Borrow + default (optional)
    #[jomini(borrow, default)]
    optional_field: Option<Cow<'a, str>>,

    // Mixed with non-borrowed fields
    count: u32,

    #[jomini(default)]
    enabled: bool,

    #[jomini(alias = "nums", duplicated)]
    numbers: Vec<i32>,
}

// Custom deserialize function with borrowing
fn custom_deserialize<'de, D>(deserializer: D) -> Result<&'de str, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let s: &str = serde::Deserialize::deserialize(deserializer)?;
    Ok(s)
}

// Test struct with custom deserialize and borrowing
#[derive(Debug, PartialEq, JominiDeserialize)]
struct BorrowWithCustom<'a> {
    #[jomini(deserialize_with = "custom_deserialize")]
    name: &'a str,
}

// Test struct with multiple lifetime parameters
#[derive(Debug, PartialEq, JominiDeserialize)]
struct MultiLifetime<'a, 'b, 'c> {
    name: &'a str,
    #[jomini(borrow)]
    description: Cow<'b, str>,
    #[jomini(borrow)]
    metadata: Cow<'c, str>,
    count: u32,
}

#[test]
fn test_basic_borrowing() {
    let data = r#"
        {
            "username": "alice",
            "comment": "Hello world!"
        }"#;

    let inner: Inner = serde_json::from_str(data).unwrap();
    assert_eq!(inner.username, "alice");
    assert_eq!(inner.comment, Cow::from("Hello world!"));
}

#[test]
fn test_composition_with_duplicated() {
    let data = r#"
        {
            "active_war": {"name": "War of 1812"},
            "active_war": {"name": "Civil War"}
        }"#;

    let gamestate: Gamestate = serde_json::from_str(data).unwrap();
    assert_eq!(gamestate.active_war.len(), 2);
    assert_eq!(gamestate.active_war[0].name, "War of 1812");
    assert_eq!(gamestate.active_war[1].name, "Civil War");
}

#[test]
fn test_zero_copy_verification() {
    let data = r#"{"username": "bob", "comment": "test"}"#;
    let inner: Inner = serde_json::from_str(data).unwrap();

    // Verify that the borrowed data works correctly
    assert_eq!(inner.username, "bob");
    assert_eq!(inner.comment, Cow::Borrowed("test"));
}

#[test]
fn test_multiple_lifetimes() {
    let data = r#"{"username": "charlie", "comment": "mixed lifetimes work"}"#;
    let inner: Inner = serde_json::from_str(data).unwrap();

    assert_eq!(inner.username, "charlie");
    assert_eq!(inner.comment, "mixed lifetimes work");
}

#[test]
fn test_comprehensive_attributes() {
    let data = r#"
    {
        "name": "test_name",
        "description": "test_description", 
        "alt_name": "alternative_name",
        "tags": "tag1",
        "tags": "tag2",
        "optional_field": "optional_value",
        "count": 42,
        "enabled": true,
        "nums": 1,
        "nums": 2,
        "nums": 3
    }"#;

    let result: ComprehensiveStruct = serde_json::from_str(data).unwrap();

    assert_eq!(result.name, "test_name");
    assert_eq!(result.description, Cow::from("test_description"));
    assert_eq!(result.alternative, Cow::from("alternative_name"));
    assert_eq!(result.tags, vec![Cow::from("tag1"), Cow::from("tag2")]);
    assert_eq!(result.optional_field, Some(Cow::from("optional_value")));
    assert_eq!(result.count, 42);
    assert_eq!(result.enabled, true);
    assert_eq!(result.numbers, vec![1, 2, 3]);
}

#[test]
fn test_missing_optional_fields() {
    let data = r#"
    {
        "name": "test_name",
        "description": "test_description",
        "alt_name": "alternative_name",
        "count": 42,
        "enabled": true
    }"#;

    let result: ComprehensiveStruct = serde_json::from_str(data).unwrap();

    assert_eq!(result.optional_field, None);
    assert_eq!(result.tags, Vec::<Cow<str>>::new());
    assert_eq!(result.numbers, Vec::<i32>::new());
}

#[test]
fn test_borrow_with_custom_deserialize() {
    let data = r#"{"name": "custom"}"#;
    let result: BorrowWithCustom = serde_json::from_str(data).unwrap();
    assert_eq!(result.name, "custom");
}

#[test]
fn test_complex_multi_lifetime() {
    let data = r#"
    {
        "name": "multi_test",
        "description": "desc",
        "metadata": "meta",
        "count": 789
    }"#;

    let result: MultiLifetime = serde_json::from_str(data).unwrap();

    assert_eq!(result.name, "multi_test");
    assert_eq!(result.description, Cow::from("desc"));
    assert_eq!(result.metadata, Cow::from("meta"));
    assert_eq!(result.count, 789);
}
