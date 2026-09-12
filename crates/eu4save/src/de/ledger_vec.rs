pub fn ledger_vec_f32<'de, D>(deserializer: D) -> Result<Vec<f32>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    struct VecVisitor;

    impl<'de> serde::de::Visitor<'de> for VecVisitor {
        type Value = Vec<f32>;

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str("a seq of wrapping floats")
        }

        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where
            A: serde::de::SeqAccess<'de>,
        {
            let mut values = if let Some(size) = seq.size_hint() {
                Vec::with_capacity(size)
            } else {
                Vec::new()
            };

            while let Some(x) = seq.next_element::<f32>()? {
                values.push(unwrap_ledger_value(x));
            }

            Ok(values)
        }
    }

    deserializer.deserialize_seq(VecVisitor)
}

/// Ledger entries are just like casualties where large values are can be represented
/// by negative numbers. However, some ledger entries can be legitimately negative,
/// like selling ships causing a negative "building ships" expense.
fn unwrap_ledger_value(x: f32) -> f32 {
    const NEGATIVE_LEEWAY: f32 = 1000.0;

    let max_ledger_value = (i32::MAX / 1000) as f32;
    let min_ledger_value = -max_ledger_value;

    if x >= 0.0 {
        x
    } else if x > -NEGATIVE_LEEWAY {
        0.0
    } else if x > min_ledger_value {
        x + 2.0 * max_ledger_value
    } else {
        x.abs()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn positive_stays_positive() {
        assert_eq!(unwrap_ledger_value(0.0), 0.0);
        assert_eq!(unwrap_ledger_value(37.652), 37.652);
    }

    #[test]
    fn small_negative_is_nulled_out() {
        assert_eq!(unwrap_ledger_value(-25.225), 0.0);
        assert_eq!(unwrap_ledger_value(-999.999), 0.0);
    }

    #[test]
    fn large_negative_is_unwrapped() {
        let max_ledger_value = (i32::MAX / 1000) as f32;
        assert_eq!(
            unwrap_ledger_value(-1000.0),
            -1000.0 + 2.0 * max_ledger_value
        );
    }

    #[test]
    fn extreme_negative_is_abs() {
        let max_ledger_value = (i32::MAX / 1000) as f32;
        let extreme = -max_ledger_value - 1.0;
        assert_eq!(unwrap_ledger_value(extreme), extreme.abs());
    }
}
