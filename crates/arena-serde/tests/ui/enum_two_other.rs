use arena_serde::ArenaDeserialize;

#[derive(ArenaDeserialize)]
enum Kind {
    #[arena(other)]
    First,
    #[arena(other)]
    Second,
}

fn main() {}
