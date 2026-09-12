use arena_serde::ArenaDeserialize;

#[derive(ArenaDeserialize)]
enum Shape<'bump> {
    Circle,
    Label(&'bump str),
}

fn main() {}
