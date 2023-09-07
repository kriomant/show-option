Rust library for diplaying `Option`s.

# Usage

```rust
use show_option::prelude::*;

println!("received bytes: {}", None::<usize>.show_or("none")); // "received bytes: none"
println!("amount: {}", Some(20).show_prefixed_or("$", "-")); // "amount: $20"
println!("amount: {}", format_option!(Some(20), "${}", "-")); // "amount: $20"
```
