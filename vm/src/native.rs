use gc::value::Value;
use gc::object::Object;
use broom::Heap;

pub fn clock(_heap: &Heap<Object>, _args: &[Value]) -> Value {
    use std::time::{SystemTime, UNIX_EPOCH};

    let epoch_time =
        SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("[FATAL] failed to get system time");

    let time_ms = epoch_time.as_secs() * 1000 + epoch_time.subsec_nanos() as u64 / 1_000_000;

    Value::float(time_ms as f64)
}

pub fn native_print(heap: &Heap<Object>, args: &[Value]) -> Value {
    println!("{}", args[1].with_heap(heap));
    Value::nil()
}
