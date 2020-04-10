use gc::value::Value;
use gc::object::Object;
use broom::Heap;

pub fn clock(_heap: &Heap<Object>, _args: &[Value]) -> Value {
    use std::time::{SystemTime, UNIX_EPOCH};

    let epoch_time =
        SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("[FATAL] failed to get system time")
        .as_secs_f64();

    Value::float(epoch_time)
}

pub fn native_print(heap: &Heap<Object>, args: &[Value]) -> Value {
    println!("{}", args[1].with_heap(heap));
    Value::nil()
}
