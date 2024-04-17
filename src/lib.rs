use std::future::Future;
use std::pin::Pin;

use tokio::runtime::Runtime;

mod time;

struct StablePtr {
    ptr: *const u8,
}
unsafe impl Send for StablePtr {}

type PinnedFuture<T> = Pin<Box<dyn Future<Output = T> + Send>>;
type RawFuture<T> = *mut PinnedFuture<T>;
type IOCallback = unsafe extern "C" fn() -> *const u8;
type ComposeCallback = unsafe extern "C" fn(ptr: *const u8) -> RawFuture<StablePtr>;

#[no_mangle]
extern "C" fn future_run(future_ptr: RawFuture<()>) {
    let runtime = Runtime::new().expect("Should build a runtime");
    runtime.block_on(async move {
        let future = unsafe { Box::from_raw(future_ptr) };
        future.await;
    });
}

#[no_mangle]
extern "C" fn future_wrap_value(ptr: *const u8) -> RawFuture<StablePtr> {
    let value = StablePtr { ptr };
    let future = async move { value };
    let pinned = Box::pin(future);
    Box::into_raw(Box::new(pinned))
}

#[no_mangle]
extern "C" fn future_wrap_io(callback: IOCallback) -> RawFuture<StablePtr> {
    let ptr = unsafe { callback() };
    let value = StablePtr { ptr };
    let future = async move { value };
    let pinned = Box::pin(future);
    Box::into_raw(Box::new(pinned))
}

#[no_mangle]
extern "C" fn future_compose(
    future_ptr_a: RawFuture<StablePtr>,
    fab: ComposeCallback,
) -> RawFuture<StablePtr> {
    let future_a = unsafe { Box::from_raw(future_ptr_a) };
    let future = async move {
        let a = future_a.await;
        let future_ptr_b = unsafe { fab(a.ptr) };
        let future_b = unsafe { Box::from_raw(future_ptr_b) };
        future_b.await
    };
    let pinned = Box::pin(future);
    Box::into_raw(Box::new(pinned))
}
