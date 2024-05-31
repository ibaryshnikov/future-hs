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

type MainCallback = extern "C" fn() -> RawFuture<()>;
type BlockingCallback = extern "C" fn() -> *const u8;
type ComposeCallback = extern "C" fn(ptr: *const u8) -> RawFuture<StablePtr>;

extern "C" {
    fn free_haskell_fun_ptr(ptr: usize);
    fn future_pair(ptr_a: *const u8, ptr_b: *const u8) -> *const u8;
    fn future_left(ptr: *const u8) -> *const u8;
    fn future_right(ptr: *const u8) -> *const u8;
}

macro_rules! free_fun_ptr {
    ($fun_ptr:ident) => {
        unsafe { free_haskell_fun_ptr($fun_ptr as usize) };
        #[allow(unused)]
        let $fun_ptr = ();
    };
}

#[no_mangle]
extern "C" fn future_run(callback: MainCallback) {
    let runtime = Runtime::new().expect("Should build a runtime");
    runtime.block_on(async move {
        let future_ptr = callback();
        free_fun_ptr!(callback);
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
extern "C" fn future_spawn_blocking(callback: BlockingCallback) -> RawFuture<StablePtr> {
    let handle = tokio::task::spawn_blocking(move || {
        let ptr = callback();
        free_fun_ptr!(callback);
        StablePtr { ptr }
    });
    let future = async move { handle.await.expect("Should join spawn_blocking handle") };
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
        let future_ptr_b = fab(a.ptr);
        free_fun_ptr!(fab);
        let future_b = unsafe { Box::from_raw(future_ptr_b) };
        future_b.await
    };
    let pinned = Box::pin(future);
    Box::into_raw(Box::new(pinned))
}

#[no_mangle]
extern "C" fn future_sequential(
    future_ptr_a: RawFuture<()>,
    future_ptr_b: RawFuture<StablePtr>,
) -> RawFuture<StablePtr> {
    let future_a = unsafe { Box::from_raw(future_ptr_a) };
    let future_b = unsafe { Box::from_raw(future_ptr_b) };
    let future = async move {
        future_a.await;
        future_b.await
    };
    let pinned = Box::pin(future);
    Box::into_raw(Box::new(pinned))
}

#[no_mangle]
extern "C" fn future_concurrent(
    future_ptr_a: RawFuture<StablePtr>,
    future_ptr_b: RawFuture<StablePtr>,
) -> RawFuture<StablePtr> {
    let future_a = unsafe { Box::from_raw(future_ptr_a) };
    let future_b = unsafe { Box::from_raw(future_ptr_b) };
    let future = async move {
        let (a, b) = tokio::join!(future_a, future_b);
        let ptr = unsafe { future_pair(a.ptr, b.ptr) };
        StablePtr { ptr }
    };
    let pinned = Box::pin(future);
    Box::into_raw(Box::new(pinned))
}

#[no_mangle]
extern "C" fn future_race(
    future_ptr_a: RawFuture<StablePtr>,
    future_ptr_b: RawFuture<StablePtr>,
) -> RawFuture<StablePtr> {
    let future_a = unsafe { Box::from_raw(future_ptr_a) };
    let future_b = unsafe { Box::from_raw(future_ptr_b) };
    let future = async move {
        let ptr = tokio::select! {
            a = future_a => unsafe { future_left(a.ptr) },
            b = future_b => unsafe { future_right(b.ptr) },
        };
        StablePtr { ptr }
    };
    let pinned = Box::pin(future);
    Box::into_raw(Box::new(pinned))
}
