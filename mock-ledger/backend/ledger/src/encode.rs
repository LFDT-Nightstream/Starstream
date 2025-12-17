use wasmtime::component::ResourceTable;
use wasmtime_wasi::{WasiCtx, WasiCtxBuilder, WasiCtxView, WasiView};
use wrpc_runtime_wasmtime::{SharedResourceTable, WrpcCtxView, WrpcView};
use wrpc_transport::Invoke;
use core::time::Duration;

pub type ChainContext = ();

pub struct WrpcCtx<C: Invoke> {
    pub wrpc: C,
    pub cx: C::Context,
    pub shared_resources: SharedResourceTable,
    pub timeout: Duration,
}

pub struct Ctx<C: Invoke> {
    pub table: ResourceTable,
    // pub wasi: WasiCtx,
    pub wrpc: WrpcCtx<C>,
}

impl<C> wrpc_runtime_wasmtime::WrpcCtx<C> for WrpcCtx<C>
where
    C: Invoke,
    C::Context: Clone,
{
    fn context(&self) -> C::Context {
        self.cx.clone()
    }

    fn client(&self) -> &C {
        &self.wrpc
    }

    fn shared_resources(&mut self) -> &mut SharedResourceTable {
        &mut self.shared_resources
    }

    fn timeout(&self) -> Option<Duration> {
        Some(self.timeout)
    }
}

impl<C> WrpcView for Ctx<C>
where
    C: Invoke,
    C::Context: Clone,
{
    type Invoke = C;

    fn wrpc(&mut self) -> WrpcCtxView<'_, Self::Invoke> {
        WrpcCtxView {
            ctx: &mut self.wrpc,
            table: &mut self.table,
        }
    }
}

pub fn gen_ctx<C: Invoke>(
    wrpc: C,
    cx: C::Context,
) -> Ctx<C> {
    Ctx {
        table: ResourceTable::new(),
        // wasi: WasiCtxBuilder::new()
        //     .inherit_env()
        //     .inherit_stdio()
        //     .inherit_network()
        //     .allow_ip_name_lookup(true)
        //     .allow_tcp(true)
        //     .allow_udp(true)
        //     .args(&["your-program.wasm"]) // or whatever arg you need
        //     .build(),
        // http: WasiHttpCtx::new(),
        wrpc: WrpcCtx {
            wrpc,
            cx,
            shared_resources: SharedResourceTable::default(),
            timeout: Duration::from_secs(10), // TODO
        },
    }
}