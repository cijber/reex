async function main() {
   let module = await import("./pkg/reex_web.js");
   await module.default('/pkg/reex_web_bg.wasm');
   module.run_app();
}

main()