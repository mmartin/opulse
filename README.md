# Opulse
PulseAudio bindings for OCaml

🚨 WARNING: Very incomplete and rough around the edges. Use at your own risk. 🚨

# Modules

* `Opulse.Bindings` - raw `Ctypes` bindings
* `Opulse.Sync` - high-level synchronous API

# Example

Get all Sink Inputs and set their volume to 69%

```ocaml
open Core
open Opulse.Sync

let () =
  let pulse = connect "Opulse" in
  get_sink_input_list pulse
  |> List.iter ~f:(fun sink_input ->
    let channels = List.length sink_input.volume in
    set_sink_input_volume pulse sink_input.index ~volume:0.69 ~channels)
;;
```
