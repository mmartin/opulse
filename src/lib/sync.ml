open Core
open Ctypes

type sink_input_info_t =
  { index : int
  ; name : string
  ; proplist : string String.Map.t
  ; volume : float list
  ; mute : bool
  }
[@@deriving make]

type t =
  { mainloop : Bindings.pa_threaded_mainloop
  ; context : Bindings.pa_context
  }

exception Pulse_error of string

let connect client_name =
  let mainloop = Bindings.pa_threaded_mainloop_new () in
  let mainloop_api = Bindings.pa_threaded_mainloop_get_api mainloop in
  let context = Bindings.pa_context_new mainloop_api client_name in
  let connected = ref false in
  Bindings.pa_context_connect context null 0 null |> ignore;
  Bindings.pa_threaded_mainloop_start mainloop |> ignore;
  while not !connected do
    let state = Bindings.pa_context_get_state context in
    match state with
    | `PA_CONTEXT_READY -> connected := true
    | `PA_CONTEXT_FAILED | `PA_CONTEXT_TERMINATED -> failwith "Failed to connect"
    | _ -> Bindings.pa_threaded_mainloop_wait mainloop
  done;
  { mainloop; context }
;;

let rec wait_for_operation pulse operation =
  match operation with
  | Some operation ->
    let rec loopy_loop () =
      match Bindings.pa_operation_get_state operation with
      | `PA_OPERATION_DONE -> Bindings.pa_operation_unref operation
      | `PA_OPERATION_RUNNING ->
        Bindings.pa_threaded_mainloop_wait pulse.mainloop;
        (loopy_loop [@tailcall]) ()
      | `PA_OPERATION_CANCELLED ->
        Bindings.pa_threaded_mainloop_unlock pulse.mainloop;
        raise @@ Pulse_error "Operation got cancelled"
    in
    loopy_loop ()
  | None ->
    let error = Bindings.pa_context_errno pulse.context |> Bindings.pa_strerror in
    Bindings.pa_threaded_mainloop_unlock pulse.mainloop;
    raise @@ Pulse_error ("Failed to execute operation: " ^ error)
;;

let context_success_cb pulse ok _ success _ =
  ok := success > 0;
  Bindings.pa_threaded_mainloop_signal pulse.mainloop 0
;;

let read_proplist proplist =
  let state = allocate_n ~count:1 (ptr void) in
  let rec loop map =
    match Bindings.pa_proplist_iterate proplist state with
    | Some key ->
      let data = Bindings.pa_proplist_gets proplist key |> Option.value ~default:"" in
      (loop [@tailcall]) (Map.add_exn map ~key ~data)
    | None -> map
  in
  loop String.Map.empty
;;

let read_volume volume =
  let channles = Unsigned.UInt8.to_int (getf volume Bindings.Pa_cvolume.channels) in
  let values = getf volume Bindings.Pa_cvolume.values in
  List.take (CArray.to_list values) channles
  |> List.map ~f:(fun value ->
    Float.(
      round_decimal ~decimal_digits:2
      @@ (of_int (Unsigned.UInt32.to_int value) / of_int 0x10000)))
;;

let transform_sink_input_info info =
  let info = !@info in
  let index = Unsigned.UInt32.to_int (getf info Bindings.Pa_sink_input_info.index)
  and name = getf info Bindings.Pa_sink_input_info.name
  and mute = getf info Bindings.Pa_sink_input_info.mute
  and volume = read_volume (getf info Bindings.Pa_sink_input_info.volume)
  and proplist = read_proplist (getf info Bindings.Pa_sink_input_info.proplist) in
  make_sink_input_info_t ~index ~name ~volume ~mute ~proplist ()
;;

let get_sink_input_by_index pulse index =
  Bindings.pa_threaded_mainloop_lock pulse.mainloop;
  let result = ref None in
  let operation =
    Bindings.pa_context_get_sink_input_info
      pulse.context
      (Unsigned.UInt32.of_int index)
      (fun _ info eol _ ->
        if eol = 0 then result := Some (transform_sink_input_info info);
        Bindings.pa_threaded_mainloop_signal pulse.mainloop 0)
      null
  in
  wait_for_operation pulse operation;
  Bindings.pa_threaded_mainloop_unlock pulse.mainloop;
  !result
;;

let get_sink_input_list pulse =
  Bindings.pa_threaded_mainloop_lock pulse.mainloop;
  let result = ref [] in
  let operation =
    Bindings.pa_context_get_sink_input_info_list
      pulse.context
      (fun _ info eol _ ->
        if eol = 0 then result := transform_sink_input_info info :: !result;
        Bindings.pa_threaded_mainloop_signal pulse.mainloop 0)
      null
  in
  wait_for_operation pulse operation;
  Bindings.pa_threaded_mainloop_unlock pulse.mainloop;
  !result
;;

let set_sink_input_volume pulse index ~volume ~channels =
  Bindings.pa_threaded_mainloop_lock pulse.mainloop;
  let volume = Float.(to_int (volume * of_int 0x10000)) in
  let cvolume = allocate_n ~count:1 Bindings.Pa_cvolume.t in
  let cvolume =
    Bindings.pa_cvolume_set
      cvolume
      (Unsigned.UInt.of_int channels)
      (Unsigned.UInt32.of_int volume)
  in
  let success = ref false in
  let operation =
    Bindings.pa_context_set_sink_input_volume
      pulse.context
      (Unsigned.UInt32.of_int index)
      cvolume
      (context_success_cb pulse success)
      null
  in
  wait_for_operation pulse operation;
  Bindings.pa_threaded_mainloop_unlock pulse.mainloop;
  if not !success then raise (Pulse_error "Failed to set sink input volume")
;;

let set_sink_input_mute pulse index mute =
  Bindings.pa_threaded_mainloop_lock pulse.mainloop;
  let success = ref false in
  let operation =
    Bindings.pa_context_set_sink_input_mute
      pulse.context
      (Unsigned.UInt32.of_int index)
      mute
      (context_success_cb pulse success)
      null
  in
  wait_for_operation pulse operation;
  Bindings.pa_threaded_mainloop_unlock pulse.mainloop;
  if not !success then raise (Pulse_error "Failed to set sink input mute")
;;

let subscribe pulse callback =
  Bindings.pa_threaded_mainloop_lock pulse.mainloop;
  Bindings.pa_context_set_subscribe_callback
    pulse.context
    (fun _ (f, e) idx _ ->
      callback f e (Unsigned.UInt32.to_int idx);
      Bindings.pa_threaded_mainloop_signal pulse.mainloop 0)
    null;
  let success = ref false in
  let operation =
    Bindings.pa_context_subscribe
      pulse.context
      `PA_SUBSCRIPTION_MASK_SINK_INPUT
      (context_success_cb pulse success)
      null
  in
  wait_for_operation pulse operation;
  Bindings.pa_threaded_mainloop_unlock pulse.mainloop;
  if not !success then raise (Pulse_error "Failed to subscribe")
;;
