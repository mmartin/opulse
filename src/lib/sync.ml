open Core
open Ctypes

type sink_input_info_t =
  { index : int
  ; name : string
  ; proplist : string String.Map.t
  ; volume : float list
  ; mute : bool
  ; corked : bool
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

let fail message errno =
  let error = Bindings.pa_strerror errno in
  raise (Pulse_error (Printf.sprintf "%s(%d): %s" message errno error))
;;

let failwith_context context message = Bindings.pa_context_errno context |> fail message
let check_error message ret = if ret <> 0 then fail message ret

let wait_for_operation pulse operation =
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
    Bindings.pa_threaded_mainloop_unlock pulse.mainloop;
    failwith_context pulse.context "Failed to execute operation"
;;

let context_success_cb pulse ok _ success _ =
  ok := success > 0;
  Bindings.pa_threaded_mainloop_signal pulse.mainloop 0
;;

let read_proplist proplist =
  let state = allocate (ptr void) null in
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
  and volume = getf info Bindings.Pa_sink_input_info.volume |> read_volume
  and corked = getf info Bindings.Pa_sink_input_info.corked = 1
  and proplist = read_proplist (getf info Bindings.Pa_sink_input_info.proplist) in
  make_sink_input_info_t ~index ~name ~volume ~mute ~corked ~proplist ()
;;

let get_sink_input_by_index pulse index =
  Bindings.pa_threaded_mainloop_lock pulse.mainloop;
  let result = ref None in
  let op_callback _ info eol _ =
    if eol = 0 then result := Some (transform_sink_input_info info);
    Bindings.pa_threaded_mainloop_signal pulse.mainloop 0
  in
  let operation =
    Bindings.pa_context_get_sink_input_info
      pulse.context
      (Unsigned.UInt32.of_int index)
      op_callback
      null
  in
  wait_for_operation pulse operation;
  Gc.keep_alive op_callback;
  Bindings.pa_threaded_mainloop_unlock pulse.mainloop;
  !result
;;

let get_sink_input_list pulse =
  Bindings.pa_threaded_mainloop_lock pulse.mainloop;
  let result = ref [] in
  let op_callback _ info eol _ =
    if eol = 0 then result := transform_sink_input_info info :: !result;
    Bindings.pa_threaded_mainloop_signal pulse.mainloop 0
  in
  let operation =
    Bindings.pa_context_get_sink_input_info_list pulse.context op_callback null
  in
  wait_for_operation pulse operation;
  Gc.keep_alive op_callback;
  Bindings.pa_threaded_mainloop_unlock pulse.mainloop;
  !result
;;

let set_sink_input_volume pulse index ~volume ~channels =
  Bindings.pa_threaded_mainloop_lock pulse.mainloop;
  let volume = Float.(to_int (volume * of_int 0x10000)) in
  let cvolume = make Bindings.Pa_cvolume.t in
  let _ = Bindings.pa_cvolume_init (addr cvolume) in
  let _ =
    Bindings.pa_cvolume_set
      (addr cvolume)
      (Unsigned.UInt.of_int channels)
      (Unsigned.UInt32.of_int volume)
  in
  let success = ref false in
  let op_callback = context_success_cb pulse success in
  let operation =
    Bindings.pa_context_set_sink_input_volume
      pulse.context
      (Unsigned.UInt32.of_int index)
      (addr cvolume)
      op_callback
      null
  in
  wait_for_operation pulse operation;
  Gc.keep_alive op_callback;
  Bindings.pa_threaded_mainloop_unlock pulse.mainloop;
  if not !success then failwith_context pulse.context "Failed to set sink input volume"
;;

let set_sink_input_mute pulse index mute =
  Bindings.pa_threaded_mainloop_lock pulse.mainloop;
  let success = ref false in
  let op_callback = context_success_cb pulse success in
  let operation =
    Bindings.pa_context_set_sink_input_mute
      pulse.context
      (Unsigned.UInt32.of_int index)
      mute
      op_callback
      null
  in
  wait_for_operation pulse operation;
  Gc.keep_alive op_callback;
  Bindings.pa_threaded_mainloop_unlock pulse.mainloop;
  if not !success then failwith_context pulse.context "Failed to set sink input mute"
;;

let event_callback = ref (fun _ _ _ _ -> ())

let subscribe pulse callback =
  Bindings.pa_threaded_mainloop_lock pulse.mainloop;
  (event_callback := fun _ (f, e) idx _ -> callback f e (Unsigned.UInt32.to_int idx));
  Bindings.pa_context_set_subscribe_callback pulse.context !event_callback null;
  let success = ref false in
  let op_callback = context_success_cb pulse success in
  let operation =
    Bindings.pa_context_subscribe
      pulse.context
      `PA_SUBSCRIPTION_MASK_SINK_INPUT
      op_callback
      null
  in
  wait_for_operation pulse operation;
  Gc.keep_alive op_callback;
  Bindings.pa_threaded_mainloop_unlock pulse.mainloop;
  if not !success then failwith_context pulse.context "Failed to subscribe"
;;

let peak_detect_callback_table = Hashtbl.create (module Int)

let sink_input_peak_detect pulse index ?(rate = 32) ?(fragsize = 8) callback =
  let nsamples = fragsize / sizeof float in
  Bindings.pa_threaded_mainloop_lock pulse.mainloop;
  let stream =
    let sample_spec = make Bindings.Pa_sample_spec.t in
    setf
      sample_spec
      Bindings.Pa_sample_spec.format
      (if Sys.big_endian then `PA_SAMPLE_FLOAT32BE else `PA_SAMPLE_FLOAT32LE);
    setf sample_spec Bindings.Pa_sample_spec.rate (Unsigned.UInt32.of_int rate);
    setf sample_spec Bindings.Pa_sample_spec.channels (Unsigned.UInt8.of_int 1);
    Bindings.pa_stream_new
      pulse.context
      ("peak detect of #" ^ Int.to_string index)
      (addr sample_spec)
      (from_voidp Bindings.Pa_channel_map.t null)
  in
  let read_callback stream nbytes _ =
    let nbytes = allocate size_t nbytes
    and buff = allocate (ptr void) null in
    Bindings.pa_stream_peek stream buff nbytes |> check_error "Failed to peek stream";
    let nbytes = !@nbytes |> Unsigned.Size_t.to_int in
    if nbytes <> fragsize
    then
      raise
        (Pulse_error
           (Printf.sprintf
              "Received nbytes (%d) does not match fragsize(%d)"
              nbytes
              fragsize));
    let samples = CArray.from_ptr ((coerce (ptr void) (ptr float)) !@buff) nsamples in
    let peak =
      Float.(
        CArray.map float Float.abs samples
        |> CArray.fold_left add 0.0
        |> fun sum -> sum / of_int nsamples)
    in
    Bindings.pa_stream_drop stream |> check_error "Failed to drop stream";
    callback stream peak
  in
  let buf_attr = make Bindings.Pa_buffer_attr.t in
  setf buf_attr Bindings.Pa_buffer_attr.fragsize (Unsigned.UInt32.of_int fragsize);
  setf buf_attr Bindings.Pa_buffer_attr.maxlength (Unsigned.UInt32.of_int 1024);
  Bindings.pa_stream_set_read_callback stream read_callback null;
  let ret =
    Bindings.pa_stream_connect_record
      stream
      (Some (string_of_int index))
      (addr buf_attr)
      ([| `PA_STREAM_DONT_MOVE; `PA_STREAM_PEAK_DETECT; `PA_STREAM_ADJUST_LATENCY |]
       |> Array.map ~f:Bindings.pa_stream_flags_t_to_enum
       |> Array.reduce_exn ~f:Int.bit_or)
  in
  if ret <> 0
  then (
    Bindings.pa_stream_unref stream;
    Bindings.pa_threaded_mainloop_unlock pulse.mainloop;
    fail "Failed to connect stream" ret);
  Hashtbl.add_exn peak_detect_callback_table ~key:index ~data:(read_callback, stream);
  Bindings.pa_threaded_mainloop_unlock pulse.mainloop
;;

let sink_input_peak_detect_detach pulse index =
  match Hashtbl.find peak_detect_callback_table index with
  | Some (_, stream) ->
    let rec loop_try_to_detach n =
      Bindings.pa_threaded_mainloop_lock pulse.mainloop;
      match Bindings.pa_stream_get_state stream with
      (* If we attached to a sink_input which immediately got destroyed, the stream will hang in
         PA_STREAM_CREATING state and will fail after a timeout (30 seconds). Instead of waiting
         for timeout, we try to kill the source output. Unfortunately PulseAudio doesn't return
         the index via `pa_stream_get_index` for streams in PA_STREAM_CREATING/PA_STREAM_FAILED
         state, so we have to manually find id by matching name. :-( *)
      | `PA_STREAM_CREATING | `PA_STREAM_FAILED ->
        let source_output_index =
          let result = ref None in
          let op_callback _ info eol _ =
            if eol = 0
            then (
              let info = !@info in
              let info_index = getf info Bindings.Pa_source_output_info.index
              and info_name = getf info Bindings.Pa_source_output_info.name in
              if String.equal info_name ("peak detect of #" ^ Int.to_string index)
              then result := Some info_index);
            Bindings.pa_threaded_mainloop_signal pulse.mainloop 0
          in
          let operation =
            Bindings.pa_context_get_source_output_info_list pulse.context op_callback null
          in
          wait_for_operation pulse operation;
          Gc.keep_alive op_callback;
          !result
        in
        (match source_output_index with
         | Some source_output_index ->
           let _ = Bindings.pa_stream_disconnect stream in
           Bindings.pa_stream_unref stream;
           let success = ref false in
           let op_callback = context_success_cb pulse success in
           let operation =
             Bindings.pa_context_kill_source_output
               pulse.context
               source_output_index
               op_callback
               null
           in
           wait_for_operation pulse operation;
           Gc.keep_alive op_callback;
           Bindings.pa_threaded_mainloop_unlock pulse.mainloop;
           if not !success then failwith_context pulse.context "Error while detaching"
         | None ->
           Bindings.pa_threaded_mainloop_unlock pulse.mainloop;
           loop_try_to_detach (n + 1))
      | _ ->
        let ret = Bindings.pa_stream_disconnect stream in
        Bindings.pa_stream_unref stream;
        Bindings.pa_threaded_mainloop_unlock pulse.mainloop;
        if ret <> 0 then fail "Error while detaching" ret
    in
    loop_try_to_detach 0;
    Hashtbl.remove peak_detect_callback_table index
  | None -> raise (Pulse_error "Trying to detach non-existent Sink Input")
;;
