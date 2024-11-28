open Base
open Ctypes
open Foreign

type pa_threaded_mainloop = unit ptr
type pa_mainloop_api = unit ptr
type pa_context = unit ptr

let pa_threaded_mainloop : pa_threaded_mainloop typ = ptr void
let pa_mainloop_api : pa_mainloop_api typ = ptr void
let pa_context : pa_context typ = ptr void

let pa_threaded_mainloop_new =
  foreign "pa_threaded_mainloop_new" (void @-> returning pa_threaded_mainloop)
;;

let pa_threaded_mainloop_start =
  foreign
    ~release_runtime_lock:true
    "pa_threaded_mainloop_start"
    (pa_threaded_mainloop @-> returning int)
;;

let pa_threaded_mainloop_lock =
  foreign
    ~release_runtime_lock:true
    "pa_threaded_mainloop_lock"
    (pa_threaded_mainloop @-> returning void)
;;

let pa_threaded_mainloop_unlock =
  foreign
    ~release_runtime_lock:true
    "pa_threaded_mainloop_unlock"
    (pa_threaded_mainloop @-> returning void)
;;

let pa_threaded_mainloop_wait =
  foreign
    ~release_runtime_lock:true
    "pa_threaded_mainloop_wait"
    (pa_threaded_mainloop @-> returning void)
;;

let pa_threaded_mainloop_signal =
  foreign
    ~release_runtime_lock:true
    "pa_threaded_mainloop_signal"
    (pa_threaded_mainloop @-> int @-> returning void)
;;

let pa_threaded_mainloop_accept =
  foreign
    ~release_runtime_lock:true
    "pa_threaded_mainloop_accept"
    (pa_threaded_mainloop @-> returning void)
;;

let pa_threaded_mainloop_get_api =
  foreign
    "pa_threaded_mainloop_get_api"
    (pa_threaded_mainloop @-> returning pa_mainloop_api)
;;

let pa_context_new =
  foreign "pa_context_new" (pa_mainloop_api @-> string @-> returning pa_context)
;;

let pa_context_connect =
  foreign
    "pa_context_connect"
    (pa_context @-> ptr void @-> int @-> ptr void @-> returning int)
;;

let pa_context_errno = foreign "pa_context_errno" (pa_context @-> returning int)
let pa_strerror = foreign "pa_strerror" (int @-> returning string)
let pa_context_notify_cb_t = pa_context @-> ptr void @-> returning void

type pa_context_state_t =
  [ `PA_CONTEXT_UNCONNECTED
  | `PA_CONTEXT_CONNECTING
  | `PA_CONTEXT_AUTHORIZING
  | `PA_CONTEXT_SETTING_NAME
  | `PA_CONTEXT_READY
  | `PA_CONTEXT_FAILED
  | `PA_CONTEXT_TERMINATED
  ]
[@@deriving enum, show { with_path = false }]

let t_of_enum_exn f value = Option.value_exn (f value)

let pa_context_state_t =
  Ctypes.view
    ~read:(t_of_enum_exn pa_context_state_t_of_enum)
    ~write:pa_context_state_t_to_enum
    Ctypes.int
;;

let pa_context_get_state =
  foreign "pa_context_get_state" (pa_context @-> returning pa_context_state_t)
;;

type pa_sample_format_t =
  [ `PA_SAMPLE_INVALID [@value -1]
  | `PA_SAMPLE_U8
  | `PA_SAMPLE_ALAW
  | `PA_SAMPLE_ULAW
  | `PA_SAMPLE_S16LE
  | `PA_SAMPLE_S16BE
  | `PA_SAMPLE_FLOAT32LE
  | `PA_SAMPLE_FLOAT32BE
  | `PA_SAMPLE_S32LE
  | `PA_SAMPLE_S32BE
  | `PA_SAMPLE_S24LE
  | `PA_SAMPLE_S24BE
  | `PA_SAMPLE_S24_32LE
  | `PA_SAMPLE_S24_32BE
  | `PA_SAMPLE_MAX
  ]
[@@deriving enum, show { with_path = false }]

let pa_sample_format_t =
  Ctypes.view
    ~read:(t_of_enum_exn pa_sample_format_t_of_enum)
    ~write:pa_sample_format_t_to_enum
    Ctypes.int
;;

type pa_channel_position_t =
  [ `PA_CHANNEL_POSITION_INVALID [@value -1]
  | `PA_CHANNEL_POSITION_MONO
  | `PA_CHANNEL_POSITION_FRONT_LEFT
  | `PA_CHANNEL_POSITION_FRONT_RIGHT
  | `PA_CHANNEL_POSITION_FRONT_CENTER
  | `PA_CHANNEL_POSITION_REAR_CENTER
  | `PA_CHANNEL_POSITION_REAR_LEFT
  | `PA_CHANNEL_POSITION_REAR_RIGHT
  | `PA_CHANNEL_POSITION_LFE
  | `PA_CHANNEL_POSITION_FRONT_LEFT_OF_CENTER
  | `PA_CHANNEL_POSITION_FRONT_RIGHT_OF_CENTER
  | `PA_CHANNEL_POSITION_SIDE_LEFT
  | `PA_CHANNEL_POSITION_SIDE_RIGHT
  | `PA_CHANNEL_POSITION_AUX0
  | `PA_CHANNEL_POSITION_AUX1
  | `PA_CHANNEL_POSITION_AUX2
  | `PA_CHANNEL_POSITION_AUX3
  | `PA_CHANNEL_POSITION_AUX4
  | `PA_CHANNEL_POSITION_AUX5
  | `PA_CHANNEL_POSITION_AUX6
  | `PA_CHANNEL_POSITION_AUX7
  | `PA_CHANNEL_POSITION_AUX8
  | `PA_CHANNEL_POSITION_AUX9
  | `PA_CHANNEL_POSITION_AUX10
  | `PA_CHANNEL_POSITION_AUX11
  | `PA_CHANNEL_POSITION_AUX12
  | `PA_CHANNEL_POSITION_AUX13
  | `PA_CHANNEL_POSITION_AUX14
  | `PA_CHANNEL_POSITION_AUX15
  | `PA_CHANNEL_POSITION_AUX16
  | `PA_CHANNEL_POSITION_AUX17
  | `PA_CHANNEL_POSITION_AUX18
  | `PA_CHANNEL_POSITION_AUX19
  | `PA_CHANNEL_POSITION_AUX20
  | `PA_CHANNEL_POSITION_AUX21
  | `PA_CHANNEL_POSITION_AUX22
  | `PA_CHANNEL_POSITION_AUX23
  | `PA_CHANNEL_POSITION_AUX24
  | `PA_CHANNEL_POSITION_AUX25
  | `PA_CHANNEL_POSITION_AUX26
  | `PA_CHANNEL_POSITION_AUX27
  | `PA_CHANNEL_POSITION_AUX28
  | `PA_CHANNEL_POSITION_AUX29
  | `PA_CHANNEL_POSITION_AUX30
  | `PA_CHANNEL_POSITION_AUX31
  | `PA_CHANNEL_POSITION_TOP_CENTER
  | `PA_CHANNEL_POSITION_TOP_FRONT_LEFT
  | `PA_CHANNEL_POSITION_TOP_FRONT_RIGHT
  | `PA_CHANNEL_POSITION_TOP_FRONT_CENTER
  | `PA_CHANNEL_POSITION_TOP_REAR_LEFT
  | `PA_CHANNEL_POSITION_TOP_REAR_RIGHT
  | `PA_CHANNEL_POSITION_TOP_REAR_CENTER
  | `PA_CHANNEL_POSITION_MAX
  ]
[@@deriving enum, show { with_path = false }]

let pa_channel_position_t =
  Ctypes.view
    ~read:(t_of_enum_exn pa_channel_position_t_of_enum)
    ~write:pa_channel_position_t_to_enum
    Ctypes.int
;;

let pa_channels_max = 32

module Pa_channel_map = struct
  type t

  let t : t structure typ = structure "pa_channel_map"
  let map_channels = field t "channels" uint8_t
  let map = field t "map" (array pa_channels_max pa_channel_position_t)
  let () = seal t
end

module Pa_sample_spec = struct
  type t

  let t : t structure typ = structure "pa_sample_spec"
  let format = field t "format" pa_sample_format_t
  let rate = field t "rate" uint32_t
  let channels = field t "channels" uint8_t
  let () = seal t
end

let pa_volume_t = uint32_t

module Pa_cvolume = struct
  type t

  let t : t structure typ = structure "pa_cvolume"
  let channels = field t "channels" uint8_t
  let values = field t "values" (array pa_channels_max pa_volume_t)
  let () = seal t
end

let pa_cvolume_init =
  foreign "pa_cvolume_init" (ptr Pa_cvolume.t @-> returning (ptr Pa_cvolume.t))
;;

let pa_cvolume_set =
  foreign
    "pa_cvolume_set"
    (ptr Pa_cvolume.t @-> uint @-> pa_volume_t @-> returning (ptr Pa_cvolume.t))
;;

let pa_usec_t = uint64_t

type pa_proplist_t = unit ptr

let pa_proplist_t : pa_proplist_t typ = ptr void

let pa_proplist_iterate =
  foreign "pa_proplist_iterate" (pa_proplist_t @-> ptr (ptr void) @-> returning string_opt)
;;

let pa_proplist_gets =
  foreign "pa_proplist_gets" (pa_proplist_t @-> string @-> returning string_opt)
;;

module Pa_sink_input_info = struct
  type t

  let t : t structure typ = structure "pa_sink_input_info"
  let index = field t "index" uint32_t
  let name = field t "name" string
  let owner_module = field t "owner_module" uint32_t
  let client = field t "client" uint32_t
  let sink = field t "sink" uint32_t
  let sample_spec = field t "sample_spec" Pa_sample_spec.t
  let channel_map = field t "channel_map" Pa_channel_map.t
  let volume = field t "volume" Pa_cvolume.t
  let buffer_usec = field t "buffer_usec" pa_usec_t
  let sink_usec = field t "sink_usec" pa_usec_t
  let resample_method = field t "resample_method" string
  let driver = field t "driver" string
  let mute = field t "mute" bool
  let proplist = field t "proplist" pa_proplist_t
  let corked = field t "corked" int
  let has_volume = field t "has_volume" int
  let volume_writable = field t "volume_writable" int
  let format = field t "format" (ptr void)
  let () = seal t
end

let pa_sink_input_info_cb_t =
  pa_context @-> ptr Pa_sink_input_info.t @-> int @-> ptr void @-> returning void
;;

type pa_operation = unit ptr
type pa_operation_opt = unit ptr

let pa_operation : pa_operation typ = ptr void
let pa_operation_opt : pa_operation_opt option typ = ptr_opt void
let pa_operation_unref = foreign "pa_operation_unref" (pa_operation @-> returning void)

type pa_operation_state_t =
  [ `PA_OPERATION_RUNNING
  | `PA_OPERATION_DONE
  | `PA_OPERATION_CANCELLED
  ]
[@@deriving enum, show { with_path = false }]

let pa_operation_state_t =
  Ctypes.view
    ~read:(t_of_enum_exn pa_operation_state_t_of_enum)
    ~write:pa_operation_state_t_to_enum
    Ctypes.int
;;

let pa_operation_get_state =
  foreign "pa_operation_get_state" (pa_operation @-> returning pa_operation_state_t)
;;

let pa_context_get_sink_input_info =
  foreign
    "pa_context_get_sink_input_info"
    (pa_context
     @-> uint32_t
     @-> funptr ~thread_registration:true ~runtime_lock:true pa_sink_input_info_cb_t
     @-> ptr void
     @-> returning pa_operation_opt)
;;

let pa_context_get_sink_input_info_list =
  foreign
    "pa_context_get_sink_input_info_list"
    (pa_context
     @-> funptr ~thread_registration:true ~runtime_lock:true pa_sink_input_info_cb_t
     @-> ptr void
     @-> returning pa_operation_opt)
;;

let pa_context_success_cb_t = pa_context @-> int @-> ptr void @-> returning void

type pa_subscription_mask_t = [ `PA_SUBSCRIPTION_MASK_SINK_INPUT [@value 0x0004] ]
[@@deriving enum, show { with_path = false }]

let pa_subscription_mask_t =
  Ctypes.view
    ~read:(t_of_enum_exn pa_subscription_mask_t_of_enum)
    ~write:pa_subscription_mask_t_to_enum
    Ctypes.int
;;

module Pa_subscription_event_type = struct
  type facility_t = [ `PA_SUBSCRIPTION_EVENT_SINK_INPUT [@value 0x0002] ]
  [@@deriving enum, show { with_path = false }]

  type event_type_t =
    [ `PA_SUBSCRIPTION_EVENT_NEW [@value 0x0000]
    | `PA_SUBSCRIPTION_EVENT_CHANGE [@value 0x0010]
    | `PA_SUBSCRIPTION_EVENT_REMOVE [@value 0x0020]
    ]
  [@@deriving enum, show { with_path = false }]

  let t =
    Ctypes.view
      ~read:(fun value ->
        ( t_of_enum_exn facility_t_of_enum @@ (0x000F land value)
        , t_of_enum_exn event_type_t_of_enum @@ (0x0030 land value) ))
      ~write:(fun (facility, event_type) ->
        facility_t_to_enum facility lor event_type_t_to_enum event_type)
      Ctypes.int
  ;;
end

let pa_context_subscribe_cb_t =
  pa_context @-> Pa_subscription_event_type.t @-> uint32_t @-> ptr void @-> returning void
;;

let pa_context_set_subscribe_callback =
  foreign
    "pa_context_set_subscribe_callback"
    (pa_context
     @-> funptr ~thread_registration:true ~runtime_lock:true pa_context_subscribe_cb_t
     @-> ptr void
     @-> returning void)
;;

let pa_context_subscribe =
  foreign
    "pa_context_subscribe"
    (pa_context
     @-> pa_subscription_mask_t
     @-> funptr ~thread_registration:true ~runtime_lock:true pa_context_success_cb_t
     @-> ptr void
     @-> returning pa_operation_opt)
;;

let pa_context_set_sink_input_mute =
  foreign
    "pa_context_set_sink_input_mute"
    (pa_context
     @-> uint32_t
     @-> bool
     @-> funptr ~thread_registration:true ~runtime_lock:true pa_context_success_cb_t
     @-> ptr void
     @-> returning pa_operation_opt)
;;

let pa_context_set_sink_input_volume =
  foreign
    "pa_context_set_sink_input_volume"
    (pa_context
     @-> uint32_t
     @-> ptr Pa_cvolume.t
     @-> funptr ~thread_registration:true ~runtime_lock:true pa_context_success_cb_t
     @-> ptr void
     @-> returning pa_operation_opt)
;;
