type sink_input_info_t =
  { index : int
  ; name : string
  ; proplist : string Core.String.Map.t
  ; volume : float list
  ; mute : bool
  }

type t

exception Pulse_error of string

val connect : string -> t
val get_sink_input_by_index : t -> int -> sink_input_info_t option
val get_sink_input_list : t -> sink_input_info_t list
val set_sink_input_volume : t -> int -> volume:float -> channels:int -> unit
val set_sink_input_mute : t -> int -> bool -> unit

val subscribe
  :  t
  -> ([ `PA_SUBSCRIPTION_EVENT_SINK_INPUT ]
      -> [ `PA_SUBSCRIPTION_EVENT_CHANGE
         | `PA_SUBSCRIPTION_EVENT_NEW
         | `PA_SUBSCRIPTION_EVENT_REMOVE
         ]
      -> int
      -> unit)
  -> unit
