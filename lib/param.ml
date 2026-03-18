type 'a t = { spec : Types.param; typ : 'a Types.typ }
type binding = { name : string; load : int; store : int -> unit }

let rec to_int : type a. a Types.typ -> a -> int =
 fun typ v ->
  match typ with
  | Uint8 -> v
  | Uint16 _ -> v
  | Uint32 _ -> UInt32.to_int v
  | Uint63 _ -> UInt63.to_int v
  | Uint64 _ -> Int64.unsigned_to_int v |> Option.value ~default:max_int
  | Bits _ -> v
  | Enum { base; _ } -> to_int base v
  | Where { inner; _ } -> to_int inner v
  | Single_elem { elem; _ } -> to_int elem v
  | Map { inner; encode; _ } -> to_int inner (encode v)
  | Apply { typ; _ } -> to_int typ v
  | Unit | All_bytes | All_zeros | Array _ | Byte_array _ | Byte_slice _
  | Casetype _ | Struct _ | Type_ref _ | Qualified_ref _ ->
      invalid_arg "Param: unsupported parameter type"

let rec of_int : type a. a Types.typ -> int -> a =
 fun typ v ->
  match typ with
  | Uint8 -> v
  | Uint16 _ -> v
  | Uint32 _ -> UInt32.of_int v
  | Uint63 _ -> UInt63.of_int v
  | Uint64 _ -> Int64.of_int v
  | Bits _ -> v
  | Enum { base; _ } -> of_int base v
  | Where { inner; _ } -> of_int inner v
  | Single_elem { elem; _ } -> of_int elem v
  | Map { inner; decode; _ } -> decode (of_int inner v)
  | Apply { typ; _ } -> of_int typ v
  | Unit | All_bytes | All_zeros | Array _ | Byte_array _ | Byte_slice _
  | Casetype _ | Struct _ | Type_ref _ | Qualified_ref _ ->
      invalid_arg "Param: unsupported parameter type"

let input name typ = { spec = Types.param name typ; typ }
let output name typ = { spec = Types.mutable_param name typ; typ }
let spec t = t.spec

let value t v =
  { name = t.spec.param_name; load = to_int t.typ v; store = (fun _ -> ()) }

let slot t r =
  {
    name = t.spec.param_name;
    load = to_int t.typ !r;
    store = (fun v -> r := of_int t.typ v);
  }

let name b = b.name
let load b = b.load
let store b = b.store
