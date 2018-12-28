open Opium.Std

(* TODO: Crypto and don't use cookie *)

let get req ~key =
  Cookie.get req ~key

let set req ?(expiration = `Session) ~key ~data () =
  Cookie.set ~expiration req ~key ~data

(* let has_key req ~key =
  match Cookie.get req ~key with
  | None -> false
  | Some _ -> true *)

let delete res ~key =
  let expiration = `Max_age 0L in (* expired *)
  Cookie.set ~expiration res ~key ~data:""