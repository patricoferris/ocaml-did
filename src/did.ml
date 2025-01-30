module J = struct
  include Yojson.Basic.Util

  let string s = `String s
  let pp = Yojson.Basic.pretty_print ~std:true
end

type t = { method' : string; identifier : string }

let method' t = t.method'
let identifier t = t.identifier
let to_uri t = Uri.of_string (Printf.sprintf "did:%s:%s" t.method' t.identifier)
let to_string t = "did:" ^ t.method' ^ ":" ^ t.identifier

(* Double check... *)
let path t =
  match String.split_on_char '/' t.identifier with
  | [] -> None
  | [ _id ] -> None
  | _id :: path -> Some ("/" ^ String.concat "/" path)

let query t = Uri.of_string t.identifier |> Uri.query
let fragment t = Uri.of_string t.identifier |> Uri.fragment

let service t =
  match query t |> List.assoc_opt "service" with
  | Some [ service ] -> Some service
  | _ -> None

let version_id t =
  match query t |> List.assoc_opt "versionId" with
  | Some [ id ] -> Some id
  | _ -> None

let hashlink t =
  match query t |> List.assoc_opt "hl" with Some [ hl ] -> Some hl | _ -> None

let relative_ref t =
  match query t |> List.assoc_opt "relativeRef" with
  | Some [ ref ] -> Some ref
  | _ -> None

let version_time t =
  match query t |> List.assoc_opt "versionTime" with
  | Some [ time ] -> Some time
  | _ -> None

let of_string_exn s =
  let uri = Uri.of_string s in
  match Uri.scheme uri with
  | Some "did" -> (
      match String.split_on_char ':' s with
      | [ _scheme; method'; identifier ] -> { method'; identifier }
      | _ ->
          invalid_arg
            (Printf.sprintf
               "did path malformed, expectex <method>:<identifier> but got %s"
               (Uri.path uri)))
  | Some scheme ->
      invalid_arg (Printf.sprintf "Expected scheme `did` but got `%s`" scheme)
  | None -> invalid_arg "Valid dids must have a scheme of `did`"

module Document = struct
  type did = t

  type t = {
    context : Uri.t list;
    id : did;
    also_known_as : string list;
    controller : string list;
    verification_method : verification_method list;
    authentication :
      [ `Verification of verification_method | `Did of did | `Relative of Uri.t ]
      list;
    assertion_method :
      [ `Verification of verification_method | `Did of did | `Relative of Uri.t ]
      list;
    key_agreement :
      [ `Verification of verification_method | `Did of did | `Relative of Uri.t ]
      list;
    capability_invocation :
      [ `Verification of verification_method | `Did of did | `Relative of Uri.t ]
      list;
    capability_delegation :
      [ `Verification of verification_method | `Did of did | `Relative of Uri.t ]
      list;
    service : service list;
  }

  and verification_method = {
    vm_id : did;
    vm_controller : did;
    vm_type : string;
    vm_material : verification_material;
  }

  and verification_material =
    | Public_key_jwk of Jose.Jwk.public Jose.Jwk.t
    | Public_key_multibase of string
    | Other of (string * Yojson.Basic.t) list

  and service = {
    service_id : did;
    service_type : string list;
    service_endpoint :
      [ `Uri of Uri.t | `Map of (string * Yojson.Basic.t) list ] list;
  }

  let w3_did_context = Uri.of_string "https://www.w3.org/ns/did/v1"

  let make ?(context = [ w3_did_context ]) ?(also_known_as = [])
      ?(controller = []) ?(verification_method = []) ?(authentication = [])
      ?(assertion_method = []) ?(key_agreement = [])
      ?(capability_invocation = []) ?(capability_delegation = [])
      ?(service = []) id =
    {
      context;
      id;
      also_known_as;
      controller;
      verification_method;
      authentication;
      assertion_method;
      key_agreement;
      capability_invocation;
      capability_delegation;
      service;
    }

  let id t = t.id
  let aka t = t.also_known_as

  let vm_material_of_json v =
    match Yojson.Basic.Util.member "publicKeyJwk" v with
    | `Assoc _ as map ->
        let conv = Yojson.Basic.to_string map |> Yojson.Safe.from_string in
        let jwk = Jose.Jwk.of_pub_json conv |> Result.get_ok in
        Public_key_jwk jwk
    | `Null -> (
        match Yojson.Basic.Util.member "publicKeyMultibase" v with
        | `String s -> Public_key_multibase s
        | `Null ->
            let filter_keys = [ "id"; "type"; "controller" ] in
            let assoc = Yojson.Basic.Util.to_assoc v in
            Other
              (List.filter (fun (k, _) -> not (List.mem k filter_keys)) assoc)
        | _ -> invalid_arg "publicKeyMultibase should be a string")
    | _ -> invalid_arg "publicKeyJwk should be a map"

  let vm_material_to_json = function
    | Public_key_jwk jwk ->
        [ ("publicKeyJwk", Jose.Jwk.to_pub_json jwk |> Yojson.Safe.to_basic) ]
    | Public_key_multibase m -> [ ("publicKeyMultibase", `String m) ]
    | Other assoc -> assoc

  let string_or_set_to_json id = function
    | [] -> []
    | [ a ] -> [ (id, `String a) ]
    | set -> [ (id, `List (List.map (fun s -> `String s) set)) ]

  let string_or_set_of_json = function
    | `String s -> [ s ]
    | `List ss ->
        List.map
          (function
            | `String s -> s | _ -> invalid_arg "Expected a list of strings")
          ss
    | _ -> invalid_arg "Cannot convert non string or list to set"

  let null_opt = function `Null -> None | v -> Some v

  let members ks a =
    List.map (fun k -> Yojson.Basic.Util.member k a |> null_opt) ks

  let verification_method_of_json s =
    match members [ "id"; "type"; "controller" ] s with
    | [
     Some (`String vm_id); Some (`String vm_type); Some (`String vm_controller);
    ] ->
        let vm_id = of_string_exn vm_id in
        let vm_controller = of_string_exn vm_controller in
        let vm_material = vm_material_of_json s in
        { vm_id; vm_type; vm_controller; vm_material }
    | _ -> invalid_arg "Verification method missing id, type and/or controller."

  let verification_method_to_json vm =
    let vm_material = vm_material_to_json vm.vm_material in
    `Assoc
      ([
         ("id", `String (to_string vm.vm_id));
         ("type", `String vm.vm_type);
         ("controller", `String (to_string vm.vm_controller));
       ]
      @ vm_material)

  let vm_or_did_to_json = function
    | `Verification vm -> verification_method_to_json vm
    | `Did did -> `String (to_string did)
    | `Relative uri -> `String (Uri.to_string uri)

  let vm_or_did_of_json = function
    | `String did -> (
        try `Did (of_string_exn did)
        with Invalid_argument _ -> `Relative (Uri.of_string did))
    | `Assoc _ as map -> `Verification (verification_method_of_json map)
    | v ->
        Fmt.invalid_arg
          "Expected a string or object for DID or verification method, got %a"
          J.pp v

  let service_endpoint_to_json s : Yojson.Basic.t =
    let endpoint =
      match s.service_endpoint with
      | [] ->
          invalid_arg
            "Service endpoint must be a string, a map or a set of such things"
      | [ `Uri uri ] -> `String (Uri.to_string uri)
      | endpoints ->
          `List
            (List.map
               (function
                 | `Uri uri -> `String (Uri.to_string uri)
                 | `Map assoc -> `Assoc assoc)
               endpoints)
    in
    `Assoc
      ([ ("id", `String (to_string s.service_id)) ]
      @ string_or_set_to_json "type" s.service_type
      @ [ ("serviceEndpoint", endpoint) ])

  let service_endpoint_of_json v =
    match members [ "id"; "type"; "serviceEndpoint" ] v with
    | [ Some (`String service_id); Some type'; Some service_endpoint ] ->
        let service_id = of_string_exn service_id in
        let service_type = string_or_set_of_json type' in
        let service_endpoint =
          match service_endpoint with
          | `String s -> [ `Uri (Uri.of_string s) ]
          | `List ss ->
              List.map
                (function
                  | `String s -> `Uri (Uri.of_string s)
                  | `Assoc assoc -> `Map assoc
                  | _ -> invalid_arg "Expected a URI or Map")
                ss
          | _ -> invalid_arg "service endpoint: expected a string or list"
        in
        { service_id; service_type; service_endpoint }
    | _ -> invalid_arg "service endpoint: failed to parse"

  let to_json document =
    let vms =
      List.map verification_method_to_json document.verification_method
    in
    let services = List.map service_endpoint_to_json document.service in
    let authentication = List.map vm_or_did_to_json document.authentication in
    let assertion_method =
      List.map vm_or_did_to_json document.assertion_method
    in
    let key_agreement = List.map vm_or_did_to_json document.key_agreement in
    let capability_invocation =
      List.map vm_or_did_to_json document.capability_invocation
    in
    let capability_delegation =
      List.map vm_or_did_to_json document.capability_delegation
    in
    let list_or_null id v = (id, if v = [] then `Null else `List v) in
    `Assoc
      ([
         ( "@context",
           if document.context = [] then `String (Uri.to_string w3_did_context)
           else
             `List
               (List.map (fun v -> `String (Uri.to_string v)) document.context)
         );
         ("id", `String (to_string document.id));
         ( "alsoKnownAs",
           if document.also_known_as = [] then `Null
           else `List (List.map J.string document.also_known_as) );
         ( "controller",
           if document.also_known_as = [] then `Null
           else `List (List.map J.string document.controller) );
         list_or_null "authentication" authentication;
         list_or_null "assertionMethod" assertion_method;
         list_or_null "keyAgreement" key_agreement;
         list_or_null "capabilityInvocation" capability_invocation;
         list_or_null "capabilityDelegation" capability_delegation;
         list_or_null "verificationMethod" vms;
         list_or_null "service" services;
       ]
      |> List.filter (function _, `Null -> false | _ -> true))

  let of_json_exn json =
    match
      members
        [
          "@context";
          "id";
          "alsoKnownAs";
          "controller";
          "authentication";
          "assertionMethod";
          "keyAgreement";
          "capabilityInvocation";
          "capabilityDelegration";
          "verificationMethod";
          "service";
        ]
        json
    with
    | [
     context;
     Some (`String id);
     aka;
     controller;
     auth;
     assertion;
     key;
     cap_inv;
     cap_del;
     vm;
     service;
    ] ->
        let context =
          match context with
          | None -> invalid_arg "Expected an @context field"
          | Some (`String s) -> [ Uri.of_string s ]
          | Some (`List ls) ->
              List.map (fun v -> J.to_string v |> Uri.of_string) ls
          | _ -> invalid_arg "Malformed @context field"
        in
        let id = of_string_exn id in
        let also_known_as =
          match aka with
          | None -> []
          | Some (`List ls) -> List.map J.to_string ls
          | _ -> invalid_arg "Malformed alsoKnownAs field"
        in
        let controller =
          match controller with
          | None -> []
          | Some (`List ls) -> List.map J.to_string ls
          | _ -> invalid_arg "Malformed controller field"
        in
        let list_map_opt id f v =
          match v with
          | None -> []
          | Some (`List ls) -> List.map f ls
          | Some json ->
              Fmt.invalid_arg "Malformed %s field, got %a" id J.pp json
        in
        let authentication =
          list_map_opt "authentication" vm_or_did_of_json auth
        in
        let assertion_method =
          list_map_opt "assertionMethod" vm_or_did_of_json assertion
        in
        let key_agreement = list_map_opt "keyAgreement" vm_or_did_of_json key in
        let capability_invocation =
          list_map_opt "capabilityInvocation" vm_or_did_of_json cap_inv
        in
        let capability_delegation =
          list_map_opt "capabilityDelegation" vm_or_did_of_json cap_del
        in
        let verification_method =
          list_map_opt "verificationMethod" verification_method_of_json vm
        in
        let service = list_map_opt "service" service_endpoint_of_json service in
        make ~context ~also_known_as ~controller ~authentication
          ~assertion_method ~key_agreement ~capability_invocation
          ~capability_delegation ~verification_method ~service id
    | _ -> invalid_arg "Malformed DID Document"
end

module Method = struct
  (* Static cryptographic keys *)
  module Key = struct end
  module Jwk = struct end
  module Web = struct end
end
