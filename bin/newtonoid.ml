(* ouvre la bibliotheque de modules definis dans lib/ *)
open Libnewtonoid

(* exemple d'ouverture d'un tel module de la bibliotheque : *)
open Iterator

module Box = struct
  let marge = 10.
  let infx = 0.
  let infy = 0.
  let supx = 800.
  let supy = 600.

  let graphic_format =
    Format.sprintf
      " %dx%d+50+50"
      (int_of_float ((2. *. marge) +. supx -. infx))
      (int_of_float ((2. *. marge) +. supy -. infy))
end

module Init = struct
  (* 16ms : 60 Hz *)
  (* À UTILISER AUSSI DANS LE CALCUL D'INTÉGRALES *)
  let dt = 0.016
end

let run_game etats_jeu =
  let ref_etats_jeu = ref etats_jeu in
  let ref_hdlr_alarm = ref Sys.(Signal_handle (fun _ -> ())) in
  let ref_hdlr_interruption = ref Sys.(Signal_handle (fun _ -> ())) in
  let handler_alrm _ =
    match Flux.uncons !ref_etats_jeu with
    | None ->
      Sys.(set_signal sigalrm !ref_hdlr_alarm);
      Sys.(set_signal sigint !ref_hdlr_interruption)
    | Some (etat, etats_jeu') ->
      Graphics.clear_graph ();
      (* PLACER LE CODE DE DESSIN (FONCTION D'etat) ICI : *)

      (* FIN CODE DESSIN *)
      Graphics.synchronize ();
      ref_etats_jeu := etats_jeu'
  in
  let handler_int _ = ref_etats_jeu := Flux.vide in
  Sys.(ref_hdlr_alarm := signal sigalrm (Signal_handle handler_alrm));
  Sys.(ref_hdlr_interruption := signal sigint (Signal_handle handler_int));
  ignore Unix.(setitimer ITIMER_REAL { it_interval = Init.dt; it_value = Init.dt });
  ignore (read_line ())

let etats_jeu = Tick (lazy (failwith "À DÉFINIR"))

let () =
  Graphics.open_graph Box.graphic_format;
  Graphics.auto_synchronize false;
  (* À DÉCOMMENTER UNE FOIS LE FLUX D'ÉTATS DÉFINI :*)
  (* run_game etats_jeu;  *)
  ignore (read_line ());
  Graphics.close_graph ()
