open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* Tipos e Definições *)
type estado = int [@@deriving yojson]
type simbolo = char option [@@deriving yojson]
type transicao = (estado * simbolo * simbolo) * (estado * simbolo) [@@deriving yojson]
type pda = estado list * char list * char list * transicao list * estado list * estado list [@@deriving yojson]
type fita = char option list [@@deriving yojson]
type pdaw = pda * fita [@@deriving yojson]

type producao = char * string list [@@deriving yojson]
type cfg = producao list [@@deriving yojson]
type cfgw = cfg * fita [@@deriving yojson]


(* Funções Auxiliares *)
let rec read_multiplelines () =
  try
    let line = read_line () in
    line ^ " " ^ read_multiplelines ()
  with End_of_file -> ""

(* Identifica o conjunto finito de símbolos terminais *)
let terminais cfg =
  let variaveis = List.map fst cfg in
  let todos_simbolos =
    List.flatten (List.map (fun (_, regras) -> List.concat (List.map (fun r -> List.init (String.length r) (String.get r)) regras)) cfg)
  in
  List.filter (fun s -> not (List.mem s variaveis)) todos_simbolos |> List.sort_uniq Char.compare

(* Função 1: Simulação do PDA para Aceitação de Palavra *)
let simular_pda (pda, fita) =
  let (_, _, _, transicoes, estado_inicial, estados_finais) = pda in
  
  let estados_pendentes = Queue.create () in
  let transicoes_processadas = Hashtbl.create 1000 in
  
  Queue.add (estado_inicial, [], fita, 0) estados_pendentes;

  let limite_profundidade = 
    (List.length fita * 10) + (List.length transicoes * 10)
  in
  
  let profundidade_max_global = 
    limite_profundidade * 2  (* Define o teto absoluto como o dobro do limite de profundidade *)
  in
  

  let rec iterar () =
    if Queue.is_empty estados_pendentes then
      false (* Rejeita se não houver mais estados para processar *)
    else
      let (estado, pilha, fita, profundidade) = Queue.pop estados_pendentes in

      (* Verificar se ultrapassou o limite de profundidade *)
      if profundidade > limite_profundidade || profundidade > profundidade_max_global then (
        Printf.printf "Limite de profundidade atingido (%d). Rejeitado.\n" limite_profundidade;
          false
      ) else
        let estado_chave = (estado, pilha, fita, profundidade)in

        if Hashtbl.mem transicoes_processadas estado_chave then
          iterar ()
        else (
          Hashtbl.add transicoes_processadas estado_chave true;

          Printf.printf "Profundidade: %d\n" profundidade;
          Printf.printf "Pilha: [%s]\n" (String.concat ", " (List.map (String.make 1) pilha));
          Printf.printf "Palavra: [%s]\n" (String.concat "" (List.map (String.make 1) fita));
          Printf.printf "Tamanho da FITA: %d\n" (List.length fita);

          (* Aceitação: Verificar estado final *)
          if List.mem estado estados_finais && fita = [] then
            true
          else (
            List.iter (fun ((de, simbolo_fita, simbolo_pilha), (para, simbolo_pilha_nova)) ->
              if de = estado then (
                match simbolo_fita, simbolo_pilha with
                | Some f, None when fita <> [] && List.hd fita = f ->
                    let nova_fita = List.tl fita in
                    Printf.printf "Consumindo símbolo da fita: %s, sem verificar pilha.\n" (String.make 1 f);
                    Queue.add (para, pilha, nova_fita, profundidade + 1) estados_pendentes;

                | Some f, Some p when fita <> [] && pilha <> [] && List.hd fita = f && List.hd pilha = p ->
                    let nova_pilha =
                      match simbolo_pilha_nova with
                      | None -> List.tl pilha (* Remove o topo da pilha *)
                      | Some s -> s :: List.tl pilha (* Adiciona o novo símbolo ao topo *)
                    in
                    let nova_fita = List.tl fita in
                    Printf.printf "Consumindo símbolo da fita: %s, atualizando pilha.\n" (String.make 1 f);
                    Printf.printf "Estado atualizado: Pilha -> [%s], Palavra -> [%s]\n"
                      (String.concat ", " (List.map (String.make 1) nova_pilha))
                      (String.concat "" (List.map (String.make 1) nova_fita));
                    Queue.add (para, nova_pilha, nova_fita, profundidade + 1) estados_pendentes;

                | None, Some p when pilha <> [] && List.hd pilha = p ->
                    let nova_pilha =
                      match simbolo_pilha_nova with
                      | None -> List.tl pilha (* Remove o topo da pilha *)
                      | Some s when s = '_' -> List.tl pilha (* Remove o topo da pilha sem adicionar nada *)
                      | Some s -> s :: List.tl pilha (* Adiciona o novo símbolo ao topo *)
                    in
                    Printf.printf "Transição epsilon (sem consumo de fita), atualizando pilha.\n";
                    Printf.printf "Estado atualizado: Pilha -> [%s], Palavra -> [%s]\n"
                      (String.concat ", " (List.map (String.make 1) nova_pilha))
                      (String.concat "" (List.map (String.make 1) fita));
                    Queue.add (para, nova_pilha, fita, profundidade + 1) estados_pendentes;

                | None, None ->
                    let nova_pilha =
                      match simbolo_pilha_nova with
                      | None -> pilha
                      | Some s -> s :: pilha
                    in
                    Printf.printf "Transição epsilon (sem consumo de fita ou pilha), atualizando estado.\n";
                    Printf.printf "Estado atualizado: Pilha -> [%s], Palavra -> [%s]\n"
                      (String.concat ", " (List.map (String.make 1) nova_pilha))
                      (String.concat "" (List.map (String.make 1) fita));
                    Queue.add (para, nova_pilha, fita, profundidade + 1) estados_pendentes;

                | _ -> Printf.printf "Transição ignorada: condições não satisfeitas.\n"
              )
            ) transicoes;
            iterar ()
          )
        )
  in
  iterar ()
;;






(* Função 2: Conversão de CFG para PDA *)
let cfg_para_pda (producoes, fita) =

  (* Estados do PDA *)
  let estado_inicial = 1 in
  let estado_final = 0 in
  let estado_intermediario = 2 in

  let estados = ref [estado_inicial; estado_intermediario; estado_final] in

  (* Alfabeto de entrada (terminais) *)
  let terminais = terminais producoes in

  (* Alfabeto da pilha (terminais + variáveis) *)
  let pilha_alfabeto = List.map fst producoes @ terminais in

  (* Transições do PDA *)
  let transicoes = ref [] in

  (* Transição inicial: empilhar o símbolo inicial da CFG com o marcador $ *)
  let simbolo_inicial = fst (List.hd producoes) in
  transicoes := ((estado_inicial, None, None), (estado_intermediario, Some '$')) :: !transicoes;
  transicoes := ((estado_intermediario, None, None), (estado_intermediario + 1, Some simbolo_inicial)) :: !transicoes;
  estados := (estado_intermediario + 1) :: !estados;

  let proximo_estado = ref (estado_intermediario + 2) in

  (* Transições para cada produção da CFG *)
  List.iter (fun (variavel, regras) ->
  List.iter (fun regra ->
    (* Para cada regra A -> x1x2...xn, adiciona transições que substituem A por x1x2...xn *)
    let regra_simbolos = List.init (String.length regra) (String.get regra) in

    let rec empilhar_regra regra_restante estado_atual =
      match regra_restante with
      | [] -> ()
      | [x] ->
        (* Último símbolo da regra vai para o estado 3 *)
        if estado_atual = 3 then
          transicoes :=
            ((estado_atual, None, Some variavel), (3, Some x)) :: !transicoes
        else
          transicoes :=
            ((estado_atual, None, None), (3, Some x)) :: !transicoes
      | x :: xs ->
        (* Se estamos no estado 3, consumimos o símbolo com a variável *)
        if estado_atual = 3 then
          let novo_estado = !proximo_estado in
          proximo_estado := !proximo_estado + 1;
          estados := novo_estado :: !estados;
          transicoes :=
            ((estado_atual, None, Some variavel), (novo_estado, Some x)) :: !transicoes;
          empilhar_regra xs novo_estado
        else
          (* Estados intermediários usam transições ε *)
          let novo_estado = !proximo_estado in
          proximo_estado := !proximo_estado + 1;
          estados := novo_estado :: !estados;
          transicoes :=
            ((estado_atual, None, None), (novo_estado, Some x)) :: !transicoes;
          empilhar_regra xs novo_estado
    in

    (* Processa os símbolos da regra de trás para frente *)
    empilhar_regra (List.rev regra_simbolos) (estado_intermediario + 1)
  ) regras
) producoes;



  (* Transições de consumo de caracteres (terminais) *)
(* Transições de consumo de caracteres (terminais) *)
List.iter (fun c ->
  if c <> '_' then (* Ignorar epsilon como terminal *)
    transicoes :=
      ((estado_intermediario + 1, Some c, Some c), (estado_intermediario + 1, None)) :: !transicoes
) terminais;


  (* Transição para o estado final quando a pilha contém apenas $ *)
  transicoes := ((estado_intermediario + 1, None, Some '$'), (estado_final, None)) :: !transicoes;

  (* Construir e retornar o PDA *)
  let transicoes_list = !transicoes in
  let estados_list = List.rev !estados in

  (* Imprime o PDA para depuração *)
  print_endline "Transições Geradas:";
  List.iter (fun (((de, simbolo_fita, simbolo_pilha), (para, simbolo_nova_pilha))) ->
    Printf.printf "  ((%d, %s, %s) -> (%d, %s))\n"
      de
      (match simbolo_fita with None -> "ε" | Some c -> String.make 1 c)
      (match simbolo_pilha with None -> "ε" | Some c -> String.make 1 c)
      para
      (match simbolo_nova_pilha with None -> "ε" | Some c -> String.make 1 c)
  ) !transicoes;
  

  (* Retornar o PDA e a fita *)
  ((estados_list, terminais, pilha_alfabeto, transicoes_list, estado_inicial, [estado_final]), fita)





  



(* Função Principal *)
let () =
  let s = read_multiplelines () in
  try
    let json = Yojson.Safe.from_string s in
    let re_cfgw = Str.regexp {|^ *\[ *\[ *\[ *"[A-Za-z]"|} in
    let re_pdaw = Str.regexp {|^ *\[ *\[ *\[ *[0-9]|} in
    if Str.string_match re_cfgw s 0 then (
      (* Processar como CFG *)
      let cfgw = json |> cfgw_of_yojson in
      let (cfg, palavra) = cfgw in
      (* Converter fita para char list *)
      let palavra_convertida = List.filter_map (function None -> None | Some c -> Some c) palavra in
      (* Converte CFG para PDA *)
      let pda = cfg_para_pda (cfg, palavra_convertida) in
      
      (* Imprime o PDA para depuração *)
      let (estados, alfabeto, pilha_alfabeto, transicoes, estado_inicial, estados_finais), fita = pda in
      print_endline "PDA Gerado:";
      Printf.printf "Estados: [%s]\n" (String.concat ", " (List.map string_of_int estados));
      Printf.printf "Alfabeto: [%s]\n" (String.concat ", " (List.map (String.make 1) alfabeto));
      Printf.printf "Alfabeto da pilha: [%s]\n" (String.concat ", " (List.map (String.make 1) pilha_alfabeto));
      print_endline "Transições:";
      List.iter (fun (((de, simbolo_fita, simbolo_pilha), (para, simbolo_nova_pilha))) ->
        Printf.printf "  ((%d, %s, %s) -> (%d, %s))\n"
          de
          (match simbolo_fita with None -> "ε" | Some c -> String.make 1 c)
          (match simbolo_pilha with None -> "ε" | Some c -> String.make 1 c)
          para
          (match simbolo_nova_pilha with None -> "ε" | Some c -> String.make 1 c)
      ) transicoes;
      Printf.printf "Estado inicial: %d\n" estado_inicial;
      Printf.printf "Estados finais: [%s]\n" (String.concat ", " (List.map string_of_int estados_finais));
      Printf.printf "Fita: [%s]\n" (String.concat ", " (List.map (String.make 1) fita));
      
      (* Simula o PDA *)
      let resultado = simular_pda pda in
      if resultado then print_endline "Aceita"
      else print_endline "Rejeita"
    )
    
    else if Str.string_match re_pdaw s 0 then (
      (* Processar como PDA *)
      let pdaw = json |> pdaw_of_yojson in
      let (pda, palavra) = pdaw in
      (* Imprime os detalhes do PDA *)
      let (estados, alfabeto, pilha_alfabeto, transicoes, estado_inicial, estados_finais) = pda in
     
   
    
      (* Converter palavra para lista de char *)
      let palavra_convertida = List.filter_map (function None -> None | Some c -> Some c) palavra in
    
      (* Simular a palavra no PDA *)
      let resultado = simular_pda ((estados, alfabeto, pilha_alfabeto, transicoes, List.hd estado_inicial, estados_finais), palavra_convertida) in
      if resultado then print_endline "Aceita"
      else print_endline "Rejeita"
    )
    else ()
  with
  | Yojson.Json_error _ -> ()
  | Failure _ ->()

