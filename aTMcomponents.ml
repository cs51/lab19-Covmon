(* ATM Components Implementation *)

open Scanf 
open Stack

type id = int 

type action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)
;; 

type account_spec = {name : string; id : id; balance : int} ;;
type account = {acct_name : string; acct_id : id; mutable current_balance : int}

let account_database = create ()

let initialize (accounts : account_spec list) : unit = 
  List.iter (fun account -> push 
    {acct_name = account.name; acct_id = account.id; current_balance = account.balance} account_database) 
    accounts
  ;;

let present_message (msg : string) : unit = 
  Printf.printf "%s\n" msg 
;;

let acquire_id () = 
  present_message "Enter customer id:";
  read_int () ;;

let acquire_amount () = read_int () ;;

(* eplotnick@college 
 kevinsu@college.harvard.edu 
 Seeam Noor: seeamnoor@college.harvard.edu *)

let rec acquire_act () : action = 
  present_message "Enter action: (B) Balance (-) Withdraw (+) Deposit (=) Done (X) Exit: ";
  let input = read_line () in 
  match input with 
  | "B" -> Balance 
  | "-" -> 
    Printf.printf ("Enter amount: ");
    let amt = read_int () in Withdraw amt
  | "+" -> 
    Printf.printf ("Enter amount: ");
    let amt = read_int () in Deposit amt
  | "=" -> Next 
  | "X" -> Finished 
  | _ -> acquire_act ()
  ;;

let get_balance (id : id) : int = 
  fold (fun acc acct -> if acct.acct_id = id then acct.current_balance else acc) 0 account_database
;;

let get_name (id : id) : string = 
  fold (fun acc acct -> if acct.acct_id = id then acct.acct_name else acc) "" account_database
;;

let update_balance (id : id) (amt : int) : unit = 
  iter (fun acct -> if acct.acct_id = id then acct.current_balance <- amt else ()) account_database
;;

let deliver_cash (amt : int) : unit = 
  present_message ("Here's your $" ^ (string_of_int amt) ^ ".00")
;;



  
