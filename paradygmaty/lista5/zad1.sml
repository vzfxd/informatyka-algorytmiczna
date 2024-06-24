(* binomial *)
fun binomial n 0 = 1
  | binomial n k = 
    if n = k then 1 
    else binomial (n-1) (k-1) + binomial (n-1) k;



fun pascal_row 0 = [1]
  | pascal_row n =
      let
          val prev_row = pascal_row (n - 1)
          val extended_prev_row = prev_row @ [0]
          val shifted_prev_row = 0 :: prev_row
      in
          ListPair.map (op +) (extended_prev_row, shifted_prev_row)
      end


fun binomial2 n k = List.nth (pascal_row n, k);

fun merge (xs, []) = xs
  | merge ([], ys) = ys
  | merge (x::xs, y::ys) =
      if x <= y then x :: merge (xs, y::ys)
      else y :: merge (x::xs, ys)

fun mergeSort [] = []
  | mergeSort [x] = [x]
  | mergeSort xs =
      let
          val mid = length xs div 2
          val left = List.take (xs, mid)
          val right = List.drop (xs, mid)
      in
          merge (mergeSort left, mergeSort right)
      end



(* diophant eq  *)
fun extended_gcd a 0 = (a, 1, 0)
  | extended_gcd a b = 
    let val (d, x, y) = extended_gcd b (a mod b)
    in 
        (d, y, x - y * (a div b))
    end;

fun de a b =  extended_gcd a b


(* prime factors *)
fun prime_factors n =
  let
    fun prime_factors 1 d acc = acc
        | prime_factors n d acc = 
            if n mod d = 0 then prime_factors (n div d) d (d::acc)
            else prime_factors n (d+1) acc
  in
    prime_factors n 2 []
  end;


(*totient *)
fun totient n =
  let
    fun gcd a 0 = a
      | gcd a b = gcd b (a mod b)

    fun totient n k acc =
      if k = n then acc
      else if gcd n k = 1 then totient n (k + 1) (acc + 1)
      else totient n (k + 1) acc
  in
    totient n 1 0
  end;


(*totient2 *)
(* phi(n) = (p1^(k1 -1)) * (p1 - 1) * (p2^(k2 -1)) * (p2 - 1) * ... * (pr^(kr -1)) * (pr - 1)*)
fun totient2 n =
  let
    fun totient2 acc divisors =
      case divisors of
          [] => acc
        | first :: second :: rest =>
            if first = second then totient2 (acc * first) (second :: rest)
            else totient2 (acc * (first - 1)) (second :: rest)
        | first :: [] => acc * (first - 1)
  in
    totient2 1 (prime_factors n)
  end;



(* primes *)
fun sito n =
  let
    fun range a b = if a > b then [] else a :: range (a + 1) b

    fun sito_ numbers it =
        if it * it >= n then numbers
        else
          case numbers of
            [] => []
          | first :: rest =>
            first :: sito_ (List.filter (fn x => x mod first <> 0) rest) (it + 1)

  in
    sito_ (range 2 n) 0
  end;

fun joinWithSeparator sep [] = ""
  | joinWithSeparator sep [x] = x
  | joinWithSeparator sep (x::xs) = x ^ sep ^ joinWithSeparator sep xs;

fun main () =
    let
        val n = 5
        val k = 2
        val list = [5, 1, 9, 3, 7, 6, 2, 8, 4]

        (* Pomiar czasu dla funkcji binomial *)
        val startTimeBinomial = Time.now ()
        val binomialResult = binomial n k
        val endTimeBinomial = Time.now ()
        val elapsedBinomial = Time.toMilliseconds (Time.-(endTimeBinomial, startTimeBinomial))

        (* Pomiar czasu dla funkcji binomial2 *)
        val startTimeBinomial2 = Time.now ()
        val binomial2Result = binomial2 n k
        val endTimeBinomial2 = Time.now ()
        val elapsedBinomial2 = Time.toMilliseconds (Time.-(endTimeBinomial2, startTimeBinomial2))

        (* Pomiar czasu dla funkcji totient *)
        val startTimeTotient = Time.now ()
        val totientResult = totient 21
        val endTimeTotient = Time.now ()
        val elapsedTotient = Time.toMilliseconds (Time.-(endTimeTotient, startTimeTotient))

        (* Pomiar czasu dla funkcji totient2 *)
        val startTimeTotient2 = Time.now ()
        val totient2Result = totient2 21
        val endTimeTotient2 = Time.now ()
        val elapsedTotient2 = Time.toMilliseconds (Time.-(endTimeTotient2, startTimeTotient2))    

        val primesResult = sito 21
        val primeFactorsResult = prime_factors 21
        val deResult = de 15 4
    in
        print "==test==\n";
        print ("binomial 5 2: " ^ Int.toString binomialResult ^ " took " ^ IntInf.toString elapsedBinomial ^ " milliseconds\n");
        print ("binomial2 5 2: " ^ Int.toString binomial2Result ^ " took " ^ IntInf.toString elapsedBinomial2 ^ " milliseconds\n");
        print ("mergesort [5,1,9,3,7,6,2,8,4]: " ^ String.concatWith ", " (map Int.toString list) ^ "\n");
        print ("de 15 4: " ^ Int.toString (#1 deResult) ^ " " ^ Int.toString (#2 deResult) ^  " " ^ Int.toString (#3 deResult) ^ "\n");
        print ("primeFactors 21: " ^ String.concatWith ", " (map Int.toString primeFactorsResult) ^ "\n");
        print ("totient 21: " ^ Int.toString totientResult ^ " took " ^ IntInf.toString elapsedTotient ^ " milliseconds\n");
        print ("totient2 21: " ^ Int.toString totient2Result ^ " took " ^ IntInf.toString elapsedTotient2 ^ " milliseconds\n");
        print ("primes 21: " ^ String.concatWith ", " (map Int.toString primesResult) ^ "\n");
        print "=========\n"
    end;

main ();

