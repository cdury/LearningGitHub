// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

// Exercises 1 //
/// Exercise 1.1
let g n = n + 4
/// Exercises 1.2
let h x (y:float) = sqrt(x*x + y*y)
/// Exercise 1.3
let gf  = fun n -> n+ 4
let hf  = fun x (y:float) -> sqrt(x*x + y*y) 
/// Exercise 1.4
let rec f = function
   | 0 -> 0
   | n -> n + f(n-1)
//f 4
/// Exercise 1.5
let rec fib  = function
   | 0 -> 0
   | 1 -> 1
   | n ->  fib(n-1) + fib(n-2)
//fib 4
/// Exercise 1.6
let rec sum  = function
   | (m,0) -> m
   | (m,n) -> sum (m,n-1) + m + n
//sum (1,2)
///Exercise 1.7
let a = 5
let f' a = a + 1
//f' 3
//g' 3


// Exercises 2 //
/// Exercise 2.1
let f = function
   | n when n%5 = 0 -> false
   | n when (n%2 = 0) || (n%3=0) -> true
   | _ -> false
//f(24)
//f(27)
//f(29)
/// Exercise 2.2
let rec pow = function
    | (s,0) -> ""
    | (s,n) -> s + pow(s,n-1)
/// Exercise 2.3
let isIthChar ( (str:string), i ,ch)=
    str.[i] = ch
/// Exercise 2.4
let rec occFromIth ((str:string),i,ch)= 
   let s = 
      if String.length str >= i then 
         str.[i..]
      else
          ""
   //String.filter (fun x -> x = ch) s |> String.length
   s
/// Exercise 2.5
let occInString(str,ch) = occFromIth(str,0,ch)
/// Exercise 2.6
let notDivisible =
    function
    | (d,n) when n%d = 0 -> false
    | _ -> true
//notDivisible(2,5)
//notDivisible(3,9)
// Exercise 2.7
let rec test =
    function
    | (a,b,c) when a > b  -> true 
    | (a,b,c) when a = b  -> notDivisible(b,c)
    | (a,b,c) -> notDivisible(a,c) && test(a+1,b,c)
let prime n = test(2,n-1,n)
let rec nextPrime =
    function
    | n when prime (n+1) -> (n+1)
    | n -> nextPrime (n+1)
/// Exercise 2.8
let rec bin =
    function
    | (n,0) -> 1
    | (n,k) when n = k -> 1
    | (n,k) when n > k && n <> 0 -> bin (n-1,k-1) + bin (n-1,k)
    | _ -> 0
/// Exercise 2.9
let rec f =
    function
    | (0,y) -> y
    | (x,y) -> f(x-1,x+y)
/// Exercise 2.10
let test(c,e) =
    if c then e
    else      0
/// Exercise 2.11
let VAT n x = x * ( 1.0 +  float(n)/100.0)
let unVAT n y  = y / ( 1.0 +  float(n)/100.0)
/// Exercise 2.12
let min f =
    let rec testf =
        function
        | n when f n = 0 -> n
        | n -> testf (n+1)
    testf 0
//min (fun n -> n - 7)
/// Exercise 2.13
let curry f   = fun a -> fun b -> f (a,b)
let uncurry f = fun (a,b) -> f a b

/// Exercise 3.1
type Meridiem =
    | AM
    | PM
type t_rec =
    { ampm : Meridiem
      hour : int
      min  : int
      }
let isEarlier (h1,m1,a1) (h2,m2,a2) =
    if a1 = a2 then
        (h1,m1) < (h2,m2)
    elif a1 = "AM" then
        true
    else
        false
let isEarlier_rec (t1:t_rec) (t2:t_rec) =
    t1 < t2
//isEarlier (11,59,"AM") (1,15,"PM)
//let t1 = {
//    ampm = AM
//    hour = 11
//    min  = 59
//    }
//let t2 = {
//    ampm = PM
//    hour = 1
//    min  = 15
//    }   
//isEarlier_rec t1 t2
/// Exercise 3.2
let rec sum_b amount1 amount2 =
    let (p1,s1,pound1) = amount1
    let (p2,s2,pound2) = amount2
    if p1 + p2 >= 12 then 
       sum_b (p1+p2-12,s1+1,pound1) (0,s2,pound2)
    elif (s1 + s2) >= 20 then
        sum_b (p1,s1+s2-20,pound1 + 1) (p2,0,pound2)
    else
        ( p1 + p2,s1 + s2,pound1 + pound2)
//sum_b ( 1,1,1) ( 3,3,3)
//sum_b (7,7,7) (9,19,9)
let rec min_b amount1 amount2 =
    let (p1,s1,pound1) = amount1
    let (p2,s2,pound2) = amount2
    if p1 < p2  then 
       min_b (p1+12,s1-1,pound1) (p2,s2,pound2)
    elif s1 < s2  then
        min_b (p1,s1+20,pound1 - 1) (p2,s2,pound2)
    elif pound1 < pound2 then
        failwith "Negative Amount of money!"
    else
        ( p1 - p2,s1 - s2,pound1 - pound2)
//min_b (3,3,3) (1,1,1)
//min_b (1,1,0) (11,0,0)
//min_b (0,0,1) ( 1,0,0)
/// Exercise 3.3 (complex numbers)
type complex =
    { real: double
      img : double}
let (+.) (n:complex) (m:complex) =
    { real = n.real + m.real ; img = n.img + m.img }
let (-.) (n:complex) (m:complex) =
    { real = n.real - m.real ; img = n.img - m.img }
let ( *.) (n:complex) (m:complex) =
    { real = n.real * m.real - n.img * m.img ; img = n.img * m.real + n.real * m.img }
let (/.) (n:complex) (m:complex) =
    let norm = m.real * m.real + m.img * m.img
    n *. { real= m.real / norm ; img = -m.img / norm}
let c1 = { real = 1.0 ; img = 1.0}
let c2 = { real = 0.0 ; img = 1.0}
/// Exercise 3.5
type Solution =
    | Non of unit
    | One of double
    | Two of double * double
let solve (a,b,c) =
    let det = b * b - 4.0 * a * c
    match (a,b,c) with
    | (0.0,b,c)              -> One (- c / b)
    | (a,b,c) when det < 0.0 -> Non ()
    | (a,b,c)                -> Two (-b + sqrt(det)/(2.0 * a),-b - sqrt(det)/(2.0 * a))

/// Exercise 4.1
let upto n = [1..n]
/// Exercise 4.2
let downto1 n = [n..(-1)..1] 
//// Exercise 4.3
let evenN n = [2..2..(2*n)]
/// Exercise 4.4
let rec altsum =
    function
    | [] -> 0
    | x::xs ->  x - altsum xs
//altsum [2;-1;3]
/// Exercise 4.5
let rec rmodd =
    function
    | []         -> []
    | x0::x1::xs -> x0::rmodd xs
    | x0::xs     -> x0::[]
//rmodd ['a';'b';'c';'d']
//rmodd ['a';'b';'c']
//rmodd ['a';'b']
//rmodd ['a']
/// Exercise 4.6
let rec rmeven =
    function
    | []                   -> []
    | x0::xs when x0%2 = 0 -> rmeven xs
    | x0::xs               -> x0::rmeven xs
//rmeven [1;2;3;4]
/// Exercise 4.7
let rec multiplicity y  =
    function
    | x::xs when x = y -> 1 + multiplicity y xs
    | x::xs            ->     multiplicity y xs
    | []               -> 0 
//multiplicity 11 [1;2;11;33;11;0]
/// Exercise 4.8
let split xs =
    let rec carry res l =
        let (a,b) = res 
        match l with
        | x1::x2::xs -> carry (a @ [x1], b @ [x2]) xs
        | _          -> (a,b)
    carry ([],[]) xs
let rec split' =
    function
    | x1::x2::xs -> let (y,z) = split xs
                    (x1::y, x2::z)
    | _          -> ([],[])
//split ["a";"1";"b";"2"]
//split' ["a";"1";"b";"2"]
/// Exercise 4.9
let rec zip =
    function
    | (x1::xs),(y1::ys) -> (x1,y1) :: zip (xs,ys)
    | _ -> []
//zip ([1;2;3],['a';'b';'c']) 
/// Exercise 4.10
let rec prefix l1 l2=
    match l1,l2 with
    | x1::xs , y1::ys -> x1 = y1 &&  prefix xs ys 
    | [],_            -> true
    | _,[]            -> false
//prefix [1;2;3] [1;2;3;4]
/// Exercise 4.11
let rec count ((l:int list),n) =
    match l with
    | x::xs when x > n ->  0
    | x::xs when x=n   ->  1 + count (xs,n)
    | x::xs            ->  0 + count (xs,n)
    | _                ->  0
//count ([1;2;3;3;4;5],3)
let rec insert ((l:int list),n) =
    match l with
    | []                                        -> n::[]
    | x1::(x2::_ as xs) when n >= x1 && n <= x2 -> x1::n::xs
    | x1::[]            when n >= x1            -> x1::n::[]
    | x1::xs            when n <= x1            -> n::l 
    | x1::xs                                    -> x1:: insert(xs,n) 
//insert([2;3;5],4)
let rec intersect (l1,l2) =
    match l1,l2 with
    | x::xs , y::ys when x = y -> x :: intersect(xs,ys)
    | x::xs , y::ys when x < y ->      intersect(xs,l2)
    | x::xs , y::ys when x > y ->      intersect(l1,ys)
    | _     , _                ->  []
//intersect([1;1;1;2;2],[1;1;2;4])
let rec plus (l1,l2) =
    match l2 with
    | []    -> l1
    | x::xs -> plus(insert(l1,x),xs)
//plus([1;1;2],[1;2;4])
let rec minus (l1,l2) =
    let intersection = intersect(l1,l2)
    match l1,intersection with
    | _ , []                  -> l1
    | x::xs , y::ys when x = y -> minus(xs,ys)
    | x::xs , y::ys when x < y -> x::minus(xs,intersection)
    | _ , _                    -> failwith "???"
//minus([1;1;1;2;2],[1;1;2;3])
//minus([1;1;2;3],[1;1;1;2;2])
/// Exercise 4.12
let rec sump(p,l) =
    match l with
    | []              -> 0
    | x::xs when p(x) -> x + sump(p,xs)
    | _::xs           ->     sump(p,xs) 
//sump((fun x -> x%2 = 0) , [1;2;2;4;5])
/// Exercise 4.13
let smallest l =
    let rec smallest' l i = 
       match l with
       | []               -> i
       | x::xs when x < i -> smallest' xs x 
       | _::xs            -> smallest' xs i
    smallest' l l.[0]
//smallest [1;2;-1;5] 
let rec delete(element,lst) =
    match lst with
    | []                     -> []
    | x::xs when x = element -> xs
    | x::xs                  -> x::delete(element,xs)
//delete(-1,[1;2;-1;4])
let rec sortiere lst =
    let mini = smallest lst
    let rest = delete(mini,lst)
    match rest with
    | []    -> [mini]
    | rest  -> mini :: sortiere rest
//sortiere [1;3;2]
/// Exercise 4.15
let rec revrev lstlst =
    let rec rev lst =
        match lst with
        | []    -> []
        | x::xs -> rev xs @ [x]
    match lstlst with
    | []      -> []
    | xs::xss -> revrev xss @ [rev xs]
//revrev [[1;2];[3;4;5]]
/// Exercise 5.1 (List.filter)
let filter p lst =
    let f = fun x rs ->  
               match  p x with
               | true  ->  x::rs 
               | false ->     rs
    List.foldBack f lst []
//List.filter (fun x -> x = 1) [1;2;1;3]
//filter (fun x -> x = 1) [1;2;1;3]
/// Exercise 5.2 ()
let revrev_fold lstlst =
    let rev lst  = List.fold (fun rs x -> x::rs) [] lst 
    let f = fun rs x -> (rev x)::rs
    List.fold f [] lstlst
//revrev_fold [[1;2];[3;4;5]]
/// Exercise 5.3
let sump_fold(p,lst) =
    let f = fun rs x -> match p x with
                        | true -> rs + x
                        | false-> rs
    List.fold f 0 lst
//sump_fold((fun x -> x%2 = 0) , [1;2;2;4;5])
/// Exercise 5.4
let downto_1 f n e =
    match n with
    | n when n <= 0 -> e
    | n             -> List.foldBack f [1..n] e
let fact_downto1 n = downto_1 (*) n 1
let g_list g n     =
    let f = fun n rs -> g(n)::rs
    downto_1 f n []
/// Exercise 5.6



















[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code

