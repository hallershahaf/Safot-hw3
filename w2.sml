use "w2_def.sml";

structure W2 : S2 = struct 
fun valAt(l : 't list) = 
    let
        fun valAtAux(l : 't list, index : int) =
            if (index < 0) orelse (index >= length l) then
                raise Subscript
            else if index = 0 then
                hd l
            else
                valAtAux(tl l, index - 1);
    in
        fn (index : int) => valAtAux(l, index)
    end
fun removeDupes([] : ''a list) = []
    | removeDupes(x::xs : ''a list) =
    x :: removeDupes(List.filter (fn (member : ''a) => x <> member) (xs));
fun histogram ([] : int list) = []
    | histogram (x::xs : int list) = 
        (x, length (List.filter (fn (member : int) => x = member) (x::xs))) 
            :: histogram(List.filter (fn (member : int) => x <> member) xs);
end;