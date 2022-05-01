use "w1_def.sml";

structure W1 :> S1 = struct
    fun f_squared(mat : matrix) = foldl op+ 0 (map (foldl op+ 0) (map (map (fn x=>x*x)) mat));
    fun renumerate(l : 'a list) = foldr (fn (a, b) => (length b + 1, a) :: b) [] l;
end;