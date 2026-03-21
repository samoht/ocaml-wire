type +'a t = Staged of 'a [@@unboxed]

let stage x = Staged x
let unstage (Staged x) = x
