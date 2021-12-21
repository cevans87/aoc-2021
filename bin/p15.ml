open Basis
open Basis.Rudiments

let byte_stream_of_bytes_stream bytes_stream =
    let rec f i bytes bytes_stream = begin
        match i = Bytes.Slice.length bytes with
        | true -> begin
            match Stream.is_empty bytes_stream with
            | true -> Stream.Nil
            | false -> begin
                let bytes, bytes_stream = Stream.pop bytes_stream in
                f 0L bytes bytes_stream
              end
          end
        | false -> begin
            let byte = Bytes.Slice.get i bytes in
            Stream.Cons (byte, lazy (f (i + 1L) bytes bytes_stream))
          end
    end in
    lazy (f 0L (Bytes.Slice.init [||]) bytes_stream)

let uns_stream_stream_of_byte_stream byte_stream =
    let rec g byte_stream = begin
        match Stream.is_empty byte_stream with
        | true -> Stream.Nil
        | false -> begin
            let b, byte_stream = Stream.pop byte_stream in
            match Byte.(b = of_char '\n') with
            | true -> Stream.Nil
            | false -> Stream.Cons(Byte.(extend_to_uns (b - of_char '0')), lazy (g byte_stream))
          end
    end in
    let rec f byte_stream = begin
        match Stream.is_empty byte_stream with
        | true -> Stream.Nil
        | false -> begin
            let b, byte_stream = Stream.pop byte_stream in
            match Byte.(b = of_char '\n')  with
            | true -> Stream.Cons(lazy (g byte_stream), lazy (f byte_stream))
            | false -> f byte_stream
          end
    end in
    lazy (Stream.Cons (lazy (g byte_stream), lazy (f byte_stream)))


type chit = uns Array.t Array.t

let chit_n_of_uns_stream_stream n uns_stream_stream =
    let uns_stream, _ = Stream.pop uns_stream_stream in
    let rows' = Stream.length uns_stream_stream in
    let cols' = Stream.length uns_stream in
    let rows = n * rows' in
    let cols = n * cols' in
    let chit = Array.init (0L=:<cols) ~f:(fun _ -> Array.init (0L=:<rows) ~f:(fun _ -> 0L)) in

    let rec f'' d j'' uns_array uns_stream = begin
        match Stream.is_empty uns_stream with
        | true -> ()
        | false -> begin
            let uns, uns_stream = Stream.pop uns_stream in
            let uns = uns + d in
            let uns = begin
                match uns > 9L with
                | true -> uns - 9L
                | false -> uns
              end in
              let () = Array.set_inplace j'' uns uns_array in
              f'' d (j'' + 1L) uns_array uns_stream
          end
      end in
    let rec f' i' i j uns_stream_stream = begin
        match Stream.is_empty uns_stream_stream with
        | true -> ()
        | false -> begin
            let uns_stream, uns_stream_stream = Stream.pop uns_stream_stream in
            let i'' = (i * rows') + i' in
            let uns_array = Array.get i'' chit in
            let () = f'' (i + j) (j * cols') uns_array uns_stream in
            f' (i' + 1L) i j uns_stream_stream
          end
    end in
    let rec f i j uns_stream_stream = begin
        match i = n, j = n with
        | true, _ -> ()
        | _, true -> f (i + 1L) 0L uns_stream_stream
        | false, false -> begin
            let () = f' 0L i j uns_stream_stream in
            f i (j + 1L) uns_stream_stream
          end
    end in
    let () = f 0L 0L uns_stream_stream in
    chit

let p15_answer_of_chit chit =
    let max_i = Array.length chit - 1L in
    let max_j = Array.(length (get 0L chit) - 1L) in
    let risk = Array.(
        init (0L=:<max_i + 1L) ~f:(fun _ -> init (0L=:<max_j + 1L) ~f:(fun _ -> 10000L))
      )
    in
    let () = Array.get 0L risk |> Array.set_inplace 0L 0L in
    let get_chit i j = begin
        Array.(get (Uns.bits_of_sint i) chit |> get Uns.(bits_of_sint j))
    end in
    let get_risk i j = begin
        match Sint.(i < 0L || i > max_i || j < 0L || j > max_j) with
        | true -> 100000L
        | false -> Array.(get (Uns.bits_of_sint i) risk |> get Uns.(bits_of_sint j))
    end in
    let set_risk i j r = begin
        Array.(get (Uns.bits_of_sint i) risk |> set_inplace Uns.(bits_of_sint j) r)
    end in
    let rec g accum i j = begin
        match Sint.(j > max_j) with
        | true -> accum
        | false -> begin
            match Sint.(j < 0L) with
            | true -> g accum i 0L
            | false -> begin
                let r = get_risk i j in
                let r' = r in
                let v = get_chit i j in
                let r' = min r' (v + get_risk i (j - 1L)) in
                let r' = min r' (v + get_risk i (j + 1L)) in
                let r' = min r' (v + get_risk (i - 1L) j) in
                let r' = min r' (v + get_risk (i + 1L) j) in
                match r = r' with
                | true -> g accum i (j + 1L)
                | false -> begin
                    let () = set_risk i j r' in
                    g true i (j - 1L)
                  end
              end
          end
    end in
    let rec f i = begin
        match Sint.(i > max_i)  with
        | true -> get_risk max_i max_j
        | false -> begin
            match Sint.(i < 0L) with
            | true -> f 0L
            | false -> begin
                match g false i 0L with
                | true -> f (i - 1L)
                | false -> f (i + 1L)
              end
          end
    end in
    f 0L

let fmt_answer name answer =
   File.Fmt.stdout
   |> String.fmt name
   |> String.fmt ": "
   |> Sint.fmt answer
   |> String.fmt "\n"
   |> ignore;
   ()

let () =
    "p15.input"
    |> String.C.Slice.of_string
    |> Bytes.Slice.of_string_slice
    |> File.of_path_hlt
    |> File.Stream.of_file
    |> byte_stream_of_bytes_stream
    |> uns_stream_stream_of_byte_stream
    |> fun uns_stream_stream ->
        chit_n_of_uns_stream_stream 1L uns_stream_stream
        |> p15_answer_of_chit
        |> fmt_answer "p15a";
        chit_n_of_uns_stream_stream 5L uns_stream_stream
        |> p15_answer_of_chit
        |> fmt_answer "p15b";
        

