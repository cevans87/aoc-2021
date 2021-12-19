open Basis
open Basis.Rudiments

type line = String.t
type jump =
| Forward of sint
| Down of sint
| Up of sint

let byte_stream_of_bytes_slice_stream bytes_slice_stream =
    let rec f i bytes_slice bytes_slice_stream = begin
        match i = Bytes.Slice.length bytes_slice with
        | true -> begin
            match Stream.is_empty bytes_slice_stream with
            | true -> Stream.Nil
            | false -> begin
                let bytes_slice, bytes_slice_stream = Stream.pop bytes_slice_stream in
                f 0L bytes_slice bytes_slice_stream
              end
          end
        | false -> begin
            let byte = Bytes.Slice.get i bytes_slice in
            Stream.Cons (byte, lazy (f (i + 1L) bytes_slice bytes_slice_stream))
          end
    end in
    lazy (f 0L (Bytes.Slice.init [||]) bytes_slice_stream)

let byte_stream_stream_of_byte_stream byte_stream =
    let rec f' byte_stream = begin
        match Stream.is_empty byte_stream with
        | true -> Stream.Nil
        | false -> begin
            let byte, byte_stream = Stream.pop byte_stream in
            match Byte.(byte = of_char '\n') with
            | true -> Stream.Nil
            | false -> Stream.Cons(byte, lazy (f' byte_stream))
          end
    end in
    let rec f byte_stream = begin
        match Stream.is_empty byte_stream with
        | true -> Stream.Nil
        | false -> begin
            let byte, byte_stream = Stream.pop byte_stream in
            match Byte.(byte = of_char '\n')  with
            | true -> Stream.Cons(lazy (f' byte_stream), lazy (f byte_stream))
            | false -> f byte_stream
          end
    end in
    lazy (Stream.Cons (lazy (f' byte_stream), lazy (f byte_stream)))

let uns_stream_stream_of_byte_stream_stream byte_stream_stream =
    let rec f' byte_stream = begin
        match Stream.is_empty byte_stream with
        | true -> Stream.Nil
        | false -> begin
            let byte, byte_stream = Stream.pop byte_stream in
            match Byte.(byte = of_char '0') with
            | true -> Stream.Cons(0L, lazy (f' byte_stream))
            | false -> Stream.Cons(1L, lazy (f' byte_stream))
          end
    end in
    let rec f byte_stream_stream = begin
        match Stream.is_empty byte_stream_stream with
        | true -> Stream.Nil
        | false -> begin
            let byte_stream, byte_stream_stream = Stream.pop byte_stream_stream in
            Stream.Cons(lazy (f' byte_stream), lazy (f byte_stream_stream))
          end
    end in
    lazy (f byte_stream_stream)

type bias = uns Array.t

let bias_of_uns_stream_stream uns_stream_stream =
    let uns_stream, _ = Stream.pop uns_stream_stream in
    let accum = Array.init (0L=:<Stream.length uns_stream) ~f:(fun _ -> 0L) in
    let rec f'' i uns_stream = begin
        match Stream.is_empty uns_stream with
        | true -> ()
        | false -> begin
            let uns, uns_stream = Stream.pop uns_stream in
            match uns = 0L with
            | true -> f'' (i + 1L) uns_stream
            | false -> begin
                let () = Array.(set_inplace i ((get i accum) + 1L) accum) in
                f'' (i + 1L) uns_stream
              end
          end
    end in
    let rec f' uns_stream_stream = begin
        match Stream.is_empty uns_stream_stream with
        | true -> ()
        | false -> begin
            let uns_stream, uns_stream_stream = Stream.pop uns_stream_stream in
            let () = f'' 0L uns_stream in
            f' uns_stream_stream
          end
    end in
    let () = f' uns_stream_stream in
    let mid = (Stream.length uns_stream_stream) / 2L in
    let bias = Array.init (0L=:<Array.length accum) ~f:(fun _ -> 0L) in
    let rec f i = begin
        match (i = Array.length accum) with
        | true -> bias
        | false -> begin
            let () = begin
                match (Array.get i accum) < mid with
                | true -> Array.set_inplace i 0L bias
                | false -> Array.set_inplace i 1L bias
            end in
            f (i + 1L)
          end
    end in
    f 0L

let p03a_answer_of_bias bias =
    let rec f i want accum bias = begin
        match (i = Array.length bias) with
        | true -> accum
        | false -> begin
            match (Array.get i bias) = want with
            | true -> f (i + 1L) want Uns.(accum * 2L) bias
            | false -> f (i + 1L) want Uns.((accum * 2L) + 1L) bias
          end
    end in
    let gamma = f 0L 1L 0L bias in
    let epsilon = f 0L 0L 0L bias in
    Uns.(gamma * epsilon)

let biased_uns_stream_of_uns_stream_stream biaser uns_stream_stream =
    let rec get_constant c accum uns_stream_stream = begin
        match Stream.is_empty uns_stream_stream with
        | true -> accum
        | false -> begin
            let uns_stream, uns_stream_stream = Stream.pop uns_stream_stream in
            match Stream.is_empty uns_stream with
            | true -> accum
            | false -> begin
                match Stream.hd uns_stream = c with
                | true -> get_constant c (accum + 1L) uns_stream_stream
                | false -> get_constant c accum uns_stream_stream
              end
          end
    end in
    let rec filter c uns_stream_stream = begin
        match Stream.is_empty uns_stream_stream with
        | true -> Stream.Nil
        | false -> begin
            let uns_stream, uns_stream_stream = Stream.pop uns_stream_stream in
            match Stream.is_empty uns_stream with
            | true -> Stream.Nil
            | false -> begin
                match Stream.hd uns_stream = c with
                | true -> Stream.Cons (Stream.tl uns_stream, lazy (filter c uns_stream_stream))
                | false -> filter c uns_stream_stream
              end
          end
    end in
    let rec f uns_stream_stream = begin
        match Stream.is_empty uns_stream_stream with
        | true -> Stream.Nil
        | false -> begin
            let zeros = get_constant 0L 0L uns_stream_stream in
            let ones = get_constant 1L 0L uns_stream_stream in
            let total = zeros + ones in
            match total = 0L with
            | true -> Stream.Nil
            | false -> begin
                match total = 1L with
                | true -> Lazy.force (Stream.hd uns_stream_stream)
                | false -> begin
                    match biaser zeros ones with
                    | true -> Stream.Cons (0L, lazy (f (lazy (filter 0L uns_stream_stream))))
                    | false -> Stream.Cons (1L, lazy (f (lazy (filter 1L uns_stream_stream))))
                  end
                end
         end
    end in
    lazy (f uns_stream_stream)

let p03b_answer_of_uns_stream_stream uns_stream_stream =
    let rec f accum uns_stream = begin
        match Stream.is_empty uns_stream with
        | true -> accum
        | false -> f ((accum * 2L) + Stream.hd uns_stream) (Stream.tl uns_stream)
    end in
    let o2 = f 0L (biased_uns_stream_of_uns_stream_stream Uns.( > ) uns_stream_stream) in
    let co2 = f 0L (biased_uns_stream_of_uns_stream_stream Uns.( <= ) uns_stream_stream) in
    o2 * co2

let fmt_answer name answer =
   File.Fmt.stdout
   |> String.fmt name
   |> String.fmt ": "
   |> Sint.fmt answer
   |> String.fmt "\n"
   |> ignore;
   ()

let () =
    "p03.input"
    |> String.C.Slice.of_string
    |> Bytes.Slice.of_string_slice
    |> File.of_path_hlt
    |> File.Stream.of_file
    |> byte_stream_of_bytes_slice_stream
    |> byte_stream_stream_of_byte_stream
    |> uns_stream_stream_of_byte_stream_stream
    |> fun uns_stream_stream ->
        uns_stream_stream
        |> bias_of_uns_stream_stream
        |> fun bias ->
            p03a_answer_of_bias bias |> fmt_answer "p03a";
            p03b_answer_of_uns_stream_stream uns_stream_stream |> fmt_answer "p03b";
