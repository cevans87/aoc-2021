open Basis
open Basis.Rudiments

type line = String.t

let line_stream_of_bytes_stream bytes_stream =
    let line_of_accum accum = begin
        List.rev accum |> Array.of_list |> Bytes.to_string_hlt
    end in
    let rec f i accum bytes bytes_stream = begin
        match i = Bytes.Slice.length bytes with
        | true -> begin
            match Stream.is_empty bytes_stream with
            | true -> Stream.Cons (line_of_accum accum, lazy Stream.Nil)
            | false -> begin
                let bytes, bytes_stream = Stream.pop bytes_stream in
                f 0L accum bytes bytes_stream
              end
          end
        | false -> begin
            let c = Bytes.Slice.get i bytes in
            match Byte.(c = of_char '\n') with
            | true -> Stream.Cons (line_of_accum accum, lazy (f (i + 1L) [] bytes bytes_stream))
            | false -> f (i + 1L) (List.push c accum) bytes bytes_stream
          end
    end in
    lazy (f 0L [] (Bytes.Slice.init [||]) bytes_stream)

let uns_stream_of_line_stream line_stream =
    let rec f line_stream = begin
        match Stream.is_empty line_stream with
        | true -> Stream.Nil
        | false -> begin
            let line, line_stream = Stream.pop line_stream in
            Stream.Cons (Uns.of_string line, lazy (f line_stream))
          end
    end in
    lazy (f line_stream)

let p01a_answer_of_uns_stream uns_stream =
    let rec f i accum uns_stream = begin
        match Stream.is_empty uns_stream with
        | true -> accum
        | false -> begin
            let j, uns_stream = Stream.pop uns_stream in
            match Uns.(i < j) with
            | true -> f j (accum + 1L) uns_stream
            | false -> f j accum uns_stream
          end
    end in
    let i, uns_stream = Stream.pop uns_stream in
    f i 0L uns_stream

let p01b_answer_of_uns_stream uns_stream =
    let rec f i j k accum uns_stream = begin
        match Stream.is_empty uns_stream with
        | true -> accum
        | false -> begin
            let l, uns_stream = Stream.pop uns_stream in
            match Uns.(i < l) with
            | true -> f j k l (accum + 1L) uns_stream
            | false -> f j k l accum uns_stream
          end
    end in
    let i, uns_stream = Stream.pop uns_stream in
    let j, uns_stream = Stream.pop uns_stream in
    let k, uns_stream = Stream.pop uns_stream in
    f i j k 0L uns_stream

let fmt_answer name answer =
   File.Fmt.stdout
   |> String.fmt name
   |> String.fmt ": "
   |> Uns.fmt answer
   |> String.fmt "\n"
   |> ignore;
   ()

let () =
    "p01.input"
    |> String.C.Slice.of_string
    |> Bytes.Slice.of_string_slice
    |> File.of_path_hlt
    |> File.Stream.of_file
    |> line_stream_of_bytes_stream
    |> uns_stream_of_line_stream
    |> function uns_stream ->
        uns_stream |> p01a_answer_of_uns_stream |> fmt_answer "p01a";
        uns_stream |> p01b_answer_of_uns_stream |> fmt_answer "p01b";

