open Basis
open Basis.Rudiments

type line = String.t
type jump =
| Forward of sint
| Down of sint
| Up of sint

let token_stream_of_bytes_stream bytes_stream =
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
            match Byte.(c = of_char '\n' || c = of_char ' ') with
            | true -> Stream.Cons (line_of_accum accum, lazy (f (i + 1L) [] bytes bytes_stream))
            | false -> f (i + 1L) (List.push c accum) bytes bytes_stream
          end
    end in
    lazy (f 0L [] (Bytes.Slice.init [||]) bytes_stream)

let jump_stream_of_token_stream token_stream =
    let rec f token_stream = begin
        match Stream.is_empty token_stream with
        | true -> Stream.Nil
        | false -> begin
            let direction_token, token_stream = Stream.pop token_stream in
            let distance_token, token_stream = Stream.pop token_stream in
            let distance = Sint.of_string distance_token in
            let jump = begin
                match direction_token with
                | _ when String.(direction_token = "forward") -> Forward distance
                | _ when String.(direction_token = "down") -> Down distance
                | _ when String.(direction_token = "up") -> Up distance
                | _ -> halt "bad input"
            end in
            Stream.Cons (jump, lazy (f token_stream))
          end
    end in
    lazy (f token_stream)

let p02a_answer_of_jump_stream jump_stream =
    let rec f hpos depth jump_stream = begin
        match Stream.is_empty jump_stream with
        | true -> Sint.(hpos * depth)
        | false -> begin
            let jump, jump_stream = Stream.pop jump_stream in
            let open Sint in
            match jump with
            | Forward distance -> f (hpos + distance) depth jump_stream
            | Down distance -> f hpos (depth + distance) jump_stream
            | Up distance -> f hpos (depth - distance) jump_stream
          end
    end in
    f 0L 0L jump_stream

let p02b_answer_of_jump_stream jump_stream =
    let rec f aim hpos depth jump_stream = begin
        match Stream.is_empty jump_stream with
        | true -> Sint.(hpos * depth)
        | false -> begin
            let jump, jump_stream = Stream.pop jump_stream in
            let open Sint in
            match jump with
            | Forward distance -> f aim (hpos + distance) (depth + (aim * distance)) jump_stream
            | Down distance -> f (aim + distance) hpos depth jump_stream
            | Up distance -> f (aim - distance) hpos depth jump_stream
          end
    end in
    f 0L 0L 0L jump_stream

let fmt_answer name answer =
   File.Fmt.stdout
   |> String.fmt name
   |> String.fmt ": "
   |> Sint.fmt answer
   |> String.fmt "\n"
   |> ignore;
   ()

let () =
    "p02.input"
    |> String.C.Slice.of_string
    |> Bytes.Slice.of_string_slice
    |> File.of_path_hlt
    |> File.Stream.of_file
    |> token_stream_of_bytes_stream
    |> jump_stream_of_token_stream
    |> function jump_stream ->
        jump_stream |> p02a_answer_of_jump_stream |> fmt_answer "p02a";
        jump_stream |> p02b_answer_of_jump_stream |> fmt_answer "p02b";

