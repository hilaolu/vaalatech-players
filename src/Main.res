open NodeJs

let createVarIntBuffer = value => {
  let buffer = []

  let rec reduce = (val, buf: array<int>) => {
    switch val {
    | val if val >= 0b10000000 => {
        let temp = val |> land(_, 0b01111111) |> lor(_, 0b10000000)
        let val = lsr(val, 7)
        let _ = Js.Array2.push(buf, temp)
        reduce(val, buf)
      }
    | _ => {
        let temp = land(val, 0b01111111)
        let _ = Js.Array2.push(buf, temp)
        buf
      }
    }
  }

  reduce(value, buffer) |> Buffer.fromArray
}

let createUnsignedShortBuffer = value => {
  Buffer.fromArray([lsr(value, 8), land(value, 0b11111111)])
}

let createPacket = buffers => {
  let packet = Buffer.concat(buffers)
  let packet_len = packet.length |> createVarIntBuffer
  Buffer.concat([packet_len, packet])
}

let createStringBuffer = value => {
  let stringBuffer = Buffer.fromStringWithEncoding(value, StringEncoding.ascii)
  let lengthBuffer = createVarIntBuffer(stringBuffer.length)
  Buffer.concat([lengthBuffer, stringBuffer])
}

let rec removeVarIntHead = (arr: array<int>) => {
  switch arr {
  | [] => []
  | arr if Array.getUnsafe(arr, 0) >= 128 => removeVarIntHead(Array.sliceToEnd(arr, ~start=1))
  | _ => Array.sliceToEnd(arr, ~start=1) // we have reach the end
  }
}

let fetch = async request => {
  let {readable, writable} = Worker.connect("home.vaala.tech:25565")
  let writer =
    writable |> Worker.WritableStream.getWriter |> Worker.WritableStreamDefaultWriter.write

  let reader =
    readable |> Worker.ReadableStream.getReader |> Worker.ReadableStreamDefaultReader.read

  let b = [
    createVarIntBuffer(0),
    createVarIntBuffer(765),
    createStringBuffer("home.vaala.tech"),
    createUnsignedShortBuffer(25565),
    createVarIntBuffer(1),
  ]
  let b = b |> createPacket
  let _ = await (b |> writer)

  let b = [createVarIntBuffer(0)]
  let b = b |> createPacket
  let _ = await (b |> writer)

  let {value} = await reader
  let response =
    value
    |> removeVarIntHead
    |> Array.sliceToEnd(_, ~start=1)
    |> removeVarIntHead
    |> Buffer.fromArray
  Worker.createResponse(Buffer.toStringWithEncoding(response, StringEncoding.utf8))
}
