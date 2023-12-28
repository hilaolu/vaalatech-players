type writableStreamDefaultWriter
type readableStreamDefaultReader

type readableStream
type writable

external read: (readableStream, array<int>) => promise<unit> = "read"

module WritableStreamDefaultWriter = {
  @send external write: (writableStreamDefaultWriter, NodeJs.Buffer.t) => promise<unit> = "write"
}

type readResult = {
  done: bool,
  value: array<int>,
}

module ReadableStreamDefaultReader = {
  @send external read: readableStreamDefaultReader => promise<readResult> = "read"
}

module WritableStream = {
  @send external getWriter: writable => writableStreamDefaultWriter = "getWriter"
}

type readable
module ReadableStream = {
  @send external getReader: readable => readableStreamDefaultReader = "getReader"
}

type socket = {
  readable: readable,
  writable: writable,
}

@module("cloudflare:sockets") external connect: string => socket = "connect"

type request
type response
@new external createResponse: string => response = "Response"
