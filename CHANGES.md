
## 0.8

- handle sigint/sigterm
- reify exceptions from kernel with `Run_fail`
- add `deinit` to kernel, called before exiting
- replace ISO8601 with ptime (and rfc3339 printing)

## 0.7

- upgrade to more recent versions of zmq/zmq-lwt

## 0.6

- use `logs` for logging, deprecate `Log` functions

## 0.5

- address deprecation warning
- update to yojson 1.6
- remove all uses of lwt-ppx

