annex-marshal-msgpack
=====================

msgpack marshalizer for annex

Spec
----

### Request

A request sends the following information:

```
[type, msgid, module, function, arguments]
```

Example messages

```
[0, 1234, "math", "pow", [2, 2]]
```

```
[1, 5678, "analytics", "event", ["item_purchase", "user123", "item123"]]
```

#### types

##### type

Type is an integer 'enum'.

* `0` means `call`, which expects a response
* `1` means `cast`, which is a fire and forget

##### msgid

Opaque 32-bit integer for keeping track of request/response cycles. Mainly used for `call` requests but could also be used for logging.

##### module, function

Names of the `module:function` on the server to be called

##### arguments

An array of arguments to be passed to the request

### Response

A response contains the following information:

```
[type, msgid, result]
```

Example messages:

```
[0, 1234, 4]
```

```
[1, 5678, "the method 'analytics:event/3' is undefined"]
```

#### types

##### type

Type is an integer 'enum'.

* `0` means `success`
* `1` means `error`

##### msgid

see above.

##### result

Arbitrary msgpack value containing the response to the request.
