# statsd-erlang

an Erlang client for statsd

# Usage

```erlang
Statsd = statsd:start("localhost", 8125).
stats:increment(Statsd, "foo.bar").
stats:count(Statsd, "foo.bar", 5).
stats:timing(Statsd, "foo.bar", 2500).
```

# Development

To compile the source, run `erl -make` from the root directory.