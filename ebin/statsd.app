{application, statsd, [
  {description, "An Erlang client for statsd"},
  {vsn, "0.0.1"},
  {modules,
    [ statsd
	  ]
	},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {statsd, ["localhost", 8125]}},
  {env, []}
]}.