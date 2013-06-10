# Welcome use riak-http #
**riak-http** is a middle ware for access **riak** via **http**,it based on **cowboy rest ful** and **gen_server process group**.

##Quick Start##
This section assumes that you have copy of riak-http source tree. To get started, you need to:

1、Build riak-http

2、Start riak-http server

3、test server via client

###Building riak-http###
Assuming you have a working Erlang (R14B02 or later) and riak database installation, building riak-http should be as simple as:
    
    $ ./rebar get-deps
    $ ./rebar compile
###Starting riak-http###
Once you have successfully built riak-http, you can start the server with the following commands:

    $ erl -pa apps/riak_http/ebin/ deps/*/ebin/
    Erlang R15B02 (erts-5.9.2) [source] [async-threads:0] [hipe] [kernel-poll:false]

	Eshell V5.9.2  (abort with ^G)
	1>
    
    2> applicaiton:start(lager).
    3> pg2:start_link().
    4> applicaiton:start(ranch).
    5> applicaiton:start(crypto).
    6> applicaiton:start(cowboy).
    7> applicaiton:start(riak_http).

###Testing riak-http###
Now that you have a functional web server,let's try some testing via client.The test script in the dir of `riak_http_riak`.

    $ tree -L 1
    .
	├── apps
	├── Makefile
	├── rebar
	├── rebar.config
	└── riak_http_test
There are some files in the dir of `riak_http_test`,and you can ingore the *.beam and *.app files.

You can create a bucket via execute the script of `ut_db_service_op_bucket_create`.

    $ ./ut_db_service_op_bucket_create <bucketname you will create>

and it would return info contain `return code` and `return message`

You can list the buckets via execute the script of `ut_db_service_op_bucket_list` .

    $ ./ut_db_service_op_bucket_list

it would return info contain all buckets in the riak.

You can list the ketys in one given bucket via the script of `ut_db_service_op_key_list` .

    $ ./ut_db_service_op_key_list <one given bucketname>

The other operation can see in the dir of `riak_http_test`.