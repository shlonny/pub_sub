# pub_sub
Simple app for testing data retrieval using coordinated gen_servers and gen_event.

The following should manifest one outgoing call to jsonip.com rather than 10.

test_harness:go(10, "http://jsonip.com").
