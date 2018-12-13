# Distributed Process Set

This example implements a basic distributed
process set where processes have identifiers and get
notifications when a duplicate is found. How such
processes react to duplicates is out of scope.

The API consists of 4 operations:

 * `ra_process_set:add/3`
 * `ra_process_set:remove/2`
 * `ra_process_set:member/2` for node-local membership checks
 * `ra_process_set:consensus_member/2` for cluster-wide membership checks

## Compile

``` bash
rebar3 compile
```
