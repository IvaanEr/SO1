#!/bin/bash
epmd -daemon
erl -name luigi@192.168.0.14 -setcookie cookie #| echo c\(server\)\.
