#! /usr/bin/env python2
T = int(raw_input())
for cas in range(T):
    a, b = map(int, raw_input().split())
    print 'Case #' + str(cas + 1) + ':', a + b
