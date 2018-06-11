#!/usr/bin/python3

# Pings Google's public DNS server (8.8.8.8) every 30 seconds.
# Prints timestamp (in ISO 8601 format) and status (0 or 1) to stdout:
#   yyyymmddThhmmss,1
# Repeats this until interrupted with ^c.

import time
from os import system
SUCCESS = 1
FAILURE = 0

GOOGLE_PUBLIC_DNS = "8.8.8.8"

def ping_server(address):
  result = system("ping -c 1 -i 0.5 -q " + GOOGLE_PUBLIC_DNS + " > /dev/null")
  if result == 0: # successful exit code for ping
    return SUCCESS
  else:
    return FAILURE

def time_string():
  return time.strftime("%Y%m%dT%H%M%S")

def main():
  while True:
    result = ping_server(GOOGLE_PUBLIC_DNS)
    print("{},{}".format(time_string(), result))
    time.sleep(30)
  return

if __name__=="__main__":
  main()
