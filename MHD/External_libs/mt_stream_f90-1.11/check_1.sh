#!/bin/sh

echo "check_stream_1 : consistency check #1."
echo "check_stream_1 : checking multiple stream consistency with small jump."
./check_stream_1 > $1
diff $1 sample_1.out
if [ $? -ne 0 ]; then
  echo "check_stream_1 : Check Error."
  exit -1
else
  echo "check_stream_1 : Check OK."
  exit 0
fi

