#!/bin/sh

echo "check_stream_2 : consistency check #2."
echo "check_stream_2 : checking multiple stream consistency with huge jump."

./check_stream_2 > $1
diff $1 sample_2.out
if [ $? -ne 0 ]; then
  echo "check_stream_2 : Check Error."
  exit -1
else
  echo "check_stream_2 : Check OK."
  exit 0
fi

