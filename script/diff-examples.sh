#!/bin/bash

#
# Compare the outputs generated by two runs of <xxx>-run-examples.sh
#

DIR1=/Users/olli/cl-synthesizer-sbcl
DIR2=/Users/olli/cl-synthesizer-abcl

function diff2 {
    diff -q -s $DIR1/$1 $DIR2/$1
}

diff2 vco-example-2.wav
diff2 vco-example-2.wav
diff2 vco-example-2.csv
diff2 midi-sequencer-example-1.wav
diff2 monitor-example-1.wav
diff2 monitor-example-1.csv
diff2 vca-example-1.wav
diff2 vca-example-2.wav
diff2 vco-example-1.wav
diff2 vco-example-4.csv
diff2 adsr-example-1.csv
diff2 adsr-example-2.csv
diff2 adsr-example-3.csv
diff2 ramp-example-1.csv
diff2 sustain-example-1.csv

