#!/bin/sh

# http://stackoverflow.com/questions/11949764/how-to-start-a-scala-method-from-command-line
scala -cp ~/Development/datascience/target/classes -e 'datascience.Perceptron.main(null)' | gnuplot ./script.gp && geeqie ./test.png  
