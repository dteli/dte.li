#!/usr/bin/sh

parallel 'convert {} -resize 3000x3000 {.}r1.JPG' ::: *.JPG 
