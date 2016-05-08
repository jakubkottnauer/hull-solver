#!/bin/bash
echo `gnuplot -e "f='conform1'" narrowings.gp`
echo `gnuplot -e "f='conform1'" time.gp`
echo `gnuplot -e "f='mickey'" narrowings.gp`
echo `gnuplot -e "f='mickey'" time.gp`
echo `gnuplot -e "f='minus'" narrowings.gp`
echo `gnuplot -e "f='minus'" time.gp`
echo `gnuplot -e "f='polyn1'" narrowings.gp`
echo `gnuplot -e "f='polyn1'" time.gp`
echo `gnuplot -e "f='polyn2'" narrowings.gp`
echo `gnuplot -e "f='polyn2'" time.gp`
echo `gnuplot -e "f='quadfor2'" narrowings.gp`
echo `gnuplot -e "f='quadfor2'" time.gp`
echo `gnuplot -e "f='solotarev'" narrowings.gp`
echo `gnuplot -e "f='solotarev'" time.gp`
echo `gnuplot -e "f='wright'" narrowings.gp`
echo `gnuplot -e "f='wright'" time.gp`
