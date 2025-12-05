all: day1 day2

day1: d1
	./day1/d1 day1/input.txt

d1:
	ghc -o day1/d1 day1/d1.hs

day2: d2
	./day2/d2 day2/input.txt

d2:
	ghc -o day2/d2 day2/main

