all: day1 day2 day3

day1: d1
	./day1/d1 day1/input.txt

d1:
	ghc -o day1/d1 day1/d1.hs

day2: d2
	./day2/d2 day2/input.txt

d2:
	ghc -o day2/d2 day2/main

day3: d3
	./day3/d3 day3/input.txt

d3:
	ghc -o day3/d3 day3/main

day4: d4
	./day4/d4 day4/input.txt

d4:
	ghc -o day4/d4 day4/main.hs
