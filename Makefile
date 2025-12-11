all: day1 day2 day3 day4 day5 day6 day7 day8

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

day5: d5
	./day5/d5 day5/input.txt

d5:
	ghc -o day5/d5 day5/main.hs

day6: d6
	./day6/d6 day6/input.txt

d6:
	ghc -o day6/d6 day6/main.hs

day7: d7
	./day7/d7 day7/input.txt

d7:
	ghc -o day7/d7 day7/main.hs

day8: d8
	./day8/d8 day8/input.txt

d8:
	ghc -o day8/d8 day8/main.hs
