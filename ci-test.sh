#! /bin/bash


result=$(./lab3)
if echo $result | grep -q "FAIL"; then
	echo "$result"
	echo "Lab2 failed some tests, check output"
	(exit 1)
else
	echo "Lab2 passed tests!"
	(exit 0)
fi
