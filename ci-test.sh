#! /bin/bash


result=$(./lab5)
if echo $result | grep -q "FAIL"; then
	echo "$result"
	echo "Lab5 failed some tests, check output"
	(exit 1)
else
	echo "$result"
	echo "Lab5 passed tests!"
	(exit 0)
fi
