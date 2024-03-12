#! /bin/bash


result=$(./lab4)
if echo $result | grep -q "FAIL"; then
	echo "$result"
	echo "Lab4 failed some tests, check output"
	(exit 1)
else
	echo "$result"
	echo "Lab4 passed tests!"
	(exit 0)
fi
