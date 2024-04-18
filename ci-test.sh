#! /bin/bash


result=$(make lab7_test)
if echo $result | grep -q "FAIL"; then
	echo "$result"
	echo "Lab7 failed some tests, check output"
	(exit 1)
else
	echo "$result"
	echo "Lab7 passed tests!"
	(exit 0)
fi
