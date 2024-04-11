#! /bin/bash


result=$(make lab6_test)
if echo $result | grep -q "FAIL"; then
	echo "$result"
	echo "Lab6 failed some tests, check output"
	(exit 1)
else
	echo "$result"
	echo "Lab6 passed tests!"
	(exit 0)
fi
