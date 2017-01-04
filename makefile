app:
	ghc Main.hs -o Brainhask

test:
	make test_inner -s

test_inner:
	cat HelloWorld.bf | ./Brainhask -i
	echo ""
	cat HelloWorld.bf | ./Brainhask > test.c
	gcc test.c -o test
	./test
	rm test.c test

clean: 
	rm *.o *.hi
