app:
	ghc Main.hs -o Brainhask

test:
	cat HelloWorld.bf | ./Brainhask
