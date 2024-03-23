.PHONY : clean distclean

all : buildghc

parser :
	bnfc -d -o src src/Javalette.cf
	happy -gcai src/Javalette/Par.y
	alex --ghc src/Javalette/Lex.x

buildcabal : parser
# temporary solution
	cd src && cabal build && cp dist-newstyle/build/x86_64-linux/ghc-9.4.8/javalette-0.1/x/jlc/build/jlc/jlc ../

debugcabal : parser
	cd src && cabal build && cp dist-newstyle/build/x86_64-linux/ghc-9.4.8/javalette-0.1/x/debug/build/debug/debug ../

buildghc : parser
	cd src && ghc --make app/Main.hs -o ../jlc

#run :
#	./jlc "$(cat src/tmp/helloworld.jl)"

clean :
	-rm -f src/Javalette/*.log src/Javalette/*.aux src/Javalette/*.hi src/Javalette/*.o src/Javalette/*.dvi *.hi *.o

distclean : clean
	-rm -f src/Javalette/Doc.* src/Javalette/Lex.* src/Javalette/Par.* src/Javalette/Layout.* src/Javalette/Skel.* src/Javalette/Print.* src/Javalette/Test.* src/Javalette/Abs.* src/Javalette/Test src/Javalette/ErrM.* src/Javalette/SharedString.* src/Javalette/ComposOp.* src/Javalette/src/Javalette.dtd src/Javalette/XML.* src/Javalette/*.bak
	-rmdir -p src/Javalette/
	-rm -rf src/dist-newstyle
	-rm -f src/*.o src/*.hi
	-rm -f src/app/*.o src/*.hi
	-rm -f jlc debug
