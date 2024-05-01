.PHONY : clean

files=src/app/Main.hs src/Javalette.cf src/*.hs src/Javalette/*.hs src/LLVM/*.hs

jlc : $(files)
	cd src && ghc --make app/Main.hs -o ../jlc

debugjlc :
	cd src && ghc --make app/Debug.hs -o ../debugjlc

runllvm : llvm-out.ll runtime.ll
	opt -O3 llvm-out.ll > llvm-out-opt.bc
	llvm-as runtime.ll
	llvm-link llvm-out-opt.bc runtime.bc > llvm-out-linked.bc
	lli llvm-out-linked.bc

scratch : parser jlc

parser :
	bnfc -d -o src src/Javalette.cf
	happy -gcai src/Javalette/Par.y
	alex --ghc src/Javalette/Lex.x

buildcabal :
	cd src && cabal build

clean :
	-find src/Javalette/* -type f -not -name 'Par.hs' -not -name 'Lex.hs' -not -name 'Print.hs' -not -name 'Abs.hs' -delete
	-rm -rf src/dist-newstyle
	-rm -f src/*.o src/*.hi
	-rm -f src/app/*.o src/app/*.hi
	-rm -f src/LLVM/*.o src/LLVM/*.hi
	-rm -f lib/runtime.bc
	-rm -f jlc debugjlc
