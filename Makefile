# OCR
 
OCAML=ocamlopt
OCAMLFLAGS= -I +sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
 
ocr: ocr.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o ocr ocr.ml
 
clean::
	rm -f *~ *.o *.cm? ocr
 
# FIN
