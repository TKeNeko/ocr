# OCR
 
OCAML=ocamlopt
OCAMLFLAGS= -I +sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
ML= pretreatment.ml neuron.ml ocr.ml
 
ocr: ${ML}
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o ocr ${ML}
 
clean::
	rm -f *~ *.o *.cm? ocr inProgress
 
# FIN
