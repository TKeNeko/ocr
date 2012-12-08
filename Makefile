# OCR
 
OCAML=ocamlopt
OCAMLFLAGS= -I +sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
ML= neuron.ml ocr.ml
 
ocr: ${ML}
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o ocr ${ML}
 
clean::
	rm -f *~ *.o *.cm? ocr
 
# FIN
