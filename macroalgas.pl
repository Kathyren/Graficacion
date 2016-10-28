:- dynamic (conocido/4).
:- dynamic (actual/1).

nombreComun(X):- 
	actual(CF),
	retractall(actual(_)),
	writef('Nombre comun: %w \n con un CF de: %w',[X,CF]).
filo(chlorophyta):-
	(
		((color(X),!, X==verde, certeza(color,verde,CF), CF>90);
		(certeza(color,verde,CF), CF>40, certeza(color,amarillento,CF), CF>40)),
		(almidon(intraplastidial), !)
	).
filo(rhodophyta):-
	(color(X),!, X==rojo, certeza(color,rojo,CF), CF>90),
	(almidon(extraplastidial),!).
filo(phaeophyta):-
	(color(X), member(X,[pardo, verde, cafe, amarillento, marron_oscuro])),
	(almidon(nulo),!).

genero(chaetomorpha):-
	filo(chlorophyta),
	contiene(filamentos),!.
genero(codium):-
	filo(chlorophyta),
	talos(esponjosos),
	(talos_no(laminados)).
genero(ulva):-
	filo(chlorophyta),
	talos(laminados),
	base(laminada),
	estructura(distromatica).
genero(enteromorpha):-
	filo(chlorophyta),
	talos(laminados),
	base(tubular),
	estructura(monostromatica).
genero(sargassum):-
	filo(phaeophyta),
	tiene(aerocistos),
	(
		(aerocistos(esfericos));
		(tamAerocistos,	aerocistos(ejes_lamina))
	).
genero(macrocystis):-
	filo(phaeophyta),
	tiene(aerocistos),
	(aerocistos(elipsoidales);(granAerocistos,
	aerocistos(ejes_lamina))).
genero(petalonia):-
	filo(phaeophyta),
	no_tiene(aerocistos),
	(aerocistos(elipsoidales);(granAerocistos,
	aerocistos(ejes_lamina))).
genero(eisenia):-
	filo(phaeophyta),
	talos_no(laminados),
	tiene(pie_de_fijacion_fornido),
	ramificado(abundante),
	ramificado(compactado),
	ramificado(dividido_dos).
genero(padina):-
	filo(phaeophyta),
	no_tiene(pie_de_fijacion_fornido),
	adhiere_sustrato(masa_rizoides),
	tiene(laminas),
	laminas(disco),
	laminas(avanico),
	apices(enrrollados).
genero(dyctyopteris):-
	filo(rhodophyta),
	laminas(nervadura_media),
	ramificado(irregular),
	ramificado(dividido_dos).
genero(dictyota):-
	filo(rhodophyta),
	laminas_no(nervadura_media),
	ramificado(dividido_dos).
genero(jania):-
	filo(rhodophyta),
	talos(impregnado_CaCO3),
	ramificado(dividido_dos),
	ramificado(erecto),
	ramificado(segmentado).
genero(ceramium):-
	filo(rhodophyta),
	talos_no(impregnado_CaCO3),
	talos(filamentoso),
	filamentos(polisifonicos),
	ramificado(dividido_dos),
	apices(forcipiformes),
	(talos(numerosas_poliferaciones); talos(vallosidades)).
genero(centroceras):-
	filo(rhodophyta),
	talos_no(impregnado_CaCO3),
	talos(filamentoso),
	filamentos(polisifonicos),
	ramificado(dividido_dos),
	apices(forcipiformes),
	talos(espinas).
genero(porphyra):-
	filo(rhodophyta),
	talos_no(filamentoso),
	talos(laminados),
	laminas(estructura_monostromatica).
genero(laurencia):-
	filo(rhodophyta),
	talos(polistromatico),
	ramificado(cilindricas),%certeza
	talos(uniaxial),
	talos_no(filamentos_rizoidales),
	talos(apicales_huecos).
genero(gelidiella):-
	filo(rhodophyta),
	talos(polistromatico),
	ramificado(cilindricas),%certeza
	talos(uniaxial),
	talos_no(filamentos_rizoidales),
	talos_no(apicales_huecos),
	ramificado(pinnada_todas_direcciones).
genero(hypnea):-
	filo(rhodophyta),
	talos(polistromatico),
	ramificado(cilindricas),%certeza
	ramificado(aplanado_como_cintas),
	talos(uniaxial),
	talos_no(filamentos_rizoidales),
	talos_no(apicales_huecos).
genero(gymnogomgrus):-
	filo(rhodophyta),
	ramificado(dividido_dos),
	talos(polistromatico),
	ramificado(aplanado_como_cintas),
	ramificado(abundante),
	ramificado(espinas).
genero(gracilaria):-
	filo(rhodophyta),
	ramificado_no(dividido_dos),
	talos(polistromatico),
	ramificado(aplanado_como_cintas),
	frondas(aplanadas),
	filamentos_no(centrales).
genero(gigartina):-
	filo(rhodophyta),
	talos(polistromatico),
	ramificado(aplanado_como_cintas),
	ramificado_no(dividido_dos),
	frondas(aplanadas),
	tiene(espinas),
	estructura(telarana).

genero(genero_no_especificado).
%especie(plantita):- nombreComun(alguita).
especie(codium_cuneatum):-
	genero(codium),
	talos(abanico),
	ramificado(inicia_base), certeza(ramificado,inicia_base,CF), CF>20, CF<70,
	ramificado(dividido_dos), certeza(ramificado,dividido_dos,CF), CF>60,
	uso(comida),
	nombreComun(codio_coneado).
especie(codium_fragile):-
	genero(codium),
	talos(erguido),
	%colorVerde(X), X==oscuro,
	ramificado(erecto),
	ramificado(inicia_base), certeza(ramificado,inicia_base,X), X>60,
	talos(sifones_diferenciados),
	uso(comida),
	nombreComun(codio_fragil).
especie(codium_simulans):-
	genero(codium),
	frondas(cilindricas),
	frondas(en_cuna),
	frondas(aplanadas),
	(ramificado(dividido_dos);ramificado(unilaterales)),
	uso(comida),
	nombreComun(codio_falso).
especie(chaetomorpha_linum):-
	genero(chaetomorpha),
	((colorVerde(X), member(X, [amarillento, brillante, pasto, olvia]))),
	filamentos(erguido),
	filamentos(aderidos_sustrato),
	uso(comida),
	nombreComun(pelo_verde).
especie(enteromorpha_clathrata):-
	genero(enteromorpha),
	(colorVerde(X), member(X, [amarillento, brillante, pasto, olvia])),
	ramificado(repentino),
	ramificado(cabellos_enrredados),
	uso(comida),
	nombreComun(pelo_de_piedra_verde_claro).
especie(enteromorpha_compressa):-
	genero(enteromorpha),
	talos(tubular),
	talos(adelgazados_en_base),
	uso(comida),
	nombreComun(ova_chata).
especie(enteromorpha_intestinalis):-
	genero(enteromorpha),
	((colorVerde(X),  member(X, [amarillento, brillante, pasto, transparente]))),
	(talos(tubular);talos(cilindricas);talos(aplanadas)),
	uso(comida),
	nombreComun(pelo_de_piedra_verde).
especie(enteromorpha_linza):-
	genero(enteromorpha),
	talos(sedoso),
	talos(simple),
	apicales(aplanadas),
	tiene(cavidades),
	uso(antimicrobiano),
	nombreComun(ova_lechuga).
especie(ulva_lactuca):-
	genero(ulva),
	adhiere_sustrato(organo_fijacion),
	((colorVerde(X), member(X, [amarillento, brillante, pasto, olvia]))),
	uso(comida),
	nombreComun(lecuga_de_mar).
especie(dyctyopteris_undulata):-
	genero(dyctyopteris),
	talos(irregular),
	(color(cafe); (colorVerde(X),  member(X, [oscuro, pasto, olvia]))),
	frondas(planas),
	margenes(ondulados),
	nombreComun(abanico_olivo).
especie(dyctyopteris_dichotoma):-
	genero(dyctyopteris),
	talos(listones),
	talos(erguido),
	ramificado(dividido_dos),
	ramificado(encima_porcion_basal),
	adhiere_sustrato(masa_rizoides).
especie(dictyota_flabellata):-
	genero(dictyota),
	color(cafe),
	(ramificado(dividido_dos); ramificado(pinnada)),
	apices(redondeados),
	margenes_no(dentados),
	tiene(numerosas_poliferaciones),
	nombreComun(abanico_moreno).
especie(petalonia_fascia):-
	genero(petalonia),
	(color(cafe); (colorVerde(X),  member(X, [oscuro, pasto, olvia]))),
	adhiere_sustrato(disco),
	laminas(erecto),
	laminas(delgadas),
	tiene(cabellos),
	nombreComun(petalo_oliva).
especie(_):-
	genero(F),
	writef('No tengo especies `9 con esas caracteristicas, pero es del genero es %w',[F]).
%faltan de la pagina 37 en adelante (como 3)
peqAerocistos(X):-
	X<9.
tamAerocistos:-
	write('Tamaño de los aerocistos en mm: '),
	read(X), peqAerocistos(X).

granAerocistos:- tamAerocistos, !, fail.

auxiliar(Y):-
	actual(X),
	calcular(X,Y,CF),
	retractall(actual(_)),
	asserta(actual(CF)).



talos(X) :- preguntaM(talos,X),!, certeza(talos,X,CF), auxiliar(CF).
tiene(X) :- preguntaM(tiene,X),!, certeza(tiene,X,CF), auxiliar(CF).
ramificado(X) :- preguntaM(ramificado,X),!, certeza(ramificado,X,CF), auxiliar(CF).
laminas(X) :- preguntaM(laminas,X),!, certeza(laminas,X,CF), auxiliar(CF).
filamentos(X) :- preguntaM(filamentos,X),!, certeza(filamentos,X,CF), auxiliar(CF).
apicales(X) :- pregunta(apicales,X),!, certeza(apicales,X,CF), auxiliar(CF).

talos_no(X):- talos(X),!, fail.
talos_no(X) :- preguntaN(talos,X),!, certeza(talos,X,CF), auxiliar(-CF).
no_tiene(X):- tiene(X), !, fail.
no_tiene(X) :- preguntaN(tiene,X),!, certeza(tiene,X,CF), auxiliar(-CF).
no_contiene(X) :- contiene(X),!, fail.
no_contiene(X) :-preguntaN(contiene,X),!, certeza(contiene,X,CF), auxiliar(-CF).
laminas_no(X) :- preguntaN(laminas,X),!, certeza(laminas,X,CF), auxiliar(-CF).
filamentos_no(X) :- preguntaN(filamentos,X),!, certeza(filamentos,X,CF), auxiliar(-CF).
ramificado_no(X) :- preguntaN(ramificado,X),!, certeza(ramificado,X,CF), auxiliar(-CF).
margenes_no(X) :- preguntaN(margenes,X),!, certeza(margenes,X,CF), auxiliar(-CF).

almidon(X) :- pregunta(almidon,X),!, certeza(almidon,X,CF), auxiliar(CF).
contiene(X) :- pregunta(contiene,X),!, certeza(contiene,X,CF), auxiliar(CF). 
base(X) :- pregunta(base,X),!, certeza(base,X,CF), auxiliar(CF).
estructura(X) :- pregunta(estructura,X),!, certeza(estructura,X,CF), auxiliar(CF).
aerocistos(X) :- pregunta(aerocistos,X),!, certeza(aerocistos,X,CF), auxiliar(CF).
adhiere_sustrato(X) :- pregunta(adhiere_sustrato,X),!, certeza(adhiere_sustrato,X,CF), auxiliar(CF).
apices(X) :- pregunta(apices,X),!, certeza(apices,X,CF), auxiliar(CF).
frondas(X) :- pregunta(frondas,X),!, certeza(frondas,X,CF), auxiliar(CF).
margenes(X) :- pregunta(margenes,X),!, certeza(margenes,X,CF), auxiliar(CF).



uso(X):- writef('su uso es para %w',[X]).



colorVerde(X):-!, color(verde), menu_pregunta(tono_verde, X, [transparente ,olvia, amarillento, oscuro, pasto, amarillento,brillante], _),!.
color(X):-!, menu_pregunta( color , X, [verde, rojo, pardo, amarillento, marron_oscuro ], _).
certeza(A,V,CF):- conocido(_,A,V,CF).



%Preguntas que solo pueden tener un valor
pregunta(A,V) :- conocido(si,A,V,_).
pregunta(A,V) :- conocido(na,A,V,CF), CF>=0.
pregunta(A,V) :- conocido(na,A,V,_), !, fail.
pregunta(A,_) :- conocido(si,A,_,_), !, fail.
pregunta(A,V) :- conocido(no,A,V,_), !, fail.
pregunta(A,V) :- 
	 write(A:V),
	 write('? : '),
	 read(Resp),
	 (
	 	( Resp==no, asserta(conocido(no,A,V,-100)));
		( Resp==si, asserta(conocido(Resp,A,V,100)));
		( Resp==no_se, especificaCerteza(A,V) )

	 ), 
	 (Resp == si; Resp==no_se).
especificaCerteza(A,V):-
	 write('en escala del 0 al 100, \n donde 0 es definitavemnte no, \n 50 punto medio y 100 definitavemnte si \n'),
	 read(C),
	 calcular(C,CF),
	 asserta(conocido(na,A,V,CF)).
calcular(C,CF):-
	CF is (2*C -100).
calcular(X,Y,CF):-
	(
		(X>=0, Y>0,
			CF is (X+Y*(100-X)/100)
			);
		(X<0, Y<0,
			calcular((-X),(-Y),CF));
		(
			absolutoMinimo(X,Y,M),
			CF is (X+Y/(1-M)))
	).
absolutoMinimo(X,Y,M) :-
	X1 is (X^2),
	Y1 is (Y^2),
	X1<Y1,
	M=X.
absolutoMinimo(X,Y,M) :-
	X1 is (X^2),
	Y1 is (Y^2),
	Y1<X1,
	M=Y.


%Preguntas que pueden tener mas de un valor
preguntaM(A,V) :- conocido(si,A,V,_).
preguntaM(A,V) :- conocido(na,A,V,CF), CF>=0.
preguntaM(A,V) :- conocido(na,A,V,_), !, fail.
preguntaM(A,V) :- conocido(no,A,V,_), !, fail.
preguntaM(A,V) :- 
	 write(A:V),
	 write('? : '),
	 read(Resp),
	 (
	 	(Resp== no, asserta(conocido(no,A,V,-100)));
		(Resp== si, asserta(conocido(si,A,V,100)));
		(Resp== no_se, especificaCerteza(A,V) )

	 ),
	 (Resp == si; Resp==no_se).


%Preguntas Negada 
preguntaN(A,V) :- conocido(no,A,V,_).
preguntaN(A,V) :- conocido(si,A,V,_), !, fail.
preguntaN(A,V) :- 
	 write(A:V),
	 write('? : '),nl,
	 read(Resp),
	(
	 	(Resp==  no, asserta(conocido(no,A,V,-100)));
		( Resp==si, asserta(conocido(si,A,V,100)));
		(Resp==no_se, especificaCerteza(A,V) )

	 ),	 
	(Resp == no; Resp==no_se).


menu_pregunta(A,V,_, _) :- conocido(si,A,V,_),!.
menu_pregunta(A,V,_, _) :- conocido(no,A,V,_), !, fail. %MODIFICar
menu_pregunta(A,V,_,_) :-  conocido(_, A, V, CF), CF>0.
menu_pregunta(A,V,_,_) :-  conocido(_, A, V, _),!, fail.
menu_pregunta(A,_,_, _) :- conocido(si,A,_,_),!,fail.
menu_pregunta(A,V,MenuLista, _):- 
 		write('Cual es el valor para '),  
 		write(A), 
 		write('? (escriba "otro" si no es ninguno y "no_se" si no esta seguro)'), nl,  
 		write(MenuLista),nl, 
 		read(V),!,
 		checar(V,MenuLista),
 		(
 			(V==otro, asserta(conocido(no,A,_, -100) ))
 			;

 			asserta(conocido(si,A,V, 100))
 		).
 menu_pregunta(A,V,MenuLista, _):- 
 		write('Por favor, escoja una de las opciones presentadas.'),nl,
 		menu_pregunta(A,V,MenuLista, _).


member(X,[X|_]).
member(E, [_|Y]) :- member(E,Y).


checar(otro,_). 
checar(X,MenuLista) :-
 		 member(X,MenuLista), !. 



reiniciar:-retractall(conocido(_,_,_,_)),retractall(conocido(_,_,_)).


clasificar(X):- reiniciar, asserta(actual(50)), especie(X).