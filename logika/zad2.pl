rybki(Kto):-
    % Norweg zamieszkuje pierwszy dom. Norweg mieszka obok niebieskiego domu. Mieszkaniec srodkowego domu pija mleko.
	Domy=[[norweg,_,_,_,_],[_,niebieski,_,_,_],[_,_,mleko,_,_],_,_],

    % Anglik mieszka w czerwonym domu.
	member([anglik,czerwony,_,_,_],Domy),

    % Dunczyk pija herbatke.
    member([dunczyk,_,herbata,_,_],Domy),
	
    % Mieszkaniec zoltego domu pali cygara.
    member([_,zolty,_,cygara,_],Domy),

    % Niemiec pali fajke.
    member([niemiec,_,_,fajka,_],Domy),

    % Palacz papierosow bez filtra hoduje ptaki.
    member([_,_,_,bez_filtra,ptaki],Domy),

    % Szwed hoduje psy.
    member([szwed,_,_,_,psy],Domy),

    % Palacz mentolowych pija piwo.
    member([_,_,piwo,mentolowe,_],Domy),

    % Zielony dom znajduje się bezposrednio po lewej stronie domu bialego. W zielonym domu pija sie kawe.
    lewo([_,zielony,kawa,_,_],[_,bialy,_,_,_],Domy),
	
	% Palacz papierosow light mieszka obok hodowcy kotow.
    obok([_,_,_,light,_],[_,_,_,_,koty],Domy),
	
	% Palacz papierosów light ma sasiada, ktory pija wode.
	obok([_,_,_,light,_],[_,_,woda,_,_],Domy),
	
	% Hodowca koni mieszka obok zoltego domu.
	obok([_,_,_,_,konie],[_,zolty,_,_,_],Domy),
	
	% kto hoduje rybki?
    member([Kto,_,_,_,rybki],Domy).

lewo(X,Y,[X,Y|_]).
lewo(X,Y,[_|T]) :- lewo(X,Y,T).

obok(X,Y,Domy):-
	lewo(X,Y,Domy);
	lewo(Y,X,Domy).