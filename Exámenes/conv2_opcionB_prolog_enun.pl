
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%%    Programacion Declarativa                  	2013/2014
%%    SEGUNDA CONVOCATORIA                              8/09/2014
%% 
%% 
%%	Nombre ...:_________________________________________________ 
%% 
%%	DNI ......:_________________________________________________ 
%% 
%%	Email (US):_________________________________________________ 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  
%%   Para la correccion se tendra en cuenta la estructuracion del codigo, 
%%   la realizacion de comentarios cuando se consideren necesarios para 
%%   comprender la logica del algoritmo, la reduccion del codigo a la  
%%   funcionalidad estrictamente necesaria, la capacidad de control de 
%%   las funciones de valores incorrectos.
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ejercicio 6. (1 punto) 

%% Se considera el programa lógico:

p([X|A],[X|B]) :- q(X),p(A,B).   %R1
p([X|A],[[X]|B]) :- r(X),p(A,B). %R2
p([X|A],B) :- p(A,B).            %R3
p([],[]).                        %R4
q(a).                            %R5
r(b).                            %R6

%% Escribe el árbol de resolución y todas las respuestas obtenidas para el
%% programa anterior y la pregunta ?- p([a,b],L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Ejercicio 7. (1 punto) 
%%    Construir un predicado en Prolog denominado empareja que 
%%    reciba tres listas. El predicado obtendrá un valor verdadero 
%%    cuando el tercer argumento sea el resultado de emparejar 
%%    los elementos de las dos primeras listas. 
%% 
%%    Por ejemplo:  
%% ?- empareja([a,b],[1,2],[(a,1),(b,2)]). 
%% true . 
%%  
%% ?- empareja([a,b],[1,2],[(a,2),(b,1)]). 
%% false. 
%% 
%%    También se podrá invocar con los argumentos parcialmente instanciados, 
%%    y deberá ignorar los elementos de las listas iniciales cuando el tamaño 
%%    sea diferente: 
%% 
%%  
%% ?- empareja([a,b],[1,2,3,4],P). 
%% P = [ (a, 1), (b, 2)] . 
%%  
%% ?- empareja([a,b],X,[(a,1),(b,3)]). 
%% X = [1, 3] . 
%%  
%% ?- empareja(X,Y,[(a,1),(b,2)]). 
%% X = [a, b], 
%% Y = [1, 2] . 
%%
%% SOLUCIÓN: 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 

