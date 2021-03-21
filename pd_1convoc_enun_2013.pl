%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PROGRAMACIÓN DECLARATIVA. 3º GRADO TECNOLOGÍAS INFORMÁTICAS
%% PRIMERA CONVOCATORIA  CURSO 2013-14           
%% 29 ENERO 2014
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NOMBRE Y APELLIDOS:
%% E-MAIL:
%% UVUS:
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Antes de empezar, renombra este archivo: uvus_feb.pl

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EJERCICIO 1 (0.75 ptos)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Definir una relación esSuma(+N, +L) tal que, dados un número N y una
%% lista de números L, determine si N se puede obtener sumando algunos
%% elementos de L. Por ejemplo:

%% ?- esSuma(13,[1,2,3,5,7]).
%% true .       
%% ?- esSuma(13,[2,5,6,7]).
%% true .
%% Ya que 13 = 1+2+3+7
%% ?- esSuma(13,[2,5,7]).
%% false .


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EJERCICIO 2. (0.75 ptos)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Definir la relación menores(+N,?L) tal que, dado N, L es la lista
%% con los números de 1 a N-1.



% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EJERCICIO 3. (0.5 ptos).
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Calcular el árbol de decisión para la siguiente base de
%% conocimiento y la consulta s(X).

q(X) :- p(X), r(X).
q(X) :- t(X).
s(X) :- q(X).
s(c).
p(a).
p(b).
p(c).
r(b).
r(c).
t(a).
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
