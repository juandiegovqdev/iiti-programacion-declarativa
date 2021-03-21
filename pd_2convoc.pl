% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PROGRAMACIÓN DECLARATIVA. 3º GRADO TECNOLOGÍAS INFORMÁTICAS
% SEGUNDA CONVOCATORIA  CURSO 2013-14           
% 4 DE SEPTIEMBRE DE 2014
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NOMBRE Y APELLIDOS:
% E-MAIL:
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ANTES DE EMPEZAR, RENOMBRA ESTE ARCHIVO, uvus_.pl
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EJERCICIO 1.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Definir una relación esPermutacion(+L1, +L2) tal que, dadas dos
%% listas, determine si una es permutación de la otra. Por ejemplo:

%% ?- esPermutacion([3,2,5,1,7],[1,2,3,5,7]).
%% true      
%% ?- esPermutacion([],[]).
%% true 
%% ?- esPermutacion([5,7],[2,5,7]).
%% false 

%% SOLUCIÓN:


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EJERCICIO 2.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Definir la relaión entre(+N,+M, ?L) tal que, dados dos números
%% enteros N y M, es cierta si L es la lista con los números de N a M.
%%

%% ?- entre(5, 7, L).
%% L = [5, 6, 7]
%% ?- entre(5, 2, L).
%% L = []

%% SOLUCIÓN:

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EJERCICIO 3.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Se considera el siguiente programa lógico:

p([X|A]) :- q(X),p(A).   %R1
p([[X]|B]) :- r(X),p(B). %R2
p([X|A]) :- p(A).        %R3
p([]).                   %R4
q(a).                    %R5
r(b).                    %R6

%% Indicar todas las listas de dos elementos que verifican la relación
%% p. Escribir el árbol de resolución necesario para obtenerlas.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
