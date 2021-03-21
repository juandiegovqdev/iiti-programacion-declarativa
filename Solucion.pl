primo(2). primo(3). primo(5). primo(7). primo(11). primo(13). primo(17).
primo(19). primo(23). primo(29). primo(31). primo(37).


cuatrinos(5, [5]).
cuatrinos(X, [X|LY]) :- X > 5, esCuatrino(X), Y is X-1, cuatrinos(Y, LY).
cuatrinos(X, L) :- X > 5, Y is X-1, cuatrinos(Y, L).

esCuatrino(X) :- primo(X). 