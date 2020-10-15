MODULE blocoType
    USE postype
    type bloco
        !o bloco status devine o estado do bloco
        !diremos 0: se estiver morto
        !diremos 1: se for recem nacido
        !diremos 2: se for adulto
        integer*1 estado
        type(pos)  visinhos(1:3,1:3)
    endtype bloco
END MODULE