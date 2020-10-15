program jogo_da_vida
    use blocotype
    use postype
    implicit none
    type(bloco) blocos(1:50,1:50)
    integer contagem,nE,nCiclo
    integer escala,winId,largBloco
    escala = 500
    nE =1
    nCiclo =0
    largBloco = escala/50
    !criar uma janela
    call gopen(escala,escala+100,winId)
    call gclr(winId)
    call gsetbgcolor(winId,'#99999f'//CHAR (0))
    !condições iniciais
    call inicializarBlocos(blocos)
    call definirVivos(blocos)
    print*,"e seu vizinho (1,3) é: ", blocos(50,3)%visinhos(3,2)%linha, blocos(50,3)%visinhos(3,2)%coluna
    !recursos de mundança de gereção
    do
        if(nE>0 .and. nCiclo<1000) then
            call pintarBlocos(blocos,largBloco,winId)
            call reproduzir(blocos)
            call selecaoNatural(blocos)
            call removerMortos(blocos)
            call tornarAdulto(blocos)
            nE = contagem(blocos)
            !print*,"existe ",nE," sobreviventes."
            call msleep(10)
            nCiclo = nCiclo+1
        else
            exit
        endif
    enddo
    print*,"o estado do bloco b(24,25) é: ", blocos(24,25)%estado
    print*,"e seu vizinho (1,1) é: ", blocos(1,50)%visinhos(1,3)%linha, blocos(1,50)%visinhos(1,3)%coluna
end program jogo_da_vida

subroutine inicializarBlocos(osblocos)
    use blocotype
    use postype
    implicit none
    type(bloco) :: osblocos(1:50,1:50)
    integer i,j,vi,vj
    do i=1,50
        do j=1,50
            osblocos(i,j)%estado = 0
            do vi = 1,3
                do vj =1,3
                    osblocos(i,j)%visinhos(vi,vj)= pos(i+vi-2,j+vj-2)
                    if(i+vi-2 == 0)then
                        osblocos(i,j)%visinhos(vi,vj)%linha = 50
                    endif
                    if(i+vi-2 == 51)then
                        osblocos(i,j)%visinhos(vi,vj)%linha = 1
                    endif
                    if(j+vj-2 == 0)then
                        osblocos(i,j)%visinhos(vi,vj)%coluna = 50
                    endif
                    if(j+vj-2 == 51)then
                        osblocos(i,j)%visinhos(vi,vj)%coluna = 1
                    endif
                end do  
            end do
        enddo
    enddo
endsubroutine

subroutine definirVivos(osblocos)
    use blocotype
    use postype
    implicit none
    type(bloco) :: osblocos(1:50,1:50)
    osblocos(25,25)%estado = 2
    osblocos(25,24)%estado = 2
    osblocos(25,23)%estado = 2

    osblocos(24,25)%estado = 2
    osblocos(26,24)%estado = 2
    osblocos(23,23)%estado = 2

    osblocos(30,16)%estado = 2
    osblocos(30,17)%estado = 2
    osblocos(30,15)%estado = 2
    osblocos(29,15)%estado = 2
    osblocos(28,15)%estado = 2
    osblocos(28,16)%estado = 2
    osblocos(28,17)%estado = 2
    osblocos(29,16)%estado = 2
    osblocos(29,16)%estado = 2
    osblocos(29,14)%estado = 2

    osblocos(24,24)%estado = 2
    osblocos(10,12)%estado = 2
    osblocos(11,11)%estado = 2
    osblocos( 9,11)%estado = 2
    osblocos(24,26)%estado = 2
    osblocos(40, 3)%estado = 2
    osblocos(32,22)%estado = 2
    osblocos(11,13)%estado = 2
    osblocos(15,18)%estado = 2

    osblocos(5,5)%estado = 2
    osblocos(5,6)%estado = 2
    osblocos(6,6)%estado = 2
    osblocos(6,7)%estado = 2
    osblocos(7,5)%estado = 2
end subroutine definirVivos

subroutine reproduzir(osblocos)
    use blocotype
    use postype
    implicit none
    type(bloco) :: osblocos(1:50,1:50)
    integer nV,i,j,vi,vj
    integer vizinhox,vizinhoy
    nV =0
    !para todos os blocos
    do i=1,50
        do j=1,50
            !se eles etiverem vazios
            if(osblocos(i,j)%estado == 0) then
                !devemos verificar seus vizinhos
                do vi=1,3
                    do vj=1,3
                        !então iremos contalos
                        if((vi==2).and.(vj==2))then
                            
                        else
                            vizinhox = osblocos(i,j)%visinhos(vi,vj)%linha
                            vizinhoy = osblocos(i,j)%visinhos(vi,vj)%coluna
                            if ( osblocos(vizinhox,vizinhoy)%estado ==2 ) then
                                nV = nV+1
                            endif
                        endif
                    enddo
                enddo
                !se tiver pelomenos 3 dispostos a reproduzir
                if(nV==3)then
                    !nascerá, nesse espaço vasio, um finhotinho
                    osblocos(i,j)%estado = 1
                endif
            endif
            !dando tudo certo, nos preparemos para observar outro espaço vazio
            nV =0
        enddo
    enddo
end subroutine reproduzir

subroutine selecaoNatural(osblocos)
    use blocotype
    use postype
    implicit none
    type(bloco) :: osblocos(1:50,1:50)
    integer nV,i,j,vi,vj
    integer vizinhox,vizinhoy
    nV =0
    !para todos os blocos
    do i=1,50
        do j=1,50
            !se eles etiverem vazios
            if(osblocos(i,j)%estado == 2) then
                !devemos verificar seus vizinhos
                do vi=1,3
                    do vj=1,3
                        !então iremos contalos
                        if((vj==2).and.(vi==2))then

                        else
                            vizinhox = osblocos(i,j)%visinhos(vi,vj)%linha
                            vizinhoy = osblocos(i,j)%visinhos(vi,vj)%coluna
                            if ( osblocos(vizinhox,vizinhoy)%estado ==2 .or. osblocos(vizinhox,vizinhoy)%estado ==3) then
                                nV = nV+1
                            endif
                        endif
                    enddo
                enddo
                !se tiver pelomenos 3 dispostos a reproduzir
                if(nV >3 .or. nV<2)then
                    !nascerá, nesse espaço vasio, um finhotinho
                    osblocos(i,j)%estado = 3
                endif
            endif
            !dando tudo certo, nos preparemos para observar outro espaço vazio
            nV =0
        enddo
    enddo
end subroutine selecaoNatural

subroutine tornarAdulto(osblocos)
    use blocotype
    use postype
    implicit none
    type(bloco) :: osblocos(1:50,1:50)
    integer i,j
    do i=1,50
        do j=1,50
            !se eles etiverem vazios
            if(osblocos(i,j)%estado == 1) then
                osblocos(i,j)%estado = 2
            endif
        enddo
    enddo
end subroutine tornarAdulto

subroutine removerMortos(osblocos)
    use blocotype
    use postype
    implicit none
    type(bloco) :: osblocos(1:50,1:50)
    integer i,j
    do i=1,50
        do j=1,50
            !se eles etiverem vazios
            if(osblocos(i,j)%estado == 3) then
                osblocos(i,j)%estado = 0
            endif
        enddo
    enddo
end subroutine removerMortos

integer function contagem(osblocos)
    use blocotype
    use postype
    implicit none
    type(bloco) :: osblocos(1:50,1:50)
    integer i,j,nE
    do i=1,50
        do j=1,50
            !se eles etiverem vazios
            if(osblocos(i,j)%estado == 2) then
                nE = nE +1
            endif
        enddo
    enddo
    contagem = nE
    return
endfunction contagem

subroutine pintarBlocos(osblocos,largura,winId)
    use blocotype
    use postype
    implicit none
    integer largura, i, j,winId
    type(bloco) :: osblocos(1:50,1:50)
    do i =1,50
        do j= 1,50
            if(osblocos(i,j)%estado == 0) then
                call newrgbcolor(winId,255,255,255)
                !call newpencolor(winId,1)
                call fillrect(winId,largura*i*1.0-largura*1.0,largura*j*1.0 -largura*1.0,largura*1.0,largura*1.0)
            else if(osblocos(i,j)%estado == 3) then
                call newrgbcolor(winId,80,80,80)
                !call newpencolor(winId,2)
                call fillrect(winId,largura*i*1.0 -largura*1.0,largura*j*1.0 -largura*1.0,largura*1.0,largura*1.0)
            else if (osblocos(i,j)%estado == 2) then
                call newrgbcolor(winId,255,50,65)
                !call newpencolor(winId,2)
                call fillrect(winId,largura*i*1.0 -largura*1.0,largura*j*1.0 -largura*1.0,largura*1.0,largura*1.0)
            endif
            call newrgbcolor(winId,30,30,30)
                !call newpencolor(winId,2)
            call fillrect(winId,0.0 ,largura*j*1.0 -largura*1.0,500*1.0,1*1.0)
            call fillrect(winId,largura*i*1.0 -largura*1.0,0.0,1*1.0,500*1.0)
        enddo
    enddo
end subroutine pintarBlocos

