program bissecao
    implicit none
    integer :: iter, max_iter
    real(kind=8) :: a, b, x, tol, fa, fb, fx

    ! Limites do intervalo inicial
    a = 0.0d0
    b = 2.0d0

    ! Tolerância e número máximo de iterações
    tol = 1.0d-6
    max_iter = 100

    ! Inicializa contagem de iterações
    iter = 0

    ! Calcula os valores da função nos pontos a e b
    fa = func(a)
    fb = func(b)

    ! Verifica se a função muda de sinal nos extremos do intervalo
    if (fa * fb >= 0.0d0) then
        print *, "Erro: A função não muda de sinal no intervalo inicial."
        stop
    end if

    ! Loop principal do método da bisseção
    do while (abs(b - a) > tol .and. iter < max_iter)
        iter = iter + 1
        x = (a + b) / 2.0d0
        fx = func(x)

        if (fx * fa < 0.0d0) then
            b = x
            fb = fx
        else
            a = x
            fa = fx
        end if

        ! Saída dos resultados parciais
        print *, "Iteração:", iter, "  x:", x, "  f(x):", fx
    end do

    ! Resultado final
    print *, "Raiz aproximada:", x

contains

    ! Definição da função
    real(kind=8) function func(x)
        real(kind=8), intent(in) :: x
        func = x**3 - 9.0d0 * x + 5.0d0
    end function func

end program bissecao
