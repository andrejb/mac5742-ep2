! adaptado do exemplo do pacote openmpi-doc do debian estável (lenny).
program sorteio
  use mpi
  implicit none
  integer :: rank, size, tag, next, prev, message, ierr, destination, i
  integer*4 :: timeArray(3) ! Contem a hora, minuto e segundos
  integer :: active
  character*10 :: cfrac
  real :: frac
  integer, dimension(2) :: seed


  ! --------------------------------------------------------------------------
  ! inicializacao.

  ! inicia o MPI
  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)

  ! verifica se o argumento da linha de comando faz sentido
  call getarg(1,cfrac)
  read(cfrac,*) frac
  if (frac .lt. 0.0 .or. frac .gt. 1.0) then
    print *, "A fracao (", frac, ") esta fora do esperado."
    call MPI_FINALIZE(ierr)
    call exit(1)
  end if
  message = ceiling(frac*size)

  ! inicializa o anel
  tag = 201
  next = mod((rank + 1), size)
  prev = mod((rank + size - 1), size)
  active = 0

  ! inicializa numeros aleatorios
  call init_random_seed()

  ! se somos o processo 0, iniciamos o ciclo escolhendo um processo aleatorio e
  ! enviando quantos processos faltam mudar para o estado "Ativado".
  if (rank .eq. 0) then
     call get_random_int(0,size-1,destination)
     !print *, '  0 -> ',destination,': faltam ',message,' processos.'
     call MPI_SEND(message, 1, MPI_INTEGER, destination, tag, MPI_COMM_WORLD, ierr)
  endif


  ! --------------------------------------------------------------------------
  ! loop principal:
  !
  ! enquanto (1), faça:
  !   aguarde uma mensagem com o número de processos que faltam mudar de estado.
  !   se (já estou ocupado), então
  !     envie o número de processos que faltam mudar de estado para o próximo no
  !       anel.
  !   senão
  !     entre em estado ocupado.
  !     decremente o número de processos que faltam mudar de estado.
  !     escolha um nó aleatoriamente.
  !     envie o número de processos que faltam mudar de estado para este nó.
  do
    call MPI_RECV(message, 1, MPI_INTEGER, MPI_ANY_SOURCE, tag, MPI_COMM_WORLD, &
                  MPI_STATUS_IGNORE, ierr)

    ! caso a mensagem seja -1, devo terminar.
    if (message .eq. -1) then
      exit
    end if

    ! caso a mensagem seja 0, nao ha mais processos para serem alterado e eu
    ! devo avisar aos outros disto.
    if (message .eq. 0) then
      do i = 0, size-1
        call MPI_SEND(-1, 1, MPI_INTEGER, i, tag, MPI_COMM_WORLD, ierr)
      end do
    end if

    ! se eu ja estou ativo passo a mensagem para frente.
    if (active .eq. 1) then
      !print *, '  ',rank,' -> ',destination,': faltam ',message,' processos.'
      call MPI_SEND(message, 1, MPI_INTEGER, next, tag, MPI_COMM_WORLD, ierr)
    ! caso contrario, mudo meu estado e repito o processo.
    else
      active = 1
      message = message - 1
      call get_random_int(0,size-1,destination)
      !print *, '  ',rank,' -> ',destination,': faltam ',message,' processos.'
      call MPI_SEND(message, 1, MPI_INTEGER, destination, tag, MPI_COMM_WORLD, ierr)
    end if
  end do


  ! --------------------------------------------------------------------------
  ! impressao de resultados.
  call MPI_BARRIER(MPI_COMM_WORLD, ierr)

  if (rank .ne. 0) then
    do
      call MPI_RECV(message, 1, MPI_INTEGER, prev, tag, MPI_COMM_WORLD, &
                    MPI_STATUS_IGNORE, ierr)
      ! descarta mensagens indesejadas
      if (message .eq. -2) then
        exit
      end if
    end do
  end if

  if (active .eq. 1) then
    print '(I4,A)', rank, ': ativo'
  else
    print '(I4,A)', rank, ':       inativo'
  end if

  if (rank .ne. size-1) then
    message = -2
    call MPI_SEND(message, 1, MPI_INTEGER, next, tag, MPI_COMM_WORLD, ierr)
  end if

  call MPI_FINALIZE(ierr)


contains


! ---------------------------------------------------------------------------- 
! subrotina: get_random_int
!
! retorna um inteiro aleatorio entre start_int e end_int. Supoe que ja houve
! uma inicializacao previa dos numeros aleatorios.
subroutine get_random_int(start_int, end_int, random_int)
  integer, intent(in) :: start_int, end_int
  integer, intent(out) :: random_int
  real :: r
  call random_number(r)
  random_int = int(r*(end_int+1-start_int))+start_int
end subroutine get_random_int


! ---------------------------------------------------------------------------- 
! subrotina: init_random_seed
!
! inicializa a semente de numeros pseudoaleatorios.
subroutine init_random_seed()
  integer :: i, n, clock
  integer, dimension(:), allocatable :: seed

  call random_seed(size = n)
  allocate(seed(n))

  call system_clock(count=clock)

  seed = clock + 37 * (/ (i - 1, i = 1, n) /)
  call random_seed(put = seed)

  deallocate(seed)
end subroutine

end program
      
