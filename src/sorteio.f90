! Adaptado do exemplo do pacote openmpi-doc do debian estável (lenny).
program sorteio
  use mpi
  implicit none
  integer :: rank, size, tag, next, from, message, ierr, destination, i
  integer*4 :: timeArray(3) ! Contem a hora, minuto e segundos
  integer :: active
  character*10 :: cfrac
  real :: frac
  integer :: mensagens

  ! Inicia o MPI
  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)

  ! Verifica se o argumento da linha de comando faz sentido
  call getarg(1,cfrac)
  read(cfrac,*) frac
  if (frac .lt. 0.0 .or. frac .gt. 1.0) then
    print *, "A fracao (", frac, ") esta fora do esperado."
    call MPI_FINALIZE(ierr)
    call exit(1)
  end if
  message = ceiling(frac*size)

  ! Inicializa o anel
  tag = 201
  next = mod((rank + 1), size)
  from = mod((rank + size - 1), size)
  active = 0
  mensagens = 0

  ! inicializa numeros aleatorios
  call itime(timeArray) ! Obtem a hora atual
  destination = int(rand(timeArray(1)+timeArray(2)+timeArray(3))) ! este primeiro valor eh jogado fora.

  ! Se somos o processo 0, iniciamos o ciclo escolhendo um processo aleatorio e
  ! enviando quantos processos faltam mudar para o estado "Ativado".
  if (rank .eq. 0) then
     call get_random_int(0,size-1,destination)
     !print *, 'Inicializacao:'
     !print *, '  0 -> ',destination,': faltam ',message,' processos.'
     mensagens = mensagens+1
     call MPI_SEND(message, 1, MPI_INTEGER, destination, tag, MPI_COMM_WORLD, ierr)
  endif

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

    ! Caso a mensagem seja -1, devo terminar.
    if (message .eq. -1) then
      if (active .eq. 1) then
        print '(I4,A,I0,A)', rank, ': ativo          (',mensagens,') mensagens'
      else
        print '(I4,A,I0,A)', rank, ':       inativo  (',mensagens,') mensagens'
      end if
      exit
    end if

    ! Caso a mensagem seja 0, nao ha mais processos para serem alterado e eu
    ! devo avisar aos outros disto.
    if (message .eq. 0) then
      do i = 0, size-1
        mensagens = mensagens+1
        call MPI_SEND(-1, 1, MPI_INTEGER, i, tag, MPI_COMM_WORLD, ierr)
      end do
    end if

    ! Se eu ja estou ativo passo a mensagem para frente.
    if (active .eq. 1) then
      !print *, '  ',rank,' -> ',destination,': faltam ',message,' processos.'
      mensagens = mensagens+1
      call MPI_SEND(message, 1, MPI_INTEGER, next, tag, MPI_COMM_WORLD, ierr)
    ! Caso contrario, mudo meu estado e repito o processo.
    else
      active = 1
      message = message - 1
      call get_random_int(0,size-1,destination)
      !print *, '  ',rank,' -> ',destination,': faltam ',message,' processos.'
      mensagens = mensagens+1
      call MPI_SEND(message, 1, MPI_INTEGER, destination, tag, MPI_COMM_WORLD, ierr)
    end if
  end do

  call MPI_FINALIZE(ierr)

contains

! Retorna um inteiro aleatorio entre start_int e end_int. Supoe que ja houve
! uma inicializacao previa dos numeros aleatorios.
subroutine get_random_int(start_int, end_int, random_int)
  integer, intent(in) :: start_int, end_int
  integer, intent(out) :: random_int
  random_int = int(rand()*(end_int+1-start_int))+start_int
end subroutine get_random_int

end program
      
