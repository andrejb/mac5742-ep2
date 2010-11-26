! adaptado do exemplo do pacote openmpi-doc do debian estável (lenny).
program sorteio
  use mpi
  use qsort_module
  implicit none
  integer :: rank, size, tag, next, prev, nproc, ierr, destination, i, message
  integer*4 :: timeArray(3) ! Contem a hora, minuto e segundos
  integer :: active
  character*10 :: cfrac
  real :: frac, myrand
  integer, dimension(2) :: seed
  real, allocatable :: procrand(:)
  integer, allocatable :: processes(:)


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
  nproc = ceiling(frac*size)

  ! inicializa variaveis
  tag = 201
  next = mod((rank + 1), size)
  prev = mod((rank + size - 1), size)
  active = 0
  allocate(procrand(size))
  allocate(processes(size))

  ! escolhe um numero aleatorio entre 0 e 1
  call init_random_seed(rank)
  call random_number(myrand)

  ! Cada processo recebe uma copia dos numeros sorteados pelos outros.
  do i = 0, size-1
    processes(i+1) = i
    CALL MPI_GATHER(myrand, 1, MPI_REAL, procrand, 1, MPI_REAL, i, MPI_COMM_WORLD, ierr)
  end do

  ! Ordena os números sorteados e os processos.
  call Qsort(procrand, processes)

  ! descobre se eu tenho que mudar de estado
  do i = 1, nproc
    if (processes(i) .eq. rank) then
      active = 1
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
! subrotina: init_random_seed
!
! inicializa a semente de numeros pseudoaleatorios.
subroutine init_random_seed(rank)
  integer, intent(in) :: rank
  integer :: i, n, clock
  integer, dimension(:), allocatable :: seed

  call random_seed(size = n)
  allocate(seed(n))

  call system_clock(count=clock)

  seed = clock + 37 * (/ (i - 1, i = 1, n) /) + rank
  call random_seed(put = seed)

  deallocate(seed)
end subroutine

end program
      
