!>@file   t_3d_noise.f90
!!@brief  module t_3d_noise
!!
!!@author Y. Liao and H. Matsui
!!@date Programmed by Y. Liao in Apr. 2018
!!      Modified by H. Matsui in Apr. 2020
!
!> @brief Construct 3D noise data for LIC
!!
!!@verbatim
!!      subroutine set_control_3d_cube_noise(noise_ctl, nze)
!!        type(cube_noise_ctl), intent(in) :: noise_ctl
!!        type(noise_cube), intent(inout) :: nze
!!      subroutine sel_const_3d_cube_noise(nze)
!!      subroutine dealloc_3d_cube_noise(nze)
!!      subroutine bcast_3d_cube_noise(nze)
!!        type(noise_cube), intent(inout) :: nze
!!        type(noise_cube), intent(inout) :: nze
!!@endverbatim
!
      module t_3d_noise
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &                        :: cflag_from_file = 'file'
      character(len = kchara), parameter, private                       &
     &                        :: cflag_randum = 'randum'
!
      integer(kind = kint), parameter :: iflag_from_file =    0
      integer(kind = kint), parameter :: iflag_randum =       1
!
      type noise_cube
!>        integer flag for LIC kernel function
!>          cflag_from_file: Read noise data file
!>          iflag_randum:    generate fram randum number
        integer(kind = kint) :: iflag_noise_type = 0
!
!>         Noise file name to read/write
        character(len=kchara) :: noise_file_name
!
!>         size of noise cube in physical space
        real(kind = kreal) :: size_cube(3)
!>         1 / asize_cube
        real(kind = kreal) :: asize_cube(3)
!
!>         step size of noise
        integer(kind = kint) :: i_stepsize
!>         resolution of noise cube
        integer(kind = kint) :: nidx_xyz(3)
!>         nobe of nodes of noise cube
        integer(kind = kint_gl) :: n_cube
!
!>         noise data
        real(kind = kreal), allocatable :: rnoise(:)
!>         gradient of noise
        real(kind = kreal), allocatable :: rnoise_grad(:,:)
!
!>         noise for data IO
        character(len = 1), allocatable :: cnoise(:)
      end type noise_cube
!
      private :: alloc_3d_cube_noise, alloc_3d_cube_noise_IO
      private :: dealloc_3d_cube_noise_IO, set_3d_cube_resolution
      private :: write_3d_charanoise, read_alloc_3d_charanoise
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_3d_cube_noise(noise_ctl, nze)
!
      use t_control_data_LIC_noise
      use skip_comment_f
!
      type(cube_noise_ctl), intent(in) :: noise_ctl
      type(noise_cube), intent(inout) :: nze
!
      character(len = kchara) :: tmpchara
      integer(kind = kint)  :: num_1d(3)
      real(kind = kreal)  :: c_size(3)
!
!
      nze%noise_file_name = 'noise'
      nze%i_stepsize = ione
      num_1d(1:3) = 256
      c_size(1:3) = one
!
      nze%iflag_noise_type = iflag_from_file
      if(noise_ctl%noise_type_ctl%iflag .gt. 0) then
        tmpchara = noise_ctl%noise_type_ctl%charavalue
        if(cmp_no_case(tmpchara, cflag_from_file)) then
          nze%iflag_noise_type = iflag_from_file
        else if(cmp_no_case(tmpchara, cflag_randum)) then
          nze%iflag_noise_type = iflag_randum
        end if
      end if
!
      if(noise_ctl%noise_file_name_ctl%iflag .gt. 0) then
        nze%noise_file_name = noise_ctl%noise_file_name_ctl%charavalue
      end if
      if(noise_ctl%noise_stepping_ctl%iflag .gt. 0) then
        nze%i_stepsize = noise_ctl%noise_stepping_ctl%intvalue
      end if
      if(noise_ctl%noise_resolution_ctl%iflag .gt. 0) then
        num_1d(1:3) = noise_ctl%noise_resolution_ctl%intvalue
      end if
      if(noise_ctl%noise_cube_size_ctl%iflag .gt. 0) then
        c_size(1:3) = noise_ctl%noise_cube_size_ctl%realvalue
      end if
!
      call set_3d_cube_resolution(num_1d(1), num_1d(2), num_1d(3), nze)
      call set_3d_cube_size(c_size(1), c_size(2), c_size(3), nze)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'nze%iflag_noise_type', nze%iflag_noise_type
        write(*,*) 'nze%noise_file_name: ', trim(nze%noise_file_name)
        write(*,*) 'nze%nidx_xyz', nze%nidx_xyz
        write(*,*) 'nze%n_cube', nze%n_cube
        write(*,*) 'nze%i_stepsize', nze%i_stepsize
        write(*,*) 'nze%size_cube', nze%size_cube
      end if
!
      end subroutine set_control_3d_cube_noise
!
!  ---------------------------------------------------------------------
!
      subroutine sel_const_3d_cube_noise(nze)
!
      use delete_data_files
      use cal_3d_noise
!
      type(noise_cube), intent(inout) :: nze
!
!
      if(check_file_exist(nze%noise_file_name)) then
        call read_alloc_3d_charanoise(my_rank, nze%noise_file_name, nze)
        call alloc_3d_cube_noise(nze)
      else
        call alloc_3d_cube_noise(nze)
        call const_3d_noise                                             &
     &     (nze%i_stepsize, nze%nidx_xyz, nze%n_cube, nze%rnoise)
!
        call alloc_3d_cube_noise_IO(nze)
        call cvt_rnoise_to_chara(nze%n_cube, nze%rnoise, nze%cnoise)
        call write_3d_charanoise(nze%noise_file_name, nze)
      end if
      call cvt_cnoise_to_real(nze%n_cube, nze%cnoise, nze%rnoise)
      call dealloc_3d_cube_noise_IO(nze)
!
      call grad_3d_noise                                                &
     &   (nze%n_cube, nze%nidx_xyz, nze%asize_cube, nze%rnoise,         &
     &    nze%rnoise_grad)
!
      end subroutine sel_const_3d_cube_noise
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_3d_cube_noise(nze)
!
      type(noise_cube), intent(inout) :: nze
!
      deallocate(nze%rnoise, nze%rnoise_grad)
!
      end subroutine dealloc_3d_cube_noise
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_3d_cube_noise(nze)
!
      use calypso_mpi_real
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(noise_cube), intent(inout) :: nze
!
      integer(kind = kint_gl) :: n3_cube
!
!
      call MPI_BCAST(nze%iflag_noise_type, 1, CALYPSO_INTEGER,          &
     &               0, CALYPSO_COMM, ierr_MPI)
      call calypso_mpi_bcast_character                                  &
     &   (nze%noise_file_name, cast_long(kchara), 0)
!
      call calypso_mpi_bcast_real(nze%size_cube, cast_long(ithree), 0)
      call calypso_mpi_bcast_real(nze%asize_cube, cast_long(ithree), 0)
!
      call calypso_mpi_bcast_int(nze%nidx_xyz, cast_long(ithree), 0)
      call MPI_BCAST(nze%i_stepsize, 1, CALYPSO_INTEGER,                &
     &               0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(nze%n_cube, 1, CALYPSO_GLOBAL_INT,                 &
     &               0, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) call alloc_3d_cube_noise(nze)
      n3_cube = 3 * nze%n_cube
      call calypso_mpi_bcast_real(nze%rnoise(1), nze%n_cube, 0)
      call calypso_mpi_bcast_real(nze%rnoise_grad(1,1), n3_cube, 0)
!
      end subroutine bcast_3d_cube_noise
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_3d_cube_noise(nze)
!
      type(noise_cube), intent(inout) :: nze
!
!
      allocate(nze%rnoise(nze%n_cube))
      allocate(nze%rnoise_grad(nze%n_cube,3))
!
!$omp parallel workshare
      nze%rnoise(1:nze%n_cube) = 0.0d0
      nze%rnoise_grad(1:nze%n_cube,1) = 0.0d0
      nze%rnoise_grad(1:nze%n_cube,2) = 0.0d0
      nze%rnoise_grad(1:nze%n_cube,3) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_3d_cube_noise
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_3d_cube_noise_IO(nze)
!
      type(noise_cube), intent(inout) :: nze
!
      allocate(nze%cnoise(nze%n_cube))
!
      end subroutine alloc_3d_cube_noise_IO
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_3d_cube_noise_IO(nze)
!
      type(noise_cube), intent(inout) :: nze
!
      deallocate(nze%cnoise)
!
      end subroutine dealloc_3d_cube_noise_IO
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_3d_cube_resolution(nx, ny, nz, nze)
!
      integer(kind = kint), intent(in) :: nx, ny, nz
      type(noise_cube), intent(inout) :: nze
!
      nze%nidx_xyz(1) = nx
      nze%nidx_xyz(2) = ny
      nze%nidx_xyz(3) = nz
      nze%n_cube =  int(nx,KIND(nze%n_cube)) * int(ny,KIND(nze%n_cube)) &
     &            * int(nz,KIND(nze%n_cube))
!
      end subroutine set_3d_cube_resolution
!
!  ---------------------------------------------------------------------
!
      subroutine set_3d_cube_size(x_size, y_size, z_size, nze)
!
      real(kind = kreal), intent(in) :: x_size, y_size, z_size
      type(noise_cube), intent(inout) :: nze
!
      integer(kind = kint) :: i
!
      nze%size_cube(1) = x_size
      nze%size_cube(2) = y_size
      nze%size_cube(3) = z_size
      do i = 1, 3
        if(nze%size_cube(i) .le. 0.0d0) then
          nze%asize_cube(i) = 1.0d0
        else
          nze%asize_cube(i) = 1.0d0 / nze%size_cube(i)
        end if
      end do
!
      end subroutine set_3d_cube_size
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_3d_charanoise(file_name, nze)
!
      use binary_IO
!
      character(len = kchara), intent(in) :: file_name
!
      type(noise_cube), intent(in) :: nze
!
      integer(kind = kint_gl), parameter :: ithree64 = 3
      integer(kind = kint), parameter :: id_write_nze = 22
      type(binary_IO_buffer)  :: bbuf
!
!
      bbuf%id_binary = id_write_nze
      call open_write_binary_file(file_name, bbuf)
      call write_mul_integer_b(ithree64, nze%nidx_xyz(1), bbuf)
      call write_mul_one_character_b(nze%n_cube, nze%cnoise, bbuf)
      call close_binary_file(bbuf)
!
      end subroutine write_3d_charanoise
!
!  ---------------------------------------------------------------------
!
      subroutine read_alloc_3d_charanoise(my_rank, file_name, nze)
!
      use binary_IO
!
      integer, intent(in) :: my_rank
      character(len = kchara), intent(in) :: file_name
!
      type(noise_cube), intent(inout) :: nze
!
      type(binary_IO_buffer)  :: bbuf
      integer(kind = kint), parameter :: id_read_nze =  21
      integer(kind = kint) :: nx(3)
      integer(kind = kint_gl), parameter :: ithree64 = 3
!
!
      bbuf%id_binary = id_read_nze
      call open_read_binary_file(file_name, my_rank, bbuf)
      call read_mul_integer_b(bbuf, ithree64, nx(1))
!
      call set_3d_cube_resolution(nx(1), nx(2), nx(3), nze)
      call alloc_3d_cube_noise_IO(nze)
!
      call read_mul_one_character_b(bbuf, nze%n_cube, nze%cnoise)
      call close_binary_file(bbuf)
!
      end subroutine read_alloc_3d_charanoise
!
!  ---------------------------------------------------------------------
!
      end module t_3d_noise
