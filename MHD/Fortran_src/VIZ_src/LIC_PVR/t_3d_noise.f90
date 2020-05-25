!>@file   t_3d_noise.f90
!!@brief  module t_3d_noise
!!
!!@author H. Matsui
!!@date Programmed in Apr. 2018
!
!> @brief Construct 3D noise data for LIC
!!
!!@verbatim
!!      subroutine set_control_3d_cube_noise(noise_ctl, nze)
!!        type(cube_noise_ctl), intent(in) :: noise_ctl
!!        type(noise_cube), intent(inout) :: nze
!!      subroutine sel_const_3d_cube_noise(my_rank, nze)
!!        integer, intent(in) :: my_rank
!!        type(noise_cube), intent(inout) :: nze
!!      subroutine dealloc_3d_cube_noise(nze)
!!        type(noise_cube), intent(inout) :: nze
!!@endverbatim
!
      module t_3d_noise
!
      use m_precision
      use m_constants
!
      implicit none
!
      type noise_cube
        character(len=kchara) :: noise_file_name
!
        real(kind = kreal) :: size_cube(3)
        real(kind = kreal) :: asize_cube(3)
!
        integer(kind = kint) :: i_stepsize
        integer(kind = kint) :: nidx_xyz(3)
        integer(kind = kint_gl) :: n_cube
!
        real(kind = kreal), allocatable :: rnoise(:)
        real(kind = kreal), allocatable :: rnoise_grad(:,:)
!
        character(len = 1), allocatable :: cnoise(:)
      end type noise_cube
!
      private :: alloc_3d_cube_noise, alloc_3d_cube_noise_IO
      private :: dealloc_3d_cube_noise_IO, set_3d_cube_resolution
      private :: write_3d_charanoise, read_alloc_3d_charanoise
      private :: cvt_rnoise_to_chara, cvt_cnoise_to_real
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
!
      type(cube_noise_ctl), intent(in) :: noise_ctl
      type(noise_cube), intent(inout) :: nze
!
      integer(kind = kint)  :: num_1d(3)
      real(kind = kreal)  :: c_size(3)
!
!
      nze%noise_file_name = 'noise'
      nze%i_stepsize = ione
      num_1d(1:3) = 256
      c_size(1:3) = one
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
      end subroutine set_control_3d_cube_noise
!
!  ---------------------------------------------------------------------
!
      subroutine sel_const_3d_cube_noise(my_rank, nze)
!
      use delete_data_files
      use cal_3d_noise
!
      integer, intent(in) :: my_rank
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
      type(binary_IO_buffer)  :: bbuf
!
!
      call open_write_binary_file(file_name, bbuf)
      call write_mul_integer_b(ithree64, nze%nidx_xyz(1), bbuf)
      call write_mul_one_character_b(nze%n_cube, nze%cnoise, bbuf)
      call close_binary_file
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
      integer(kind = kint) :: nx(3)
      integer(kind = kint_gl), parameter :: ithree64 = 3
!
!
      call open_read_binary_file(file_name, my_rank, bbuf)
      call read_mul_integer_b(bbuf, ithree64, nx(1))
!
      call set_3d_cube_resolution(nx(1), nx(2), nx(3), nze)
      call alloc_3d_cube_noise_IO(nze)
!
      call read_mul_one_character_b(bbuf, nze%n_cube, nze%cnoise)
      call close_binary_file
!
      end subroutine read_alloc_3d_charanoise
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cvt_rnoise_to_chara(nnod_gl, rnoise, cnoise)
!
      integer(kind = kint_gl), intent(in) :: nnod_gl
      real(kind = kreal), intent(in) :: rnoise(nnod_gl)
      character(len = 1), intent(inout) :: cnoise(nnod_gl)
!
      integer(kind = kint_gl) :: inod_gl, inoise
!
!$omp parallel do private(inod_gl, inoise)
      do inod_gl = 1, nnod_gl
        inoise = int((rnoise(inod_gl) * 256),KIND(inoise))
        cnoise(inod_gl) = char(inoise)
      end do
!$omp end parallel do
!
      end subroutine cvt_rnoise_to_chara
!
!  ---------------------------------------------------------------------
!
      subroutine cvt_cnoise_to_real(nnod_gl, cnoise, rnoise)
!
      integer(kind = kint_gl), intent(in) :: nnod_gl
      character(len = 1), intent(in) :: cnoise(nnod_gl)
      real(kind = kreal), intent(inout) :: rnoise(nnod_gl)
!
      real(kind = kreal) :: pol
      integer(kind = kint_gl) :: inod_gl
!
      pol = 1.0d0 / 256.0
!$omp parallel do private(inod_gl)
      do inod_gl = 1, nnod_gl
        rnoise(inod_gl) = dble( ichar(cnoise(inod_gl)) ) * pol
      end do
!$omp end parallel do
!
      end subroutine cvt_cnoise_to_real
!
!  ---------------------------------------------------------------------
!
      end module t_3d_noise
