!>@file   t_calypso_mpi_IO_buffer.f90
!!@brief  module t_calypso_mpi_IO_buffer
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!> @brief Base parameter structure for MPI-IO
!!
!!@verbatim
!!      subroutine alloc_realarray_IO(IO_param, r_array)
!!      subroutine alloc_vectarray_IO(IO_param, v_array)
!!      subroutine alloc_charaarray_IO(IO_param, c_array)
!!      subroutine alloc_ivecarray_IO(IO_param, iv_array)
!!      subroutine alloc_intarray_IO(IO_param, i_array)
!!      subroutine alloc_int8array_IO(IO_param, i8_array)
!!
!!      subroutine dealloc_realarray_IO(r_array)
!!      subroutine dealloc_vectarray_IO(v_array)
!!      subroutine dealloc_charaarray_IO(c_array)
!!      subroutine dealloc_ivecarray_IO(iv_array)
!!      subroutine dealloc_intarray_IO(i_array)
!!      subroutine dealloc_int8array_IO(i8_array)
!!        type(calypso_MPI_IO_params), intent(in) :: IO_param
!!        type(realarray_IO), intent(inout) :: r_array
!!        type(vectarray_IO), intent(inout) :: v_array
!!        type(charaarray_IO), intent(inout) :: c_array
!!        type(ivecarray_IO), intent(inout) :: iv_array
!!        type(intarray_IO), intent(inout) :: i_array
!!        type(charaarray_IO), intent(inout) :: i8_array
!!
!!      subroutine dealloc_character_buffers(nloop, c_array)
!!      subroutine dealloc_integer_buffers(nloop, i_array)
!!
!!      subroutine set_istack_by_chara_length                           &
!!     &         (nprocs_in, nloop, c_array, istack_merged)
!!      subroutine set_istack_by_i8_buffer                              &
!!     &         (nprocs_in, nloop, i8_array, istack_merged)
!!      subroutine set_istack_by_int_buffer                             &
!!     &         (nprocs_in, nloop, i_array, istack_merged)
!!      subroutine set_istack_by_int2d_buffer                           &
!!     &         (nprocs_in, nloop, iv_array, istack_merged)
!!      subroutine set_istack_by_real_buffer                            &
!!     &         (nprocs_in, nloop, r_array, istack_merged)
!!      subroutine set_istack_by_vector_buffer                          &
!!     &         (nprocs_in, nloop, v_array, istack_merged)
!!@endverbatim
!
      module t_calypso_mpi_IO_buffer
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
      use t_calypso_mpi_IO_param
!
      implicit none
!
!
!>      Structure for real array for MPI-IO
      type realarray_IO
        integer(kind = kint_gl) :: num
        real(kind = kreal), allocatable :: r_IO(:)
      end type realarray_IO
!
!>      Structure for 2D vectr array for MPI-IO
      type vectarray_IO
        integer(kind = kint_gl) :: n1
        integer(kind = kint) :: n2
        real(kind = kreal), allocatable :: v_IO(:,:)
      end type vectarray_IO
!
!>      Structure for integer array for MPI-IO
      type intarray_IO
        integer(kind = kint_gl) :: num
        integer(kind = kint), allocatable :: i_IO(:)
      end type intarray_IO
!
!>      Structure for integer vector array for MPI-IO
      type ivecarray_IO
        integer(kind = kint_gl) :: n1
        integer(kind = kint) :: n2
        integer(kind = kint), allocatable :: iv_IO(:,:)
      end type ivecarray_IO
!
!>      Structure for 8-byte integer array for MPI-IO
      type int8array_IO
        integer(kind = kint_gl) :: num
        integer(kind = kint_gl), allocatable :: i8_IO(:)
      end type int8array_IO
!
!>      Structure for 8-byte integer array for MPI-IO
      type charaarray_IO
        integer(kind = kint_gl) :: num
        character(len = 1), allocatable :: c_IO(:)
      end type charaarray_IO
!
!>      Structure for parameters of MPI-IO
      type calypso_MPI_IO_params
!>        Structure for real array for MPI-IO
        type(realarray_IO), allocatable ::  r_array(:)
!>        Structure for real array for MPI-IO
        type(vectarray_IO), allocatable ::  v_array(:)
!>        Structure for real array for MPI-IO
        type(intarray_IO), allocatable ::   i_array(:)
!>        Structure for real array for MPI-IO
        type(ivecarray_IO), allocatable ::  iv_array(:)
!>        Structure for real array for MPI-IO
        type(int8array_IO), allocatable ::  i8_array(:)
!>        Structure for real array for MPI-IO
        type(charaarray_IO), allocatable :: c_array(:)
      end type calypso_MPI_IO_params
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_realarray_IO(IO_param, r_array)
!
      type(calypso_MPI_IO_params), intent(in) :: IO_param
      type(realarray_IO), intent(inout) :: r_array
!
      allocate(r_array(IO_param%nloop))
!
      end subroutine alloc_realarray_IO
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_vectarray_IO(IO_param, v_array)
!
      type(calypso_MPI_IO_params), intent(in) :: IO_param
      type(vectarray_IO), intent(inout) :: v_array
!
      allocate(v_array(IO_param%nloop))
!
      end subroutine alloc_vectarray_IO
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_charaarray_IO(IO_param, c_array)
!
      type(calypso_MPI_IO_params), intent(in) :: IO_param
      type(charaarray_IO), intent(inout) :: c_array
!
      allocate(c_array(IO_param%nloop))
!
      end subroutine alloc_charaarray_IO
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_ivecarray_IO(IO_param, iv_array)
!
      type(calypso_MPI_IO_params), intent(in) :: IO_param
      type(ivecarray_IO), intent(inout) :: iv_array
!
      allocate(iv_array(IO_param%nloop))
!
      end subroutine alloc_ivecarray_IO
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_intarray_IO(IO_param, i_array)
!
      type(calypso_MPI_IO_params), intent(in) :: IO_param
      type(intarray_IO), intent(inout) :: i_array
!
      allocate(i_array(IO_param%nloop))
!
      end subroutine alloc_intarray_IO
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_int8array_IO(IO_param, i8_array)
!
      type(calypso_MPI_IO_params), intent(in) :: IO_param
      type(charaarray_IO), intent(inout) :: i8_array
!
      allocate(i8_array(IO_param%nloop))
!
      end subroutine alloc_int8array_IO
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_realarray_IO(r_array)
!
      type(realarray_IO), intent(inout) :: r_array
!
      deallocate(r_array)
!
      end subroutine dealloc_realarray_IO
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_vectarray_IO(v_array)
!
      type(vectarray_IO), intent(inout) :: v_array
!
      deallocate(v_array)
!
      end subroutine dealloc_vectarray_IO
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_charaarray_IO(c_array)
!
      type(charaarray_IO), intent(inout) :: c_array
!
      deallocate(c_array)
!
      end subroutine dealloc_charaarray_IO
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_ivecarray_IO(iv_array)
!
      type(ivecarray_IO), intent(inout) :: iv_array
!
      deallocate(iv_array)
!
      end subroutine dealloc_ivecarray_IO
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_intarray_IO(i_array)
!
      type(intarray_IO), intent(inout) :: i_array
!
      deallocate(i_array)
!
      end subroutine dealloc_intarray_IO
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_int8array_IO(i8_array)
!
      type(charaarray_IO), intent(inout) :: i8_array
!
      deallocate(charaarray_IO)
!
      end subroutine dealloc_int8array_IO
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_character_buffers(nloop, c_array)
!
      integer(kind = kint), intent(in) :: nloop
      type(charaarray_IO), intent(inout) ::  c_array(nloop)
!
      integer(kind = kint) :: iloop
!
!
      do iloop = 1, nloop
        deallocate(c_array(iloop)%c_IO)
      end do
!
      end subroutine dealloc_character_buffers
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_integer_buffers(nloop, i_array)
!
      integer(kind = kint), intent(in) :: nloop
      type(intarray_IO),  intent(inout) :: i_array(nloop)
!
      integer(kind = kint) :: iloop
!
!
      do iloop = 1, nloop
        deallocate(i_array(iloop)%I_IO)
      end do
!
      end subroutine dealloc_integer_buffers
!
!  ---------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_istack_by_chara_length                             &
     &         (nprocs_in, nloop, c_array, istack_merged)
!
      integer, intent(in) :: nprocs_in
      integer(kind = kint), intent(in) :: nloop
      type(charaarray_IO), intent(inout) ::  c_array(nloop)
      integer(kind = kint_gl), intent(inout)                            &
     &                         :: istack_merged(0:nprocs_in)
!
      integer(kind = kint_gl) :: num_local(nloop)
!
!
      num_local(1:nloop) = c_array(1:nloop)%num
      call set_istack_over_subdomains                                   &
     &   (nprocs_in, nloop, num_local, istack_merged)
!
      end subroutine set_istack_by_chara_length
!
!  ---------------------------------------------------------------------
!
      subroutine set_istack_by_i8_buffer                                &
     &         (nprocs_in, nloop, i8_array, istack_merged)
!
      integer, intent(in) :: nprocs_in
      integer(kind = kint), intent(in) :: nloop
      type(int8array_IO), intent(inout) ::  i8_array(nloop)
      integer(kind = kint_gl), intent(inout)                            &
     &                         :: istack_merged(0:nprocs_in)
!
      integer(kind = kint_gl) :: num_local(nloop)
!
!
      num_local(1:nloop) = i8_array(1:nloop)%num * kint_gl
      call set_istack_over_subdomains                                   &
     &   (nprocs_in, nloop, num_local, istack_merged)
!
      end subroutine set_istack_by_i8_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine set_istack_by_int_buffer                               &
     &         (nprocs_in, nloop, i_array, istack_merged)
!
      integer, intent(in) :: nprocs_in
      integer(kind = kint), intent(in) :: nloop
      type(intarray_IO), intent(inout) ::  i_array(nloop)
      integer(kind = kint_gl), intent(inout)                            &
     &                         :: istack_merged(0:nprocs_in)
!
      integer(kind = kint_gl) :: num_local(nloop)
!
!
      num_local(1:nloop) = i_array(1:nloop)%num * kint
      call set_istack_over_subdomains                                   &
     &   (nprocs_in, nloop, num_local, istack_merged)
!
      end subroutine set_istack_by_int_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine set_istack_by_int2d_buffer                             &
     &         (nprocs_in, nloop, iv_array, istack_merged)
!
      integer, intent(in) :: nprocs_in
      integer(kind = kint), intent(in) :: nloop
      type(ivecarray_IO), intent(inout) ::  iv_array(nloop)
      integer(kind = kint_gl), intent(inout)                            &
     &                         :: istack_merged(0:nprocs_in)
!
      integer(kind = kint_gl) :: num_local(nloop)
!
!
      num_local(1:nloop) = iv_array(1:nloop)%n1                         &
     &                    * iv_array(1:nloop)%n2 *kint
      call set_istack_over_subdomains                                   &
     &   (nprocs_in, nloop, num_local, istack_merged)
!
      end subroutine set_istack_by_int2d_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine set_istack_by_real_buffer                              &
     &         (nprocs_in, nloop, r_array, istack_merged)
!
      integer, intent(in) :: nprocs_in
      integer(kind = kint), intent(in) :: nloop
      type(realarray_IO), intent(inout) ::  r_array(nloop)
      integer(kind = kint_gl), intent(inout)                            &
     &                         :: istack_merged(0:nprocs_in)
!
      integer(kind = kint_gl) :: num_local(nloop)
!
!
      num_local(1:nloop) = r_array(1:nloop)%num * kreal
      call set_istack_over_subdomains                                   &
     &   (nprocs_in, nloop, num_local, istack_merged)
!
      end subroutine set_istack_by_real_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine set_istack_by_vector_buffer                            &
     &         (nprocs_in, nloop, v_array, istack_merged)
!
      integer, intent(in) :: nprocs_in
      integer(kind = kint), intent(in) :: nloop
      type(vectarray_IO), intent(inout) ::  v_array(nloop)
      integer(kind = kint_gl), intent(inout)                            &
     &                         :: istack_merged(0:nprocs_in)
!
      integer(kind = kint_gl) :: num_local(nloop)
!
!
      num_local(1:nloop) = v_array(1:nloop)%n1                          &
     &                    * v_array(1:nloop)%n2 * kreal
      call set_istack_over_subdomains                                   &
     &   (nprocs_in, nloop, num_local, istack_merged)
!
      end subroutine set_istack_by_vector_buffer
!
!  ---------------------------------------------------------------------
!
      end module t_calypso_mpi_IO_buffer
