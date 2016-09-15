!>@file   link_sph_data_2_IO_buffer.f90
!!@brief  module link_sph_data_2_IO_buffer
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!> @brief Structure for grid and comm table for spherical transform
!!
!!@verbatim
!!      subroutine link_sf_sph_nidx_4_mpi_IO(nloop, sph_IO, i_array)
!!      subroutine link_sph_ist_4_mpi_IO(nloop, sph_IO, i_array)
!!      subroutine link_sph_ied_4_mpi_IO(nloop, sph_IO, i_array)
!!      subroutine link_sph_radius_4_mpi_IO(nloop, sph_IO,              &
!!     &          i_array, r_array)
!!      subroutine link_sph_theta_4_mpi_IO(nloop, sph_IO, iv_array)
!!      subroutine link_sph_phi_4_mpi_IO(nloop, sph_IO, iv_array)
!!      subroutine link_sph_node_4_mpi_IO(nloop, sph_IO,                &
!!     &          i8_array, iv_array)
!!        type(sph_IO_data), intent(in) :: sph_IO(nloop)
!!        type(int8array_IO), intent(inout) :: i8_array(nloop)
!!        type(intarray_IO),  intent(inout) :: i_array(nloop)
!!        type(ivecarray_IO), intent(inout) :: iv_array(nloop)
!!        type(realarray_IO), intent(inout) :: r_array(nloop)
!!@endverbatim
!
      module link_sph_data_2_IO_buffer
!
      use m_precision
      use calypso_mpi
!
      use t_node_id_spherical_IO
      use t_calypso_mpi_IO_param
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine link_sf_sph_nidx_4_mpi_IO(nloop, sph_IO, i_array)
!
      integer(kind = kint), intent(in) :: nloop
      type(sph_IO_data), intent(in) :: sph_IO(nloop)
      type(intarray_IO), intent(inout) :: i_array(nloop)
!
      integer(kind = kint) :: i
!
!
      do i = 1, nloop
        i_array(i)%num =   sph_IO(i)%numdir_sph
        i_array(i)%i_IO => sph_IO(i)%nidx_sph
      end do
!
      end subroutine link_sf_sph_nidx_4_mpi_IO
!
! -----------------------------------------------------------------------
!
      subroutine link_sph_ist_4_mpi_IO(nloop, sph_IO, i_array)
!
      integer(kind = kint), intent(in) :: nloop
      type(sph_IO_data), intent(in) :: sph_IO(nloop)
      type(intarray_IO), intent(inout) :: i_array(nloop)
!
      integer(kind = kint) :: i
!
!
      do i = 1, nloop
        i_array(i)%num =   sph_IO(i)%numdir_sph
        i_array(i)%i_IO => sph_IO(i)%ist_sph
      end do
!
      end subroutine link_sph_ist_4_mpi_IO
!
! -----------------------------------------------------------------------
!
      subroutine link_sph_ied_4_mpi_IO(nloop, sph_IO, i_array)
!
      integer(kind = kint), intent(in) :: nloop
      type(sph_IO_data), intent(in) :: sph_IO(nloop)
      type(intarray_IO), intent(inout) :: i_array(nloop)
!
      integer(kind = kint) :: i
!
!
      do i = 1, nloop
        i_array(i)%num =   sph_IO(i)%numdir_sph
        i_array(i)%i_IO => sph_IO(i)%ied_sph
      end do
!
      end subroutine link_sph_ied_4_mpi_IO
!
! -----------------------------------------------------------------------
!
      subroutine link_sph_radius_4_mpi_IO(nloop, sph_IO,                &
     &          i_array, r_array)
!
      integer(kind = kint), intent(in) :: nloop
      type(sph_IO_data), intent(in) :: sph_IO(nloop)
      type(intarray_IO), intent(inout) :: i_array(nloop)
      type(realarray_IO), intent(inout) :: r_array(nloop)
!
      integer(kind = kint) :: i
!
!
      do i = 1, nloop
        i_array(i)%num =   sph_IO(i)%nidx_sph(1)
        i_array(i)%i_IO => sph_IO(i)%idx_gl_1
!
        r_array(i)%num =   sph_IO(i)%nidx_sph(1)
        r_array(i)%r_IO => sph_IO(i)%r_gl_1
      end do
!
      end subroutine link_sph_radius_4_mpi_IO
!
! -----------------------------------------------------------------------
!
      subroutine link_sph_theta_4_mpi_IO(nloop, sph_IO, iv_array)
!
      integer(kind = kint), intent(in) :: nloop
      type(sph_IO_data), intent(inout) :: sph_IO(nloop)
      type(ivecarray_IO), intent(inout) :: iv_array(nloop)
!
      integer(kind = kint) :: i
!
!
      do i = 1, nloop
        iv_array(i)%n1 =     sph_IO(i)%nidx_sph(2)
        iv_array(i)%n2 =     sph_IO(i)%ncomp_table_1d(2)
        iv_array(i)%iv_IO => sph_IO(i)%idx_gl_2
      end do
!
      end subroutine link_sph_theta_4_mpi_IO
!
! -----------------------------------------------------------------------
!
      subroutine link_sph_phi_4_mpi_IO(nloop, sph_IO, iv_array)
!
      integer(kind = kint), intent(in) :: nloop
      type(sph_IO_data), intent(inout) :: sph_IO(nloop)
      type(ivecarray_IO), intent(inout) :: iv_array(nloop)
!
      integer(kind = kint) :: i
!
!
      do i = 1, nloop
        iv_array(i)%n1 =     sph_IO(i)%nidx_sph(3)
        iv_array(i)%n2 =     sph_IO(i)%ncomp_table_1d(3)
        iv_array(i)%iv_IO => sph_IO(i)%idx_gl_3
      end do
!
      end subroutine link_sph_phi_4_mpi_IO
!
! -----------------------------------------------------------------------
!
      subroutine link_sph_node_4_mpi_IO(nloop, sph_IO,                  &
     &          i8_array, iv_array)
!
      integer(kind = kint), intent(in) :: nloop
      type(sph_IO_data), intent(inout) :: sph_IO(nloop)
!
      type(int8array_IO), intent(inout) :: i8_array(nloop)
      type(ivecarray_IO), intent(inout) :: iv_array(nloop)
!
      integer(kind = kint) :: i
!
!
      do i = 1, nloop
        i8_array(i)%num =    sph_IO(i)%numnod_sph
        i8_array(i)%i8_IO => sph_IO(i)%inod_gl_sph
!
        iv_array(i)%n1 = sph_IO(i)%numnod_sph
        iv_array(i)%n2 = sph_IO(i)%numdir_sph
        iv_array(i)%iv_IO => sph_IO(i)%idx_gl_sph
      end do
!
      end subroutine link_sph_node_4_mpi_IO
!
!------------------------------------------------------------------
!
      end module link_sph_data_2_IO_buffer
