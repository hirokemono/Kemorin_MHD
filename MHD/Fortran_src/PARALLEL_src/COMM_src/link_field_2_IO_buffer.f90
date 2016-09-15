!>@file   link_field_2_IO_buffer.f90
!!@brief  module link_field_2_IO_buffer
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!> @brief Structure for grid and comm table for spherical transform
!!
!!@verbatim
!!      subroutine link_field_data_4_mpi_IO(nloop, fld_IO, v_array)
!!        type(sph_IO_data), intent(in) :: sph_IO(nloop)
!!        type(int8array_IO), intent(inout) :: i8_array(nloop)
!!        type(intarray_IO),  intent(inout) :: i_array(nloop)
!!        type(ivecarray_IO), intent(inout) :: iv_array(nloop)
!!        type(vectarray_IO), intent(inout) :: v_array(nloop)
!!@endverbatim
!
      module link_field_2_IO_buffer
!
      use m_precision
      use calypso_mpi
!
      use t_field_data_IO
      use t_calypso_mpi_IO_param
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine link_field_data_4_mpi_IO(nloop, fld_IO, v_array)
!
      integer(kind = kint), intent(in) :: nloop
      type(field_IO), intent(in) :: fld_IO(nloop)
      type(vectarray_IO), intent(inout) :: v_array(nloop)
!
      integer(kind = kint) :: i
!
!
      do i = 1, nloop
        v_array(i)%n1 =    fld_IO(i)%nnod_IO
        v_array(i)%n2 =    fld_IO(i)%ntot_comp_IO
        v_array(i)%v_IO => fld_IO(i)%d_IO
      end do
!
      end subroutine link_field_data_4_mpi_IO
!
!------------------------------------------------------------------
!
      end module link_field_2_IO_buffer
