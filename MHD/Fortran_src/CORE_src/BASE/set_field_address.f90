!>@file   set_field_address.f90
!!        module set_field_address
!!
!! @author H. Matsui
!! @date   Programmed on July, 2006
!! @n      Modified  on Jan., 2012
!!
!!
!> @brief Set start address for field data
!!
!!@verbatim
!!      subroutine init_field_data(n_point, fld, iphys)
!!        type(phys_data), intent(inout) :: fld
!!        type(phys_address), intent(inout) :: iphys
!!
!!@endverbatim
!!
!!@n @param fld    structure of field data
!!@n @param iphys  structure of field addresses
!
!
      module set_field_address
!
      use m_precision
!
      use t_phys_address
      use t_phys_data
!
      implicit none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine init_field_data(n_point, fld, iphys)
!
      use set_MHD_field_address
      use set_SGS_MHD_field_address
!
      integer(kind = kint), intent(in) :: n_point
      type(phys_data), intent(inout) :: fld
!
      type(phys_address), intent(inout) :: iphys
!
      integer(kind = kint) :: i, i_fld
      logical :: flag
!
!
      call alloc_phys_data_type(n_point, fld)
!
      do i = 1, fld%num_phys
        i_fld = fld%istack_component(i-1) + 1
!
        call set_MHD_field_addresses                                    &
     &     (i_fld, fld%phys_name(i), iphys, flag)
        if(flag) cycle
        call set_SGS_MHD_field_addresses                                &
     &     (i_fld, fld%phys_name(i), iphys, flag)
        if(flag) cycle
!
!   Old field label... Should be deleted later!!
        call set_old_MHD_field_addresses                                &
     &     (i_fld, fld%phys_name(i), iphys, flag)
      end do
!
      end subroutine init_field_data
!
! -------------------------------------------------------------------
!
      end module set_field_address
