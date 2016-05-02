!>@file   copy_sph_groups_from_IO.f90
!!@brief  module copy_sph_groups_from_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Copy sphectr grouping data from IO
!!
!!@verbatim
!!      subroutine copy_rtp_nod_grp_from_IO(bc_rtp_grp)
!!      subroutine copy_rtp_radial_grp_from_IO(radial_rtp_grp)
!!      subroutine copy_rtp_theta_grp_from_IO(theta_rtp_grp)
!!      subroutine copy_rtp_zonal_grp_from_IO(zonal_rtp_grp)
!!      subroutine copy_rj_radial_grp_from_IO(radial_rj_grp)
!!      subroutine copy_rj_sphere_grp_from_IO(sphere_rj_grp)
!!@endverbatim
!
!
      module copy_sph_groups_from_IO
!
      use m_precision
      use m_constants
!
      use t_group_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_rj_group_data(rj_grp_IO, sph_rj_grp)
!
      type(group_data), intent(inout) :: rj_grp_IO
      type(group_data), intent(inout) :: sph_rj_grp
!
!
      sph_rj_grp%num_grp =  rj_grp_IO%num_grp
      call allocate_grp_type_num(sph_rj_grp)
!
      if (sph_rj_grp%num_grp .gt. izero) then
        sph_rj_grp%grp_name(1:sph_rj_grp%num_grp)                       &
     &        = rj_grp_IO%grp_name(1:sph_rj_grp%num_grp)
        sph_rj_grp%istack_grp(0:sph_rj_grp%num_grp)                     &
     &        = rj_grp_IO%istack_grp(0:sph_rj_grp%num_grp)
!
        call allocate_grp_type_item(sph_rj_grp)
!
        sph_rj_grp%item_grp(1:sph_rj_grp%num_item)                      &
     &        = rj_grp_IO%item_grp(1:sph_rj_grp%num_item)
      else
        call allocate_grp_type_item(sph_rj_grp)
      end if
!
      call deallocate_grp_type(rj_grp_IO)
!
      end subroutine copy_rj_group_data
!
! -----------------------------------------------------------------------
!
      end module copy_sph_groups_from_IO
 