!>@file   copy_sph_groups_from_IO.f90
!!@brief  module copy_sph_groups_from_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Copy sphectr grouping data from IO
!!
!!@verbatim
!!      subroutine copy_rtp_nod_grp_from_IO
!!      subroutine copy_rtp_radial_grp_from_IO
!!      subroutine copy_rtp_theta_grp_from_IO
!!      subroutine copy_rtp_zonal_grp_from_IO
!!      subroutine copy_rj_radial_grp_from_IO
!!      subroutine copy_rj_sphere_grp_from_IO
!!@endverbatim
!
!
      module copy_sph_groups_from_IO
!
      use m_precision
!
      use m_constants
      use m_group_data_sph_specr
      use m_group_data_sph_specr_IO
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_rtp_nod_grp_from_IO
!
!
      bc_rtp_grp1%num_grp =  num_bc_grp_rtp_IO
      call allocate_grp_type_num(bc_rtp_grp1)
!
      if (bc_rtp_grp1%num_grp .gt. izero) then
        bc_rtp_grp1%grp_name(1:bc_rtp_grp1%num_grp)                     &
     &        = name_bc_grp_rtp_IO(1:bc_rtp_grp1%num_grp)
        bc_rtp_grp1%istack_grp(0:bc_rtp_grp1%num_grp)                   &
     &        = istack_bc_grp_rtp_IO(0:bc_rtp_grp1%num_grp)
!
        call allocate_grp_type_item(bc_rtp_grp1)
!
        bc_rtp_grp1%item_grp(1:bc_rtp_grp1%num_item)                    &
     &        = item_bc_grp_rtp_IO(1:bc_rtp_grp1%num_item)
      else
        call allocate_grp_type_item(bc_rtp_grp1)
      end if
!
      call deallocate_rtp_nod_grp_IO_item
!
      end subroutine copy_rtp_nod_grp_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_rtp_radial_grp_from_IO
!
!
      radial_rtp_grp1%num_grp =  num_radial_grp_rtp_IO
      call allocate_rtp_r_grp_stack
!
      if (radial_rtp_grp1%num_grp .gt. izero) then
        radial_rtp_grp1%grp_name(1:radial_rtp_grp1%num_grp)             &
     &        = name_radial_grp_rtp_IO(1:radial_rtp_grp1%num_grp)
        radial_rtp_grp1%istack_grp(0:radial_rtp_grp1%num_grp)           &
     &        = istack_radial_grp_rtp_IO(0:radial_rtp_grp1%num_grp)
!
        call allocate_rtp_r_grp_item
!
        radial_rtp_grp1%item_grp(1:radial_rtp_grp1%num_item)            &
     &        = item_radial_grp_rtp_IO(1:radial_rtp_grp1%num_item)
      else
        call allocate_rtp_r_grp_item
      end if
!
      call deallocate_rtp_r_grp_IO_item
!
      end subroutine copy_rtp_radial_grp_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_rtp_theta_grp_from_IO
!
!
      theta_rtp_grp1%num_grp =  num_theta_grp_rtp_IO
      call allocate_rtp_theta_grp_stack
!
      if (theta_rtp_grp1%num_grp .gt. izero) then
        theta_rtp_grp1%grp_name(1:theta_rtp_grp1%num_grp)               &
     &        = name_theta_grp_rtp_IO(1:theta_rtp_grp1%num_grp)
        theta_rtp_grp1%istack_grp(0:theta_rtp_grp1%num_grp)             &
     &        = istack_theta_grp_rtp_IO(0:theta_rtp_grp1%num_grp)
!
        call allocate_rtp_theta_grp_item
!
        theta_rtp_grp1%item_grp(1:theta_rtp_grp1%num_item)              &
     &        = item_theta_grp_rtp_IO(1:theta_rtp_grp1%num_item)
      else
        call allocate_rtp_theta_grp_item
      end if
!
      call deallocate_rtp_t_grp_IO_item
!
      end subroutine copy_rtp_theta_grp_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_rtp_zonal_grp_from_IO
!
!
      zonal_rtp_grp%num_grp =  num_zonal_grp_rtp_IO
      call allocate_rtp_zonal_grp_stack
!
      if (zonal_rtp_grp%num_grp .gt. izero) then
        zonal_rtp_grp%grp_name(1:zonal_rtp_grp%num_grp)                 &
     &        = name_zonal_grp_rtp_IO(1:zonal_rtp_grp%num_grp)
        zonal_rtp_grp%istack_grp(0:zonal_rtp_grp%num_grp)               &
     &        = istack_zonal_grp_rtp_IO(0:zonal_rtp_grp%num_grp)
!
        call allocate_rtp_zonal_grp_item
!
        zonal_rtp_grp%item_grp(1:zonal_rtp_grp%num_item)                &
     &        = item_zonal_grp_rtp_IO(1:zonal_rtp_grp%num_item)
      else
        zonal_rtp_grp%num_item = 0
        call allocate_rtp_zonal_grp_item
      end if
!
      call deallocate_rtp_p_grp_IO_item
!
      end subroutine copy_rtp_zonal_grp_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_rj_radial_grp_from_IO
!
!
      radial_rj_grp1%num_grp =  num_radial_grp_rj_IO
      call allocate_rj_r_grp_stack
!
      if (radial_rj_grp1%num_grp .gt. izero) then
        radial_rj_grp1%grp_name(1:radial_rj_grp1%num_grp)               &
     &        = name_radial_grp_rj_IO(1:radial_rj_grp1%num_grp)
        radial_rj_grp1%istack_grp(0:radial_rj_grp1%num_grp)             &
     &        = istack_radial_grp_rj_IO(0:radial_rj_grp1%num_grp)
!
        call allocate_rj_r_grp_item
!
        radial_rj_grp1%item_grp(1:radial_rj_grp1%num_item)              &
     &        = item_radial_grp_rj_IO(1:radial_rj_grp1%num_item)
      else
        call allocate_rj_r_grp_item
      end if
!
      call deallocate_rj_r_grp_IO_item
!
      end subroutine copy_rj_radial_grp_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_rj_sphere_grp_from_IO
!
!
      sphere_rj_grp1%num_grp =  num_sphere_grp_rj_IO
      call allocate_rj_sphere_grp_stack
!
      if (sphere_rj_grp1%num_grp .gt. izero) then
        sphere_rj_grp1%grp_name(1:sphere_rj_grp1%num_grp)               &
     &        = name_sphere_grp_rj_IO(1:sphere_rj_grp1%num_grp)
        sphere_rj_grp1%istack_grp(0:sphere_rj_grp1%num_grp)             &
     &        = istack_sphere_grp_rj_IO(0:sphere_rj_grp1%num_grp)
!
        call allocate_rj_sphere_grp_item
!
        sphere_rj_grp1%item_grp(1:sphere_rj_grp1%num_item)              &
     &        = item_sphere_grp_rj_IO(1:sphere_rj_grp1%num_item)
      else
        call allocate_rj_sphere_grp_item
      end if
!
      call deallocate_rj_j_grp_IO_item
!
      end subroutine copy_rj_sphere_grp_from_IO
!
! -----------------------------------------------------------------------
!
      end module copy_sph_groups_from_IO
 