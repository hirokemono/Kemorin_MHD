!>@file   copy_sph_groups_to_IO.f90
!!@brief  module copy_sph_groups_to_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Copy sphectr grouping data to IO buffer
!!
!!@verbatim
!!      subroutine copy_rtp_nod_grp_to_IO
!!      subroutine copy_rtp_radial_grp_to_IO
!!      subroutine copy_rtp_theta_grp_to_IO
!!      subroutine copy_rtp_zonal_grp_to_IO
!!      subroutine copy_rj_radial_grp_to_IO
!!      subroutine copy_rj_sphere_grp_to_IO
!!@endverbatim
!
      module copy_sph_groups_to_IO
!
      use m_precision
      use m_constants
!
      use m_group_data_sph_specr
      use m_group_data_sph_specr_IO
!
      implicit none
!
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_rtp_nod_grp_to_IO
!
!
      num_bc_grp_rtp_IO =  bc_rtp_grp1%num_grp
      call allocate_rtp_nod_grp_IO_stack
!
      if (bc_rtp_grp1%num_grp .gt. izero) then
!
        ntot_bc_grp_rtp_IO = bc_rtp_grp1%num_item
        call allocate_rtp_nod_grp_IO_item
!
        name_bc_grp_rtp_IO(1:bc_rtp_grp1%num_grp)                       &
     &        = bc_rtp_grp1%grp_name(1:bc_rtp_grp1%num_grp)
        istack_bc_grp_rtp_IO(0:bc_rtp_grp1%num_grp)                     &
     &        = bc_rtp_grp1%istack_grp(0:bc_rtp_grp1%num_grp)
        item_bc_grp_rtp_IO(1:bc_rtp_grp1%num_item)                      &
     &        = bc_rtp_grp1%item_grp(1:bc_rtp_grp1%num_item)
      else
        ntot_bc_grp_rtp_IO = 0
        call allocate_rtp_nod_grp_IO_item
      end if
!
      call deallocate_grp_type(bc_rtp_grp1)
!
      end subroutine copy_rtp_nod_grp_to_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_rtp_radial_grp_to_IO
!
!
      num_radial_grp_rtp_IO =  radial_rtp_grp1%num_grp
      call allocate_rtp_r_grp_IO_stack
!
      if (radial_rtp_grp1%num_grp .gt. izero) then
!
        ntot_radial_grp_rtp_IO = radial_rtp_grp1%num_item
        call allocate_rtp_r_grp_IO_item
!
        name_radial_grp_rtp_IO(1:radial_rtp_grp1%num_grp)               &
     &        = radial_rtp_grp1%grp_name(1:radial_rtp_grp1%num_grp)
        istack_radial_grp_rtp_IO(0:radial_rtp_grp1%num_grp)             &
     &        = radial_rtp_grp1%istack_grp(0:radial_rtp_grp1%num_grp)
        item_radial_grp_rtp_IO(1:radial_rtp_grp1%num_item)              &
     &        = radial_rtp_grp1%item_grp(1:radial_rtp_grp1%num_item)
      else
        ntot_radial_grp_rtp_IO = 0
        call allocate_rtp_r_grp_IO_item
      end if
!
      call deallocate_rtp_r_grp_item
!
      end subroutine copy_rtp_radial_grp_to_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_rtp_theta_grp_to_IO
!
!
      num_theta_grp_rtp_IO =  theta_rtp_grp1%num_grp
      call allocate_rtp_t_grp_IO_stack
!
      if (theta_rtp_grp1%num_grp .gt. izero) then
!
        ntot_theta_grp_rtp_IO = theta_rtp_grp1%num_item
        call allocate_rtp_t_grp_IO_item
!
        name_theta_grp_rtp_IO(1:theta_rtp_grp1%num_grp)                 &
     &        = theta_rtp_grp1%grp_name(1:theta_rtp_grp1%num_grp)
        istack_theta_grp_rtp_IO(0:theta_rtp_grp1%num_grp)               &
     &        = theta_rtp_grp1%istack_grp(0:theta_rtp_grp1%num_grp)
        item_theta_grp_rtp_IO(1:theta_rtp_grp1%num_item)                &
     &        = theta_rtp_grp1%item_grp(1:theta_rtp_grp1%num_item)
      else
        ntot_theta_grp_rtp_IO = 0
        call allocate_rtp_t_grp_IO_item
      end if
!
      call deallocate_rtp_theta_grp_item
!
      end subroutine copy_rtp_theta_grp_to_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_rtp_zonal_grp_to_IO
!
!
      num_zonal_grp_rtp_IO =  zonal_rtp_grp%num_grp
      call allocate_rtp_p_grp_IO_stack
!
      if (zonal_rtp_grp%num_grp .gt. izero) then
!
        ntot_zonal_grp_rtp_IO = zonal_rtp_grp%num_item
        call allocate_rtp_p_grp_IO_item
!
        name_zonal_grp_rtp_IO(1:zonal_rtp_grp%num_grp)                  &
     &        = zonal_rtp_grp%grp_name(1:zonal_rtp_grp%num_grp)
        istack_zonal_grp_rtp_IO(0:zonal_rtp_grp%num_grp)                &
     &        = zonal_rtp_grp%istack_grp(0:zonal_rtp_grp%num_grp)
        item_zonal_grp_rtp_IO(1:zonal_rtp_grp%num_item)                 &
     &        = zonal_rtp_grp%item_grp(1:zonal_rtp_grp%num_item)
      else
        ntot_zonal_grp_rtp_IO = 0
        call allocate_rtp_p_grp_IO_item
      end if
!
      call deallocate_rtp_zonal_grp_item
!
      end subroutine copy_rtp_zonal_grp_to_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_rj_radial_grp_to_IO
!
!
      num_radial_grp_rj_IO =  radial_rj_grp1%num_grp
      call allocate_rj_r_grp_IO_stack
!
      if (radial_rj_grp1%num_grp .gt. izero) then
!
        ntot_radial_grp_rj_IO = radial_rj_grp1%num_item
        call allocate_rj_r_grp_IO_item
!
        name_radial_grp_rj_IO(1:radial_rj_grp1%num_grp)                 &
     &        = radial_rj_grp1%grp_name(1:radial_rj_grp1%num_grp)
        istack_radial_grp_rj_IO(0:radial_rj_grp1%num_grp)               &
     &        = radial_rj_grp1%istack_grp(0:radial_rj_grp1%num_grp)
        item_radial_grp_rj_IO(1:radial_rj_grp1%num_item)                &
     &        = radial_rj_grp1%item_grp(1:radial_rj_grp1%num_item)
      else
        ntot_radial_grp_rj_IO = 0
        call allocate_rj_r_grp_IO_item
      end if
!
      call deallocate_rj_r_grp_item
!
      end subroutine copy_rj_radial_grp_to_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_rj_sphere_grp_to_IO
!
!
      num_sphere_grp_rj_IO =  sphere_rj_grp1%num_grp
      call allocate_rj_j_grp_IO_stack
!
      if (sphere_rj_grp1%num_grp .gt. izero) then
!
        ntot_sphere_grp_rj_IO = sphere_rj_grp1%num_item
        call allocate_rj_j_grp_IO_item
!
        name_sphere_grp_rj_IO(1:sphere_rj_grp1%num_grp)                 &
     &        = sphere_rj_grp1%grp_name(1:sphere_rj_grp1%num_grp)
        istack_sphere_grp_rj_IO(0:sphere_rj_grp1%num_grp)               &
     &        = sphere_rj_grp1%istack_grp(0:sphere_rj_grp1%num_grp)
        item_sphere_grp_rj_IO(1:sphere_rj_grp1%num_item)                &
     &        = sphere_rj_grp1%item_grp(1:sphere_rj_grp1%num_item)
      else
        sphere_rj_grp1%num_item = 0
        call allocate_rj_j_grp_IO_item
      end if
!
      call deallocate_rj_sphere_grp_item
!
      end subroutine copy_rj_sphere_grp_to_IO
!
! -----------------------------------------------------------------------
!
      end module copy_sph_groups_to_IO
 