!>@file   set_sph_groups.f90
!!@brief  module set_sph_groups
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Set groups for spherical harmonics indexing
!!
!!@verbatim
!!      subroutine set_sph_rtp_groups
!!      subroutine set_sph_rj_groups
!!@endverbatim
!
      module set_sph_groups
!
      use m_precision
!
      use m_spheric_parameter
      use m_group_data_sph_specr
!
      implicit none
!
      private :: set_rtp_radial_grp
      private :: set_rj_spectr_grp
      private :: set_no_rtp_node_grp
      private :: set_no_rtp_meridian_grp, set_no_rtp_zonal_grp
      private :: count_sph_radial_group
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_rtp_groups
!
!
!      write(*,*) 'set_rtp_radial_grp'
      call set_rtp_radial_grp
!      write(*,*) 'set_no_rtp_meridian_grp'
      call set_no_rtp_meridian_grp
!      write(*,*) 'set_no_rtp_zonal_grp'
      call set_no_rtp_zonal_grp
!
!      write(*,*) 'set_no_rtp_node_grp'
      call set_no_rtp_node_grp
!
      end subroutine set_sph_rtp_groups
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_rj_groups
!
!
!      write(*,*) 'set_rj_radial_grp'
      call set_rj_radial_grp
!      write(*,*) 'set_rj_spectr_grp'
      call set_rj_spectr_grp
!
      end subroutine set_sph_rj_groups
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_rtp_radial_grp
!
      use m_sph_1d_global_index
      use set_stack_4_sph_groups
      use set_item_4_sph_groups
!
!
      call count_sph_radial_group(radial_rtp_grp1%num_grp)
!
      call allocate_grp_type_num(radial_rtp_grp1)
      call set_stack_rtp_radial_grp
!
      call allocate_grp_type_item(radial_rtp_grp1)
      call set_item_rtp_radial_grp(sph_rtp1, radial_rtp_grp1)
!
      end subroutine set_rtp_radial_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_rj_radial_grp
!
      use set_stack_4_sph_groups
      use set_item_4_sph_groups
!
!
      call count_sph_radial_group(radial_rj_grp1%num_grp)
      call allocate_grp_type_num(radial_rj_grp1)
      call set_stack_rj_radial_grp
!
      call allocate_grp_type_item(radial_rj_grp1)
      call set_item_rj_radial_grp(sph_rj1, radial_rj_grp1)
!
      end subroutine set_rj_radial_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_rj_spectr_grp
!
      use set_stack_4_sph_groups
      use set_item_4_sph_groups
!
!
      sphere_rj_grp1%num_grp =  4
!      write(*,*) 'allocate_rj_sphere_grp_stack'
      call allocate_grp_type_num(sphere_rj_grp1)
!      write(*,*) 'set_stack_rj_spectr_grp'
      call set_stack_rj_spectr_grp
!
!      write(*,*) 'allocate_rj_sphere_grp_item'
      call allocate_grp_type_item(sphere_rj_grp1)
!      write(*,*) 'set_item_rj_spectr_grp'
      call set_item_rj_spectr_grp(sph_rj1, sphere_rj_grp1)
!
      end subroutine set_rj_spectr_grp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_no_rtp_node_grp
!
      bc_rtp_grp1%num_grp =  0
      bc_rtp_grp1%num_item = 0
      call allocate_grp_type_num(bc_rtp_grp1)
      call allocate_grp_type_item(bc_rtp_grp1)
!
      end subroutine set_no_rtp_node_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_no_rtp_meridian_grp
!
      theta_rtp_grp1%num_grp =  0
      theta_rtp_grp1%num_item = 0
      call allocate_grp_type_num(theta_rtp_grp1)
      call allocate_grp_type_item(theta_rtp_grp1)
!
      end subroutine set_no_rtp_meridian_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_no_rtp_zonal_grp
!
      zonal_rtp_grp%num_grp =  0
      zonal_rtp_grp%num_item = 0
      call allocate_grp_type_num(zonal_rtp_grp)
      call allocate_grp_type_item(zonal_rtp_grp)
!
      end subroutine set_no_rtp_zonal_grp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_sph_radial_group(num_grp)
!
      use m_sph_1d_global_index
!
      integer(kind = kint), intent(inout) :: num_grp
!
!
      num_grp =  3 + numlayer_sph_bc
      if (nlayer_2_center .gt. 0)             num_grp =  num_grp + 2
      if (sph_rtp1%nidx_global_rtp(1) .gt. nlayer_CMB) then
        num_grp =  num_grp + 1
      end if
      if (nlayer_mid_OC .gt. 0)               num_grp =  num_grp + 1
!
      end subroutine count_sph_radial_group
!
! -----------------------------------------------------------------------
!
      end module set_sph_groups
