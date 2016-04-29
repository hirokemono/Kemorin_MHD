!>@file   m_group_data_sph_specr.f90
!!@brief  module m_group_data_sph_specr
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Grouping information for spherical hermonics data
!!
!!@verbatim
!!      subroutine allocate_rtp_nod_grp_stack
!!      subroutine allocate_rtp_r_grp_stack
!!      subroutine allocate_rtp_theta_grp_stack
!!      subroutine allocate_rtp_zonal_grp_stack
!!      subroutine allocate_rj_r_grp_stack
!!      subroutine allocate_rj_sphere_grp_stack
!!
!!      subroutine allocate_rtp_nod_grp_item
!!      subroutine allocate_rtp_r_grp_item
!!      subroutine allocate_rtp_theta_grp_item
!!      subroutine allocate_rtp_zonal_grp_item
!!      subroutine allocate_rj_r_grp_item
!!      subroutine allocate_rj_sphere_grp_item
!!
!!      subroutine deallocate_rtp_nod_grp_item
!!      subroutine deallocate_rtp_r_grp_item
!!      subroutine deallocate_rtp_theta_grp_item
!!      subroutine deallocate_rtp_zonal_grp_item
!!      subroutine deallocate_rj_r_grp_item
!!      subroutine deallocate_rj_sphere_grp_item
!!
!!      subroutine check_rtp_nod_grp_stack(my_rank)
!!      subroutine check_rtp_r_grp_stack(my_rank)
!!      subroutine check_rtp_theta_grp_stack(my_rank)
!!      subroutine check_rtp_zonal_grp_stack(my_rank)
!!      subroutine check_rj_r_grp_stack(my_rank)
!!      subroutine check_rj_sph_grp_stack(my_rank)
!!@endverbatim
!!
!!@n @param  my_rank  Proccess ID
!
      module m_group_data_sph_specr
!
      use m_precision
      use t_group_data
!
      implicit none
!
!>       node group for grid space
      type(group_data), save :: bc_rtp_grp1
!>       radial group for grid space
      type(group_data), save :: radial_rtp_grp1
!>       meridional group for grid space
      type(group_data), save :: theta_rtp_grp1
!>       zonal group for grid space
      type(group_data), save :: zonal_rtp_grp
!
!>       radial group for sprctrum space
      type(group_data), save :: radial_rj_grp1
!>       spherical harmonics group for sprctrum space
      type(group_data), save :: sphere_rj_grp1
!sphere_rj_grp1%num_grp
!
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rtp_r_grp_stack
!
      call allocate_grp_type_num(radial_rtp_grp1)
!
      end subroutine allocate_rtp_r_grp_stack
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rtp_theta_grp_stack
!
      call allocate_grp_type_num(theta_rtp_grp1)
!
      end subroutine allocate_rtp_theta_grp_stack
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rtp_zonal_grp_stack
!
      call allocate_grp_type_num(zonal_rtp_grp)
!
      end subroutine allocate_rtp_zonal_grp_stack
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_rj_r_grp_stack
!
      call allocate_grp_type_num(radial_rj_grp1)
!
      end subroutine allocate_rj_r_grp_stack
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rj_sphere_grp_stack
!
      call allocate_grp_type_num(sphere_rj_grp1)
!
      end subroutine allocate_rj_sphere_grp_stack
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_rtp_r_grp_item
!
      call allocate_grp_type_item(radial_rtp_grp1)
!
      end subroutine allocate_rtp_r_grp_item
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rtp_theta_grp_item
!
      call allocate_grp_type_item(theta_rtp_grp1)
!
      end subroutine allocate_rtp_theta_grp_item
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rtp_zonal_grp_item
!
      call allocate_grp_type_item(zonal_rtp_grp)
!
      end subroutine allocate_rtp_zonal_grp_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_rj_r_grp_item
!
      call allocate_grp_type_item(radial_rj_grp1)
!
      end subroutine allocate_rj_r_grp_item
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rj_sphere_grp_item
!
      call allocate_grp_type_item(sphere_rj_grp1)
!
      end subroutine allocate_rj_sphere_grp_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_rtp_r_grp_item
!
      call deallocate_grp_type(radial_rtp_grp1)
!
      end subroutine deallocate_rtp_r_grp_item
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_rtp_theta_grp_item
!
      call deallocate_grp_type(theta_rtp_grp1)
!
      end subroutine deallocate_rtp_theta_grp_item
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_rtp_zonal_grp_item
!
      call deallocate_grp_type(zonal_rtp_grp)
!
      end subroutine deallocate_rtp_zonal_grp_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_rj_r_grp_item
!
      call deallocate_grp_type(radial_rj_grp1)
!
      end subroutine deallocate_rj_r_grp_item
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_rj_sphere_grp_item
!
      call deallocate_grp_type(sphere_rj_grp1)
!
      end subroutine deallocate_rj_sphere_grp_item
!
! -----------------------------------------------------------------------
!
      end module m_group_data_sph_specr
