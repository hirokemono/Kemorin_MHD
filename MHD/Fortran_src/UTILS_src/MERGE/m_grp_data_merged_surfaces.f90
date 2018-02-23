!
!      module m_grp_data_merged_surfaces
!
!      Written by H. Matsui on Jan., 2007
!
!!      subroutine allocate_n_iso_surf_4_ele_grp(ele_grp)
!!        type(group_data), intent(in) :: ele_grp
!!      subroutine allocate_iso_surf_4_egrp_m
!!      subroutine allocate_iso_surf_4_egrp_tmp
!!
!!      subroutine allocate_iso_surf_4_sgrp_m(surf_grp)
!!        type(surface_group_data), intent(in) :: surf_grp
!!
!!      subroutine deallocate_n_iso_surf_4_ele_grp
!!      subroutine deallocate_iso_surf_4_egrp_m
!!      subroutine deallocate_iso_surf_4_egrp_tmp
!!
      module m_grp_data_merged_surfaces
!
      use m_precision
      use t_grp_data_merged_surfaces
!
      implicit none
!
      type(group_data_merged_surf), save :: mgd_sf_grp1
!
      integer(kind=kint ), allocatable :: isf_isolate_ele_grp_m(:)
!
!
      integer(kind=kint ), allocatable :: isf_surf_grp_m(:)
!
! ------------------------------------------------------
!
      contains
!
! ------------------------------------------------------
!
      subroutine allocate_n_iso_surf_4_ele_grp(ele_grp)
!
      use t_group_data
!
      type(group_data), intent(in) :: ele_grp
!
!
      call alloc_n_iso_surf_4_ele_grp(ele_grp, mgd_sf_grp1)
!
      end subroutine allocate_n_iso_surf_4_ele_grp
!
! ------------------------------------------------------
!
      subroutine allocate_iso_surf_4_egrp_m
!
      allocate( isf_isolate_ele_grp_m(mgd_sf_grp1%ntot_sf_iso_ele_grp_m) )
      isf_isolate_ele_grp_m = 0
!
      end subroutine allocate_iso_surf_4_egrp_m
!
! ------------------------------------------------------
!
      subroutine allocate_iso_surf_4_sgrp_m(surf_grp)
!
      use t_group_data
!
      type(surface_group_data), intent(in) :: surf_grp
!
      allocate( isf_surf_grp_m(surf_grp%num_item) )
      isf_surf_grp_m = 0
!
      end subroutine allocate_iso_surf_4_sgrp_m
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine deallocate_n_iso_surf_4_ele_grp
!
      call dealloc_n_iso_surf_4_ele_grp(mgd_sf_grp1)
!
      end subroutine deallocate_n_iso_surf_4_ele_grp
!
! ------------------------------------------------------
!
      subroutine deallocate_iso_surf_4_egrp_m
!
      deallocate( isf_isolate_ele_grp_m )
!
      end subroutine deallocate_iso_surf_4_egrp_m
!
! ------------------------------------------------------
!
      subroutine deallocate_iso_surf_4_sgrp_m
!
      deallocate( isf_surf_grp_m )
!
      end subroutine deallocate_iso_surf_4_sgrp_m
!
! ------------------------------------------------------
!
      end module m_grp_data_merged_surfaces
