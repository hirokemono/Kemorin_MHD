!
!      module t_grp_data_merged_surfaces
!
!      Written by H. Matsui on Jan., 2007
!
!!      subroutine alloc_n_iso_surf_4_ele_grp(ele_grp, mgd_sf_grp)
!!        type(group_data), intent(in) :: ele_grp
!!      subroutine allocate_iso_surf_4_egrp_m(mgd_sf_grp)
!!
!!      subroutine alloc_iso_surf_4_sgrp_m(surf_grp, mgd_sf_grp)
!!        type(surface_group_data), intent(in) :: surf_grp
!!
!!      subroutine dealloc_n_iso_surf_4_ele_grp(mgd_sf_grp)
!!      subroutine dealloc_iso_surf_4_egrp_m(mgd_sf_grp)
!!
!!      subroutine check_merged_isurf_4_ele_grp(ele_grp, mgd_sf_grp)
!!      subroutine check_merged_isurf_4_surf_grp(surf_grp, mgd_sf_grp)
!!        type(group_data), intent(in) :: ele_grp
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type(group_data_merged_surf), intent(in) :: mgd_sf_grp
!
      module t_grp_data_merged_surfaces
!
      use m_precision
!
      implicit    none
!
      type group_data_merged_surf
        integer(kind=kint ) ::  ntot_sf_iso_ele_grp_m
        integer(kind=kint ), allocatable :: num_sf_iso_ele_grp_m(:)
        integer(kind=kint ), allocatable :: istack_sf_iso_ele_grp_m(:)
!
        integer(kind=kint ), allocatable :: isf_isolate_ele_grp_m(:)
!
        integer(kind=kint ), allocatable :: isf_surf_grp_m(:)
      end type group_data_merged_surf
!
! ------------------------------------------------------
!
      contains
!
! ------------------------------------------------------
!
      subroutine alloc_n_iso_surf_4_ele_grp(ele_grp, mgd_sf_grp)
!
      use t_group_data
!
      type(group_data), intent(in) :: ele_grp
      type(group_data_merged_surf), intent(inout) :: mgd_sf_grp
!
!
      allocate( mgd_sf_grp%num_sf_iso_ele_grp_m(ele_grp%num_grp) )
      allocate( mgd_sf_grp%istack_sf_iso_ele_grp_m(0:ele_grp%num_grp) )
!
      mgd_sf_grp%num_sf_iso_ele_grp_m = 0
      mgd_sf_grp%istack_sf_iso_ele_grp_m = 0
!
      end subroutine alloc_n_iso_surf_4_ele_grp
!
! ------------------------------------------------------
!
      subroutine alloc_iso_surf_4_egrp_m(mgd_sf_grp)
!
      type(group_data_merged_surf), intent(inout) :: mgd_sf_grp
!
      integer(kind = kint) :: num
!
      num = mgd_sf_grp%ntot_sf_iso_ele_grp_m
      allocate( mgd_sf_grp%isf_isolate_ele_grp_m(num) )
      mgd_sf_grp%isf_isolate_ele_grp_m = 0
!
      end subroutine alloc_iso_surf_4_egrp_m
!
! ------------------------------------------------------
!
      subroutine alloc_iso_surf_4_sgrp_m(surf_grp, mgd_sf_grp)
!
      use t_group_data
!
      type(surface_group_data), intent(in) :: surf_grp
      type(group_data_merged_surf), intent(inout) :: mgd_sf_grp
!
      allocate( mgd_sf_grp%isf_surf_grp_m(surf_grp%num_item) )
      mgd_sf_grp%isf_surf_grp_m = 0
!
      end subroutine alloc_iso_surf_4_sgrp_m
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine dealloc_n_iso_surf_4_ele_grp(mgd_sf_grp)
!
      type(group_data_merged_surf), intent(inout) :: mgd_sf_grp
!
      deallocate( mgd_sf_grp%num_sf_iso_ele_grp_m )
      deallocate( mgd_sf_grp%istack_sf_iso_ele_grp_m )
!
      end subroutine dealloc_n_iso_surf_4_ele_grp
!
! ------------------------------------------------------
!
      subroutine dealloc_iso_surf_4_egrp_m(mgd_sf_grp)
!
      type(group_data_merged_surf), intent(inout) :: mgd_sf_grp
!
      deallocate( mgd_sf_grp%isf_isolate_ele_grp_m )
!
      end subroutine dealloc_iso_surf_4_egrp_m
!
! ------------------------------------------------------
!
      subroutine dealloc_iso_surf_4_sgrp_m(mgd_sf_grp)
!
      type(group_data_merged_surf), intent(inout) :: mgd_sf_grp
!
      deallocate( mgd_sf_grp%isf_surf_grp_m )
!
      end subroutine dealloc_iso_surf_4_sgrp_m
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine check_merged_isurf_4_ele_grp(ele_grp, mgd_sf_grp)
!
      use t_mesh_data
!
      type(group_data), intent(in) :: ele_grp
      type(group_data_merged_surf), intent(in) :: mgd_sf_grp
!
      integer(kind = kint) :: igrp, ist, ied
!
!
      write(50,*) 'num_mat_new', ele_grp%num_grp
      write(50,*) 'istack_sf_iso_ele_grp_m',                            &
     &           mgd_sf_grp%istack_sf_iso_ele_grp_m(1:ele_grp%num_grp)
!
      do igrp = 1, ele_grp%num_grp
        write(50,*) 'isf_isolate_ele_grp_m', igrp
        ist = mgd_sf_grp%istack_sf_iso_ele_grp_m(igrp-1) + 1
        ied = mgd_sf_grp%istack_sf_iso_ele_grp_m(igrp)
        write(50,'(8i16)') mgd_sf_grp%isf_isolate_ele_grp_m(ist:ied)
      end do
!
      end subroutine check_merged_isurf_4_ele_grp
!
!   ---------------------------------------------------------------------!
      subroutine check_merged_isurf_4_surf_grp(surf_grp, mgd_sf_grp)
!
      use t_group_data
!
      type(surface_group_data), intent(in) :: surf_grp
      type(group_data_merged_surf), intent(in) :: mgd_sf_grp
!
      integer(kind = kint) :: igrp, ist, ied
!
!
      write(50,*) 'num_surf_new', surf_grp%num_grp
      write(50,*) 'surf_istack_new',                                   &
     &   surf_grp%istack_grp(1:surf_grp%num_grp)
!
      do igrp = 1, surf_grp%num_grp
        write(50,*) 'isf_surf_grp_m', igrp
        ist = surf_grp%istack_grp(igrp-1) + 1
        ied = surf_grp%istack_grp(igrp)
        write(50,'(8i16)') mgd_sf_grp%isf_surf_grp_m(ist:ied)
      end do
!
      end subroutine check_merged_isurf_4_surf_grp
!
!   ---------------------------------------------------------------------!
      end module t_grp_data_merged_surfaces
