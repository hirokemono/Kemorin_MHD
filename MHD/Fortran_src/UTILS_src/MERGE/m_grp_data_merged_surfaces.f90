!
!      module m_grp_data_merged_surfaces
!
!      Written by H. Matsui on Jan., 2007
!
!!      subroutine allocate_n_iso_surf_4_ele_grp(merged_grp)
!!      subroutine allocate_iso_surf_4_egrp_m
!!      subroutine allocate_iso_surf_4_egrp_tmp
!!
!!      subroutine allocate_iso_surf_4_sgrp_m(merged_grp)
!!        type(mesh_groups), intent(in) :: merged_grp
!!
!!      subroutine deallocate_n_iso_surf_4_ele_grp
!!      subroutine deallocate_iso_surf_4_egrp_m
!!      subroutine deallocate_iso_surf_4_egrp_tmp
!!
!!      subroutine check_merged_isurf_4_ele_grp(merged_grp)
!!      subroutine check_merged_isurf_4_surf_grp(merged_grp)
!!        type(mesh_groups), intent(in) :: merged_grp
!
      module m_grp_data_merged_surfaces
!
      use m_precision
!
      implicit    none
!
      integer(kind=kint ) ::  ntot_sf_iso_ele_grp_m
      integer(kind=kint ), allocatable :: num_sf_iso_ele_grp_m(:)
      integer(kind=kint ), allocatable :: istack_sf_iso_ele_grp_m(:)
!
      integer(kind=kint ), allocatable :: isf_isolate_ele_grp_m(:)
      integer(kind=kint ), allocatable :: isf_isolate_ele_grp_tmp(:)
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
      subroutine allocate_n_iso_surf_4_ele_grp(merged_grp)
!
      use t_mesh_data
!
      type(mesh_groups), intent(in) :: merged_grp
!
!
      allocate( num_sf_iso_ele_grp_m(merged_grp%ele_grp%num_grp) )
      allocate( istack_sf_iso_ele_grp_m(0:merged_grp%ele_grp%num_grp) )
!
      num_sf_iso_ele_grp_m = 0
      istack_sf_iso_ele_grp_m = 0
!
      end subroutine allocate_n_iso_surf_4_ele_grp
!
! ------------------------------------------------------
!
      subroutine allocate_iso_surf_4_egrp_m
!
      allocate( isf_isolate_ele_grp_m(ntot_sf_iso_ele_grp_m) )
      isf_isolate_ele_grp_m = 0
!
      end subroutine allocate_iso_surf_4_egrp_m
!
! ------------------------------------------------------
!
      subroutine allocate_iso_surf_4_egrp_tmp
!
      allocate( isf_isolate_ele_grp_tmp(ntot_sf_iso_ele_grp_m) )
      isf_isolate_ele_grp_tmp = 0
!
      end subroutine allocate_iso_surf_4_egrp_tmp
!
! ------------------------------------------------------
!
      subroutine allocate_iso_surf_4_sgrp_m(merged_grp)
!
      use t_mesh_data
!
      type(mesh_groups), intent(in) :: merged_grp
!
      allocate( isf_surf_grp_m(merged_grp%surf_grp%num_item) )
      isf_surf_grp_m = 0
!
      end subroutine allocate_iso_surf_4_sgrp_m
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine deallocate_n_iso_surf_4_ele_grp
!
      deallocate( num_sf_iso_ele_grp_m )
      deallocate( istack_sf_iso_ele_grp_m )
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
      subroutine deallocate_iso_surf_4_egrp_tmp
!
      deallocate( isf_isolate_ele_grp_tmp )
!
      end subroutine deallocate_iso_surf_4_egrp_tmp
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
! ------------------------------------------------------
!
      subroutine check_merged_isurf_4_ele_grp(merged_grp)
!
      use t_mesh_data
!
      type(mesh_groups), intent(in) :: merged_grp
!
      integer(kind = kint) :: igrp, ist, ied
!
!
      write(50,*) 'num_mat_new', merged_grp%ele_grp%num_grp
      write(50,*) 'istack_sf_iso_ele_grp_m',                            &
     &           istack_sf_iso_ele_grp_m(1:merged_grp%ele_grp%num_grp)
!
      do igrp = 1, merged_grp%ele_grp%num_grp
        write(50,*) 'isf_isolate_ele_grp_m', igrp
        ist = istack_sf_iso_ele_grp_m(igrp-1) + 1
        ied = istack_sf_iso_ele_grp_m(igrp)
        write(50,'(8i16)') isf_isolate_ele_grp_m(ist:ied)
      end do
!
      end subroutine check_merged_isurf_4_ele_grp
!
!   ---------------------------------------------------------------------!
      subroutine check_merged_isurf_4_surf_grp(merged_grp)
!
      use t_mesh_data
!
      type(mesh_groups), intent(in) :: merged_grp
!
      integer(kind = kint) :: igrp, ist, ied
!
!
      write(50,*) 'num_surf_new', merged_grp%surf_grp%num_grp
      write(50,*) 'surf_istack_new',                                   &
     &   merged_grp%surf_grp%istack_grp(1:merged_grp%surf_grp%num_grp)
!
      do igrp = 1, merged_grp%surf_grp%num_grp
        write(50,*) 'isf_surf_grp_m', igrp
        ist = merged_grp%surf_grp%istack_grp(igrp-1) + 1
        ied = merged_grp%surf_grp%istack_grp(igrp)
        write(50,'(8i16)') isf_surf_grp_m(ist:ied)
      end do
!
      end subroutine check_merged_isurf_4_surf_grp
!
!   ---------------------------------------------------------------------!
      end module m_grp_data_merged_surfaces
