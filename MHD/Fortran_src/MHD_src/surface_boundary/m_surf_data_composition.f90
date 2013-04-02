!m_surf_data_composition.f90
!     module m_surf_data_composition
!.......................................................................
!
!     Written by H. Matsui
!
!      subroutine allocate_surf_data_composit
!      subroutine deallocate_surf_data_composit
!
      module m_surf_data_composition
!
      use m_precision
!
      implicit  none
!
      integer (kind=kint) :: ngrp_sf_sgs_cmg
      integer (kind=kint), allocatable :: id_grp_sf_sgs_cmg(:)
!
! 
      integer (kind=kint) :: ngrp_sf_fix_cmg
      integer (kind=kint), allocatable :: id_grp_sf_fix_cmg(:)
      integer (kind=kint) :: nele_sf_fix_cmg
      integer (kind=kint), allocatable :: ist_ele_sf_fix_cmg(:)
      real (kind=kreal), allocatable :: sf_apt_fix_cmg(:)
!
!
      integer (kind=kint) :: ngrp_sf_lead_cmg
      integer (kind=kint), allocatable :: id_grp_sf_lead_cmg(:)
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_data_composit
!
!
      allocate( id_grp_sf_sgs_cmg(ngrp_sf_sgs_cmg) )
      if (ngrp_sf_sgs_cmg.gt.0) id_grp_sf_sgs_cmg = 0
!
      allocate( id_grp_sf_fix_cmg(ngrp_sf_fix_cmg) )
      allocate( ist_ele_sf_fix_cmg(0:ngrp_sf_fix_cmg) )
      allocate( sf_apt_fix_cmg(nele_sf_fix_cmg) )
!
      ist_ele_sf_fix_cmg = 0
      if (ngrp_sf_fix_cmg.gt.0) id_grp_sf_fix_cmg = 0
      if (nele_sf_fix_cmg.gt.0) sf_apt_fix_cmg = 0.0d0
!
!
      allocate( id_grp_sf_lead_cmg(ngrp_sf_lead_cmg) )
      if (ngrp_sf_lead_cmg.gt.0) id_grp_sf_lead_cmg = 0
!
      end subroutine allocate_surf_data_composit
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_composit
!
!
      deallocate( id_grp_sf_sgs_cmg )
!
      deallocate( id_grp_sf_fix_cmg )
      deallocate( ist_ele_sf_fix_cmg )
      deallocate( sf_apt_fix_cmg )
!
      deallocate( id_grp_sf_lead_cmg )
!
      end subroutine deallocate_surf_data_composit
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_composition
