!
!     module m_surf_data_current
!.......................................................................
!
!     Written by H. Matsui
!
!      subroutine allocate_surf_data_current
!      subroutine deallocate_surf_data_current
!
      module m_surf_data_current
!
      use m_precision
!
      implicit  none
!
      integer (kind=kint) :: nmax_sf_sgs_current
      integer (kind=kint) :: ngrp_sf_sgs_current(3)
      integer (kind=kint), allocatable :: id_grp_sf_sgs_current(:,:)
!
      integer (kind=kint) :: ngrp_sf_fix_jn
      integer (kind=kint), allocatable :: id_grp_sf_fix_jn(:)
      integer (kind=kint) :: nnod_sf_fix_jn
      integer (kind=kint), allocatable :: ist_nod_sf_fix_jn(:)
      real (kind=kreal), allocatable :: sf_apt_fix_jn(:)
!
!
      integer (kind=kint) :: nmax_sf_lead_j
      integer (kind=kint) :: ngrp_sf_lead_j(3)
      integer (kind=kint), allocatable :: id_grp_sf_lead_j(:,:)
!
!
      integer (kind=kint) :: nmax_sf_fix_grad_j
      integer (kind=kint) :: ngrp_sf_fix_grad_j(3)
      integer (kind=kint), allocatable :: id_grp_sf_fix_grad_j(:,:)
      integer (kind=kint) :: nmax_ele_sf_fix_grad_j
      integer (kind=kint) :: nele_sf_fix_grad_j(3)
      integer (kind=kint), allocatable :: ist_ele_sf_fix_grad_j(:,:)
      real (kind=kreal), allocatable :: sf_apt_fix_grad_j(:,:)
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_data_current
!
!
      allocate( id_grp_sf_sgs_current(nmax_sf_sgs_current,3) )
      if (nmax_sf_sgs_current.gt.0) id_grp_sf_sgs_current = 0
!
!
      allocate( id_grp_sf_fix_jn(ngrp_sf_fix_jn) )
      allocate( ist_nod_sf_fix_jn(0:ngrp_sf_fix_jn) )
      allocate( sf_apt_fix_jn(nnod_sf_fix_jn) )
!
      ist_nod_sf_fix_jn = 0
      if (ngrp_sf_fix_jn.gt.0) id_grp_sf_fix_jn = 0
      if (nnod_sf_fix_jn.gt.0) sf_apt_fix_jn = 0.0d0
!
!
      allocate( id_grp_sf_lead_j(nmax_sf_lead_j,3) )
      if (nmax_sf_lead_j.gt.0) id_grp_sf_lead_j = 0
!
!
      allocate( id_grp_sf_fix_grad_j(nmax_sf_fix_grad_j,3) )
      allocate( ist_ele_sf_fix_grad_j(0:nmax_sf_fix_grad_j,3) )
      allocate( sf_apt_fix_grad_j(nmax_ele_sf_fix_grad_j,3) )
!
      ist_ele_sf_fix_grad_j = 0
      if (nmax_sf_fix_grad_j.gt.0) id_grp_sf_fix_grad_j = 0
      if (nmax_ele_sf_fix_grad_j.gt.0) sf_apt_fix_grad_j = 0.0d0
!
      end subroutine allocate_surf_data_current
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_current
!
!
      deallocate( id_grp_sf_sgs_current )
!
!
      deallocate( id_grp_sf_fix_jn )
      deallocate( ist_nod_sf_fix_jn )
      deallocate( sf_apt_fix_jn )
!
!
      deallocate( id_grp_sf_lead_j )
!
!
      deallocate( id_grp_sf_fix_grad_j )
      deallocate( ist_ele_sf_fix_grad_j )
      deallocate( sf_apt_fix_grad_j )
!
      end subroutine deallocate_surf_data_current
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_current
