!
!     module m_surf_data_magne
!.......................................................................
!
!     Written by H. Matsui
!
!      subroutine allocate_surf_data_magne
!      subroutine deallocate_surf_data_magne
!
      module m_surf_data_magne
!
      use m_precision
!
      implicit  none
!
      integer (kind=kint) :: nmax_sf_sgs_magne
      integer (kind=kint) :: ngrp_sf_sgs_magne(3)
      integer (kind=kint), allocatable :: id_grp_sf_sgs_magne(:,:)
!
!
      integer (kind=kint) :: ngrp_sf_fix_bn
      integer (kind=kint), allocatable :: id_grp_sf_fix_bn(:)
      integer (kind=kint) :: nnod_sf_fix_bn
      integer (kind=kint), allocatable :: ist_nod_sf_fix_bn(:)
      real (kind=kreal), allocatable :: sf_apt_fix_bn(:)
!
!
      integer (kind=kint) :: nmax_sf_lead_b
      integer (kind=kint) :: ngrp_sf_lead_b(3)
      integer (kind=kint), allocatable :: id_grp_sf_lead_b(:,:)
!
!
      integer (kind=kint) :: nmax_sf_fix_grad_b
      integer (kind=kint) :: ngrp_sf_fix_grad_b(3)
      integer (kind=kint), allocatable :: id_grp_sf_fix_grad_b(:,:)
      integer (kind=kint) :: nmax_ele_sf_fix_grad_b
      integer (kind=kint) :: nele_sf_fix_grad_b(3)
      integer (kind=kint), allocatable :: ist_ele_sf_fix_grad_b(:,:)
      real (kind=kreal), allocatable :: sf_apt_fix_grad_b(:,:)
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_data_magne
!
!
      allocate( id_grp_sf_sgs_magne(nmax_sf_sgs_magne,3) )
      if (nmax_sf_sgs_magne.gt.0) id_grp_sf_sgs_magne = 0
!
      allocate( id_grp_sf_fix_bn(ngrp_sf_fix_bn) )
      allocate( ist_nod_sf_fix_bn(0:ngrp_sf_fix_bn) )
      allocate( sf_apt_fix_bn(nnod_sf_fix_bn) )
!
      ist_nod_sf_fix_bn = 0
      if (ngrp_sf_fix_bn.gt.0) id_grp_sf_fix_bn = 0
      if (nnod_sf_fix_bn.gt.0) sf_apt_fix_bn = 0.0d0
!
!
      allocate( id_grp_sf_fix_grad_b(nmax_sf_fix_grad_b,3) )
      allocate( ist_ele_sf_fix_grad_b(0:nmax_sf_fix_grad_b,3) )
      allocate( sf_apt_fix_grad_b(nmax_ele_sf_fix_grad_b,3) )
      ist_ele_sf_fix_grad_b = 0
      if (nmax_sf_fix_grad_b.gt.0) id_grp_sf_fix_grad_b = 0
      if (nmax_ele_sf_fix_grad_b.gt.0) sf_apt_fix_grad_b = 0.0d0
!
      allocate( id_grp_sf_lead_b(nmax_sf_lead_b,3) )
      if (nmax_sf_lead_b.gt.0) id_grp_sf_lead_b = 0
!
      end subroutine allocate_surf_data_magne
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_magne
!
!
      deallocate( id_grp_sf_sgs_magne )
!
      deallocate( id_grp_sf_fix_bn )
      deallocate( ist_nod_sf_fix_bn )
      deallocate( sf_apt_fix_bn )
!
!
      deallocate( id_grp_sf_fix_grad_b )
      deallocate( ist_ele_sf_fix_grad_b )
      deallocate( sf_apt_fix_grad_b )
!
      deallocate( id_grp_sf_lead_b )
!
      end subroutine deallocate_surf_data_magne
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_magne
