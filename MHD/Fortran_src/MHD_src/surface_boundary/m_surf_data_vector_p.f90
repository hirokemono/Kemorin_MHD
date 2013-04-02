!
!     module m_surf_data_vector_p
!.......................................................................
!
!     Written by H. Matsui
!     Modified by H. Matsui on Feb., 2009
!
!      subroutine allocate_surf_data_vect_p
!      subroutine deallocate_surf_data_vect_p
!
      module m_surf_data_vector_p
!
      use m_precision
!
      implicit  none
!
      integer (kind=kint) :: nmax_sf_sgs_vect_p
      integer (kind=kint) :: ngrp_sf_sgs_vect_p(3)
      integer (kind=kint), allocatable :: id_grp_sf_sgs_vect_p(:,:)
!
!
      integer (kind=kint) :: ngrp_sf_fix_vpn
      integer (kind=kint), allocatable :: id_grp_sf_fix_vpn(:)
      integer (kind=kint) :: nnod_sf_fix_vpn
      integer (kind=kint), allocatable :: ist_nod_sf_fix_vpn(:)
      real (kind=kreal), allocatable :: sf_apt_fix_vpn(:)
!
!
      integer (kind=kint) :: nmax_sf_lead_vect_p
      integer (kind=kint) :: ngrp_sf_lead_vect_p(3)
      integer (kind=kint), allocatable :: id_grp_sf_lead_vect_p(:,:)
!
!
      integer (kind=kint) :: nmax_sf_fix_grad_a
      integer (kind=kint) :: ngrp_sf_fix_grad_a(3)
      integer (kind=kint), allocatable :: id_grp_sf_fix_grad_a(:,:)
      integer (kind=kint) :: nmax_ele_sf_fix_grad_a
      integer (kind=kint) :: nele_sf_fix_grad_a(3)
      integer (kind=kint), allocatable :: ist_ele_sf_fix_grad_a(:,:)
      real (kind=kreal), allocatable :: sf_apt_fix_grad_a(:,:)
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_data_vect_p
!
!
      allocate( id_grp_sf_sgs_vect_p(nmax_sf_sgs_vect_p,3) )
      if (nmax_sf_sgs_vect_p.gt.0) id_grp_sf_sgs_vect_p = 0
!
!
      allocate( id_grp_sf_fix_vpn(ngrp_sf_fix_vpn) )
      allocate( ist_nod_sf_fix_vpn(0:ngrp_sf_fix_vpn) )
      allocate( sf_apt_fix_vpn(nnod_sf_fix_vpn) )
!
      ist_nod_sf_fix_vpn = 0
      if (ngrp_sf_fix_vpn.gt.0) id_grp_sf_fix_vpn = 0
      if (nnod_sf_fix_vpn.gt.0) sf_apt_fix_vpn = 0.0d0
!
!
      allocate( id_grp_sf_fix_grad_a(nmax_sf_fix_grad_a,3) )
      allocate( ist_ele_sf_fix_grad_a(0:nmax_sf_fix_grad_a,3) )
      allocate( sf_apt_fix_grad_a(nmax_ele_sf_fix_grad_a,3) )
      ist_ele_sf_fix_grad_a = 0
      if (nmax_sf_fix_grad_a.gt.0) id_grp_sf_fix_grad_a = 0
      if (nmax_ele_sf_fix_grad_a.gt.0) sf_apt_fix_grad_a = 0.0d0
!
      allocate( id_grp_sf_lead_vect_p(nmax_sf_lead_vect_p,3) )
      if (nmax_sf_lead_vect_p.gt.0) id_grp_sf_lead_vect_p = 0
!
      end subroutine allocate_surf_data_vect_p
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_vect_p
!
!
      deallocate( id_grp_sf_sgs_vect_p )
!
      deallocate( id_grp_sf_fix_vpn )
      deallocate( ist_nod_sf_fix_vpn )
      deallocate( sf_apt_fix_vpn )
!
      deallocate( id_grp_sf_fix_grad_a )
      deallocate( ist_ele_sf_fix_grad_a )
      deallocate( sf_apt_fix_grad_a )
!
      deallocate( id_grp_sf_lead_vect_p )
!
      end subroutine deallocate_surf_data_vect_p
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_vector_p
