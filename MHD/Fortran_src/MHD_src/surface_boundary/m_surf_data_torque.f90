!
!     module m_surf_data_torque
!.......................................................................
!
!     Written by H. Matsui
!
!      subroutine allocate_surf_data_velo
!      subroutine allocate_surf_data_torque
!
!      subroutine deallocate_surf_data_velo
!      subroutine deallocate_surf_data_torque
!
      module m_surf_data_torque
!
      use m_precision
!
      implicit  none
!
      integer (kind=kint) :: nmax_sf_sgs_velo
      integer (kind=kint) :: ngrp_sf_sgs_velo(3)
      integer (kind=kint), allocatable :: id_grp_sf_sgs_velo(:,:)
!
!
      integer (kind=kint) :: ngrp_sf_fix_vn
      integer (kind=kint), allocatable :: id_grp_sf_fix_vn(:)
      integer (kind=kint) :: nnod_sf_fix_vn
      integer (kind=kint), allocatable :: ist_nod_sf_fix_vn(:)
      real (kind=kreal), allocatable :: sf_fix_vn_apt(:)
!
!
      integer (kind=kint) :: nmax_sf_fix_tq
      integer (kind=kint) :: ngrp_sf_fix_tq(3)
      integer (kind=kint), allocatable :: id_grp_sf_fix_tq(:,:)
      integer (kind=kint) :: nmax_ele_sf_fix_tq
      integer (kind=kint) :: nele_sf_fix_tq(3)
      integer (kind=kint), allocatable :: ist_ele_sf_fix_tq(:,:)
      real (kind=kreal), allocatable :: sf_apt_fix_tq(:,:)
!
!
      integer (kind=kint) :: nmax_sf_lead_tq
      integer (kind=kint) :: ngrp_sf_lead_tq(3)
      integer (kind=kint), allocatable :: id_grp_sf_lead_tq(:,:)
!
!
      integer (kind=kint) :: ngrp_sf_fr_in
      integer (kind=kint), allocatable :: id_grp_sf_fr_in(:)
!
      integer (kind=kint) :: ngrp_sf_fr_out
      integer (kind=kint), allocatable :: id_grp_sf_fr_out(:)
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_data_velo
!
!
      allocate( id_grp_sf_sgs_velo(nmax_sf_sgs_velo,3) )
      if (nmax_sf_sgs_velo.gt.0) id_grp_sf_sgs_velo = 0
!
      allocate( id_grp_sf_fix_vn(ngrp_sf_fix_vn) )
      allocate( ist_nod_sf_fix_vn(0:ngrp_sf_fix_vn) )
      allocate( sf_fix_vn_apt(nnod_sf_fix_vn) )
!
      ist_nod_sf_fix_vn = 0
      if (ngrp_sf_fix_vn.gt.0) id_grp_sf_fix_vn = 0
      if (nnod_sf_fix_vn.gt.0) sf_fix_vn_apt = 0.0d0
!
      end subroutine allocate_surf_data_velo
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_data_torque
!
      allocate( id_grp_sf_fix_tq(nmax_sf_fix_tq,3) )
      allocate( ist_ele_sf_fix_tq(0:nmax_sf_fix_tq,3) )
      allocate( sf_apt_fix_tq(nmax_ele_sf_fix_tq,3) )
!
      ist_ele_sf_fix_tq = 0
      if (nmax_sf_fix_tq.gt.0) id_grp_sf_fix_tq = 0
      if (nmax_ele_sf_fix_tq.gt.0) sf_apt_fix_tq = 0.0d0
!
!
      allocate( id_grp_sf_lead_tq(nmax_sf_lead_tq,3) )
      if (nmax_sf_lead_tq.gt.0) id_grp_sf_lead_tq = 0
!
      allocate( id_grp_sf_fr_in(ngrp_sf_fr_in) )
      if (ngrp_sf_fr_in.gt.0) id_grp_sf_fr_in = 0
!
      allocate( id_grp_sf_fr_out(ngrp_sf_fr_out) )
      if (ngrp_sf_fr_out.gt.0) id_grp_sf_fr_out = 0
!
      end subroutine allocate_surf_data_torque
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_velo
!
!
      deallocate( id_grp_sf_sgs_velo )
!
      deallocate( id_grp_sf_fix_vn )
      deallocate( ist_nod_sf_fix_vn )
      deallocate( sf_fix_vn_apt )
!
      end subroutine deallocate_surf_data_velo
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_torque
!
      deallocate( id_grp_sf_fix_tq )
      deallocate( ist_ele_sf_fix_tq )
      deallocate( sf_apt_fix_tq )
!
      deallocate( id_grp_sf_lead_tq )
      deallocate( id_grp_sf_fr_in )
      deallocate( id_grp_sf_fr_out )
!
      end subroutine deallocate_surf_data_torque
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_torque
