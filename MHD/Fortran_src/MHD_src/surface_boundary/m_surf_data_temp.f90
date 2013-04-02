!
!     module m_surf_data_temp
!.......................................................................
!
!     Written by H. Matsui
!
!      subroutine allocate_surf_data_temp
!      subroutine deallocate_surf_data_temp
!
      module m_surf_data_temp
!
      use m_precision
!
      implicit  none
!
!
      integer (kind=kint) :: ngrp_sf_sgs_temp
      integer (kind=kint), allocatable :: id_grp_sf_sgs_temp(:)
!
!
      integer (kind=kint) :: ngrp_sf_fix_hf
      integer (kind=kint), allocatable :: id_grp_sf_fix_hf(:)
      integer (kind=kint) :: nele_sf_fix_hf
      integer (kind=kint), allocatable :: ist_ele_sf_fix_hf(:)
      real (kind=kreal), allocatable :: sf_apt_fix_hf(:)
!
!
      integer (kind=kint) :: ngrp_sf_lead_hf
      integer (kind=kint), allocatable :: id_grp_sf_lead_hf(:)
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_data_temp
!
!
      allocate( id_grp_sf_sgs_temp(ngrp_sf_sgs_temp) )
      if (ngrp_sf_sgs_temp.gt.0) id_grp_sf_sgs_temp = 0
!
      allocate( id_grp_sf_fix_hf(ngrp_sf_fix_hf) )
      allocate( ist_ele_sf_fix_hf(0:ngrp_sf_fix_hf) )
      allocate( sf_apt_fix_hf(nele_sf_fix_hf) )
!
      ist_ele_sf_fix_hf = 0
      if (ngrp_sf_fix_hf.gt.0) id_grp_sf_fix_hf = 0
      if (nele_sf_fix_hf.gt.0) sf_apt_fix_hf = 0.0d0
!
!
      allocate( id_grp_sf_lead_hf(ngrp_sf_lead_hf) )
      if (ngrp_sf_lead_hf.gt.0) id_grp_sf_lead_hf = 0
!
      end subroutine allocate_surf_data_temp
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_temp
!
!
      deallocate( id_grp_sf_sgs_temp )
!
      deallocate( id_grp_sf_fix_hf )
      deallocate( ist_ele_sf_fix_hf )
      deallocate( sf_apt_fix_hf )
!
      deallocate( id_grp_sf_lead_hf )
!
      end subroutine deallocate_surf_data_temp
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_temp
