!
!     module m_surf_data_magne_p
!.......................................................................
!
!      Written by H. Matsui on Sep. 2005
!      Modified by H. Matsui on Feb., 2009
!
!      subroutine allocate_surf_data_magne_p
!      subroutine allocate_surf_magp_grad
!
!      subroutine deallocate_surf_data_magne_p
!      subroutine deallocate_surf_magp_grad
!
      module m_surf_data_magne_p
!
      use m_precision
!
      implicit  none
!
! 
      integer (kind=kint) :: ngrp_sf_sgs_magp
      integer (kind=kint), allocatable :: id_grp_sf_sgs_magp(:)
!
!
      integer (kind=kint) :: ngrp_sf_fix_mpg
      integer (kind=kint), allocatable :: id_grp_sf_fix_mpg(:)
      integer (kind=kint) :: nele_sf_fix_mpg
      integer (kind=kint), allocatable :: ist_ele_sf_fix_mpg(:)
      real (kind=kreal), allocatable :: sf_apt_fix_mpg(:)
!
!
      integer (kind=kint) :: ngrp_sf_lead_mp
      integer (kind=kint), allocatable :: id_grp_sf_lead_mp(:)
!
!
      integer (kind=kint) :: ngrp_sf_wall_mp
      integer (kind=kint), allocatable :: id_grp_sf_wall_mp(:)
!
      integer (kind=kint) :: ngrp_sf_spin_mp
      integer (kind=kint), allocatable :: id_grp_sf_spin_mp(:)
!
      integer (kind=kint) :: ngrp_sf_spout_mp
      integer (kind=kint), allocatable :: id_grp_sf_spout_mp(:)
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_data_magne_p
!
!
      allocate( id_grp_sf_sgs_magp(ngrp_sf_sgs_magp) )
      if (ngrp_sf_sgs_magp.gt.0) id_grp_sf_sgs_magp = 0
!
!
      allocate( id_grp_sf_lead_mp(ngrp_sf_lead_mp) )
      if (ngrp_sf_lead_mp.gt.0) id_grp_sf_lead_mp = 0
!
      end subroutine allocate_surf_data_magne_p
!
!-----------------------------------------------------------------------
!
       subroutine allocate_surf_magp_grad
!
!
       allocate( id_grp_sf_fix_mpg(ngrp_sf_fix_mpg) )
       allocate( ist_ele_sf_fix_mpg(0:ngrp_sf_fix_mpg) )
       allocate( sf_apt_fix_mpg(nele_sf_fix_mpg) )
!
       ist_ele_sf_fix_mpg = 0
       if (ngrp_sf_fix_mpg .gt. 0) id_grp_sf_fix_mpg = 0
       if (nele_sf_fix_mpg .gt. 0) sf_apt_fix_mpg = 0.0d0
!
       allocate( id_grp_sf_wall_mp(ngrp_sf_wall_mp) )
       if (ngrp_sf_wall_mp .gt. 0) id_grp_sf_wall_mp = 0
!
       allocate( id_grp_sf_spin_mp(ngrp_sf_spin_mp) )
       if (ngrp_sf_spin_mp .gt. 0) id_grp_sf_spin_mp = 0
!
       allocate( id_grp_sf_spout_mp(ngrp_sf_spout_mp) )
       if (ngrp_sf_spout_mp .gt. 0) id_grp_sf_spout_mp = 0
!
       end subroutine allocate_surf_magp_grad
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_magne_p
!
!
      deallocate( id_grp_sf_sgs_magp )
      deallocate( id_grp_sf_lead_mp )
!
      end subroutine deallocate_surf_data_magne_p
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_surf_magp_grad
!
!
       deallocate( id_grp_sf_fix_mpg )
       deallocate( ist_ele_sf_fix_mpg )
       deallocate( sf_apt_fix_mpg )
!
       deallocate( id_grp_sf_wall_mp )
       deallocate( id_grp_sf_spin_mp )
       deallocate( id_grp_sf_spout_mp )
!
       end subroutine deallocate_surf_magp_grad
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_magne_p
