!
!      module m_surf_data_press
!
!      Written by H. Matsui on Sep. 2005
!      Modified by H. Matsui on Feb., 2009
!
!       subroutine allocate_surf_press
!       subroutine allocate_surf_press_grad
!
!       subroutine deallocate_surf_press
!       subroutine deallocate_surf_press_grad
!
      module m_surf_data_press
!
      use m_precision
      use t_surface_bc_data
!
      implicit  none
!
!
      type(scaler_surf_flux_bc_type), save :: sf_bc1_grad_p
!sf_bc1_grad_p%ngrp_sf_fix_fx
!
!
      integer (kind=kint) :: ngrp_sf_sgs_p
      integer (kind=kint), allocatable :: id_grp_sf_sgs_p(:)
!
!
      integer (kind=kint) :: ngrp_sf_fix_pg
      integer (kind=kint), allocatable :: id_grp_sf_fix_pg(:)
      integer (kind=kint) :: nele_sf_fix_pg
      integer (kind=kint), allocatable :: ist_ele_sf_fix_pg(:)
      real (kind=kreal), allocatable :: sf_apt_fix_pg(:)
!
      integer (kind=kint) :: ngrp_sf_lead_p
      integer (kind=kint), allocatable :: id_grp_sf_lead_p(:)
!
!
      integer (kind=kint) :: ngrp_sf_wall_p
      integer (kind=kint), allocatable :: id_grp_sf_wall_p(:)
!
      integer (kind=kint) :: ngrp_sf_spin_p
      integer (kind=kint), allocatable :: id_grp_sf_spin_p(:)
!
      integer (kind=kint) :: ngrp_sf_spout_p
      integer (kind=kint), allocatable :: id_grp_sf_spout_p(:)
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
       subroutine allocate_surf_press
!
!
       allocate( id_grp_sf_sgs_p(ngrp_sf_sgs_p) )
       if (ngrp_sf_sgs_p/=0)  id_grp_sf_sgs_p = 0
!
       allocate( id_grp_sf_lead_p(ngrp_sf_lead_p) )
       if (ngrp_sf_lead_p/=0) id_grp_sf_lead_p = 0
!
       end subroutine allocate_surf_press
!
!-----------------------------------------------------------------------
!
       subroutine allocate_surf_press_grad
!
!
       allocate( id_grp_sf_fix_pg(ngrp_sf_fix_pg) )
       allocate( ist_ele_sf_fix_pg(0:ngrp_sf_fix_pg) )
       allocate( sf_apt_fix_pg(nele_sf_fix_pg) )
!
       ist_ele_sf_fix_pg = 0
       if (ngrp_sf_fix_pg .gt. 0) id_grp_sf_fix_pg = 0
       if (nele_sf_fix_pg .gt. 0) sf_apt_fix_pg = 0.0d0
!
!
       allocate( id_grp_sf_wall_p(ngrp_sf_wall_p) )
       if (ngrp_sf_wall_p .gt. 0) id_grp_sf_wall_p = 0
!
       allocate( id_grp_sf_spin_p(ngrp_sf_spin_p) )
       if (ngrp_sf_spin_p .gt. 0) id_grp_sf_spin_p = 0
!
       allocate( id_grp_sf_spout_p(ngrp_sf_spout_p) )
       if (ngrp_sf_spout_p .gt. 0) id_grp_sf_spout_p = 0
!
       end subroutine allocate_surf_press_grad
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine deallocate_surf_press
!
!
       deallocate( id_grp_sf_sgs_p )
       deallocate( id_grp_sf_lead_p )
!
       end subroutine deallocate_surf_press
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_surf_press_grad
!
!
       deallocate( id_grp_sf_fix_pg )
       deallocate( ist_ele_sf_fix_pg )
       deallocate( sf_apt_fix_pg )
!
       deallocate( id_grp_sf_wall_p )
       deallocate( id_grp_sf_spin_p )
       deallocate( id_grp_sf_spout_p )
!
       end subroutine deallocate_surf_press_grad
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_press
