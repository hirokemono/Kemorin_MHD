!
!     module m_surf_data_infinity
!.......................................................................
!
!     written by H. Matsui
!
!      subroutine allocate_infty_surf_ctl
!      subroutine deallocate_infty_surf_ctl
!      subroutine const_bc_infinity_surf_grp
!
      module m_surf_data_infinity
!
      use m_precision
!
      implicit  none
!
      integer (kind=kint) :: ngrp_sf_infty
      integer (kind=kint), allocatable :: id_grp_sf_infty(:)
      real (kind=kreal), allocatable :: sf_infty_apt(:)
!
      private :: allocate_surf_infinity
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
       subroutine allocate_surf_infinity
!
       allocate( id_grp_sf_infty(ngrp_sf_infty) )
       allocate( sf_infty_apt(ngrp_sf_infty) )
!
      if (ngrp_sf_infty.gt.0) then
        id_grp_sf_infty = 0
        sf_infty_apt = 0.0d0
      end if
!
       end subroutine allocate_surf_infinity
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_surf_infinity
!
       deallocate( id_grp_sf_infty )
       deallocate( sf_infty_apt )
!
       end subroutine deallocate_surf_infinity
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_bc_infinity_surf_grp
!
      use m_surface_group
      use const_bc_infinity_surf
!
!
      call count_num_bc_infinity(num_surf, surf_name, ngrp_sf_infty)
!
      call allocate_surf_infinity
!
      call set_bc_infty_id(num_surf, surf_name,                         &
     &    ngrp_sf_infty, id_grp_sf_infty)
!
      end subroutine const_bc_infinity_surf_grp
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_infinity
