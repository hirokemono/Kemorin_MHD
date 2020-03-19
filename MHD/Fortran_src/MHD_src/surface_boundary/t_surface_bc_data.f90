!>@file   t_surface_bc_data.f90
!!@brief  module t_surface_bc_data
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2009
!
!>    @brief flux boundary condition lists for MHD dynamo model
!!
!!@verbatim
!!      subroutine alloc_surf_scaler_num(flux_sf)
!!      subroutine alloc_surf_scaler_apt(flux_sf)
!!        type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf
!!      subroutine alloc_surf_scaler_dat_type(scaler_sf)
!!        type(scaler_surf_bc_data_type),  intent(inout) :: scaler_sf
!!
!!      subroutine dealloc_surf_scaler_type(flux_sf)
!!        type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf
!!      subroutine dealloc_surf_scaler_dat_type(scaler_sf)
!!        type(scaler_surf_bc_data_type),  intent(inout) :: scaler_sf
!!@endverbatim
!
      module t_surface_bc_data
!
      use m_precision
!
      implicit  none
!
      type scaler_surf_bc_data_type
        integer (kind=kint) :: ngrp_sf_dat = 0
        integer (kind=kint), allocatable :: id_grp_sf_dat(:)
      end type scaler_surf_bc_data_type
!
      type scaler_surf_flux_bc_type
        integer (kind=kint) :: ngrp_sf_fix_fx = 0
        integer (kind=kint), allocatable :: id_grp_sf_fix_fx(:)
        integer (kind=kint) :: nitem_sf_fix_fx = 0
        integer (kind=kint), allocatable :: ist_ele_sf_fix_fx(:)
        real(kind = kreal), allocatable :: sf_apt_fix_fx(:)
      end type scaler_surf_flux_bc_type
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_scaler_num(flux_sf)
!
      type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf
!
!
      allocate( flux_sf%id_grp_sf_fix_fx(flux_sf%ngrp_sf_fix_fx) )
      allocate( flux_sf%ist_ele_sf_fix_fx(0:flux_sf%ngrp_sf_fix_fx) )
!
      flux_sf%ist_ele_sf_fix_fx = 0
      if (flux_sf%ngrp_sf_fix_fx.gt.0) flux_sf%id_grp_sf_fix_fx = 0
!
      end subroutine alloc_surf_scaler_num
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_scaler_apt(flux_sf)
!
      type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf
!
!
      allocate( flux_sf%sf_apt_fix_fx(flux_sf%nitem_sf_fix_fx) )
      if(flux_sf%nitem_sf_fix_fx.gt.0) flux_sf%sf_apt_fix_fx = 0.0d0
!
      end subroutine alloc_surf_scaler_apt
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_scaler_dat_type(scaler_sf)
!
      type(scaler_surf_bc_data_type),  intent(inout) :: scaler_sf
!
!
       allocate( scaler_sf%id_grp_sf_dat(scaler_sf%ngrp_sf_dat) )
       if (scaler_sf%ngrp_sf_dat .gt. 0) scaler_sf%id_grp_sf_dat = 0
!
      end subroutine alloc_surf_scaler_dat_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_scaler_type(flux_sf)
!
      type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf
!
!
      deallocate(flux_sf%id_grp_sf_fix_fx)
      deallocate(flux_sf%ist_ele_sf_fix_fx)
      deallocate(flux_sf%sf_apt_fix_fx)
!
      end subroutine dealloc_surf_scaler_type
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_scaler_dat_type(scaler_sf)
!
      type(scaler_surf_bc_data_type),  intent(inout) :: scaler_sf
!
!
      deallocate( scaler_sf%id_grp_sf_dat )
!
      end subroutine dealloc_surf_scaler_dat_type
!
!-----------------------------------------------------------------------
!
      end module t_surface_bc_data
