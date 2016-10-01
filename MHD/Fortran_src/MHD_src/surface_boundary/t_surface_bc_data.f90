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
!!      subroutine alloc_surf_vector_num(flux_sf)
!!      subroutine alloc_surf_vector_apt(flux_sf)
!!        type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf(3)
!!      subroutine alloc_surf_scaler_dat_type(scaler_sf)
!!        type(scaler_surf_bc_data_type),  intent(inout) :: scaler_sf
!!      subroutine alloc_surf_vector_dat_type(vector_sf)
!!        type(scaler_surf_bc_data_type),  intent(inout) :: vector_sf(3)
!!
!!      subroutine dealloc_surf_scaler_type(flux_sf)
!!        type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf
!!      subroutine dealloc_surf_vector_type(flux_sf)
!!        type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf(3)
!!      subroutine dealloc_surf_scaler_dat_type(scaler_sf)
!!        type(scaler_surf_bc_data_type),  intent(inout) :: scaler_sf
!!      subroutine dealloc_surf_vector_dat_type(vector_sf)
!!        type(scaler_surf_bc_data_type),  intent(inout) :: vector_sf(3)
!!@endverbatim
!
      module t_surface_bc_data
!
      use m_precision
!
      implicit  none
!
      type scaler_surf_bc_data_type
        integer (kind=kint) :: ngrp_sf_dat
        integer (kind=kint), allocatable :: id_grp_sf_dat(:)
      end type scaler_surf_bc_data_type
!
      type scaler_surf_flux_bc_type
        integer (kind=kint) :: ngrp_sf_fix_fx
        integer (kind=kint), allocatable :: id_grp_sf_fix_fx(:)
        integer (kind=kint) :: nitem_sf_fix_fx
        integer (kind=kint), allocatable :: ist_ele_sf_fix_fx(:)
        real(kind = kreal), allocatable :: sf_apt_fix_fx(:)
      end type scaler_surf_flux_bc_type
!
!
      type velocity_surf_bc_type
        type(scaler_surf_bc_data_type) :: sgs(3)
        type(scaler_surf_flux_bc_type) :: normal
        type(scaler_surf_flux_bc_type) :: grad(3)
        type(scaler_surf_bc_data_type) :: torque_lead(3)
        type(scaler_surf_bc_data_type) :: free_sph_in
        type(scaler_surf_bc_data_type) :: free_sph_out
      end type velocity_surf_bc_type
!
      type potential_surf_bc_type
        type(scaler_surf_bc_data_type) :: sgs
        type(scaler_surf_flux_bc_type) :: grad
        type(scaler_surf_bc_data_type) :: grad_lead
        type(scaler_surf_bc_data_type) :: wall
        type(scaler_surf_bc_data_type) :: sph_in
        type(scaler_surf_bc_data_type) :: sph_out
      end type potential_surf_bc_type
!
      type vector_surf_bc_type
        type(scaler_surf_bc_data_type) :: sgs(3)
        type(scaler_surf_flux_bc_type) :: normal
        type(scaler_surf_flux_bc_type) :: grad(3)
        type(scaler_surf_bc_data_type) :: torque_lead(3)
      end type vector_surf_bc_type
!
      type scaler_surf_bc_type
        type(scaler_surf_bc_data_type) :: sgs
        type(scaler_surf_flux_bc_type) :: flux
        type(scaler_surf_bc_data_type) :: flux_lead
      end type scaler_surf_bc_type
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
      subroutine alloc_surf_vector_num(flux_sf)
!
      type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf(3)
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, 3
        call alloc_surf_scaler_num(flux_sf(nd))
      end do
!
      end subroutine alloc_surf_vector_num
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_vector_apt(flux_sf)
!
      type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf(3)
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, 3
        call alloc_surf_scaler_apt(flux_sf(nd))
      end do
!
      end subroutine alloc_surf_vector_apt
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
!
      subroutine alloc_surf_vector_dat_type(vector_sf)
!
      type(scaler_surf_bc_data_type),  intent(inout) :: vector_sf(3)
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, 3
        call alloc_surf_scaler_dat_type(vector_sf(nd))
      end do
!
      end subroutine alloc_surf_vector_dat_type
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
      subroutine dealloc_surf_vector_type(flux_sf)
!
      type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf(3)
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, 3
        call dealloc_surf_scaler_type(flux_sf(nd))
      end do
!
      end subroutine dealloc_surf_vector_type
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
      subroutine dealloc_surf_vector_dat_type(vector_sf)
!
      type(scaler_surf_bc_data_type),  intent(inout) :: vector_sf(3)
!
!
      integer(kind = kint) :: nd
!
      do nd = 1, 3
        call dealloc_surf_scaler_dat_type(vector_sf(3))
      end do
!
      end subroutine dealloc_surf_vector_dat_type
!
!-----------------------------------------------------------------------
!
      end module t_surface_bc_data
