!t_surface_bc_data.f90
!      module t_surface_bc_data
!
!     Written by H. Matsui on Feb., 2009
!
!>    @brief flux boundary condition lists for MHD dynamo model
!
!
!>
!>        (module m_surf_data_temp)
!>        (module m_surf_data_composition)
!>        (module m_surf_data_press)
!>        (module m_surf_data_magne_p)
!>        (module m_surf_data_torque)
!>        (module m_surf_data_vector_p)
!>        (module m_surf_data_magne)
!>        (module m_surf_data_current)
!>
!
!      subroutine alloc_surf_scaler_type(flux_sf)
!        type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf
!      subroutine alloc_surf_vector_type(flux_sf)
!        type(vector_surf_flux_bc_type), intent(inout) :: flux_sf
!      subroutine alloc_surf_scaler_dat_type(scaler_sf)
!        type(scaler_surf_bc_data_type),  intent(inout) :: scaler_sf
!      subroutine alloc_surf_vector_dat_type(vector_sf)
!        type(vector_surf_bc_data_type),  intent(inout) :: vector_sf
!
!      subroutine dealloc_surf_scaler_type(flux_sf)
!        type(scaler_surf_flux_bc_type), intent(inout) :: flux_sf
!      subroutine dealloc_surf_vector_type(flux_sf)
!        type(vector_surf_flux_bc_type), intent(inout) :: flux_sf
!      subroutine dealloc_surf_scaler_dat_type(scaler_sf)
!        type(scaler_surf_bc_data_type),  intent(inout) :: scaler_sf
!      subroutine dealloc_surf_vector_dat_type(vector_sf)
!        type(vector_surf_bc_data_type),  intent(inout) :: vector_sf
!
      module t_surface_bc_data
!
      use m_precision
!
      implicit  none
!
      type scaler_surf_bc_data_type
        integer (kind=kint) :: ngrp_sf_dat
        integer (kind=kint), pointer :: id_grp_sf_dat(:)
      end type scaler_surf_bc_data_type
!
      type scaler_surf_flux_bc_type
        integer (kind=kint) :: ngrp_sf_fix_fx
        integer (kind=kint), pointer :: id_grp_sf_fix_fx(:)
        integer (kind=kint) :: nitem_sf_fix_fx
        integer (kind=kint), pointer :: ist_ele_sf_fix_fx(:)
      end type scaler_surf_flux_bc_type
!
!
      type vector_surf_bc_data_type
        integer (kind=kint) :: nmax_sf_dat
        integer (kind=kint) :: ngrp_sf_dat(3)
        integer (kind=kint), pointer :: id_grp_sf_dat(:,:)
      end type vector_surf_bc_data_type
!
      type vector_surf_flux_bc_type
        integer (kind=kint) :: nmax_sf_fix_fx
        integer (kind=kint) :: ngrp_sf_fix_fx(3)
        integer (kind=kint), pointer :: id_grp_sf_fix_fx(:,:)
        integer (kind=kint) :: nmax_ele_sf_fix_fx
        integer (kind=kint) :: nitem_sf_fix_fx(3)
        integer (kind=kint), pointer :: ist_ele_sf_fix_fx(:,:)
      end type vector_surf_flux_bc_type
!
      type velocity_surf_bc_type
        type(vector_surf_bc_data_type) :: sgs
        type(scaler_surf_flux_bc_type) :: normal
        type(vector_surf_flux_bc_type) :: grad
        type(vector_surf_bc_data_type) :: torque_lead
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
        type(vector_surf_bc_data_type) :: sgs
        type(scaler_surf_flux_bc_type) :: normal
        type(vector_surf_flux_bc_type) :: grad
        type(vector_surf_bc_data_type) :: torque_lead
      end type vector_surf_bc_type
!
      type scaler_surf_bc_type
        type(scaler_surf_bc_data_type) :: sgs
        type(scaler_surf_flux_bc_type) :: flux
        type(scaler_surf_bc_data_type) :: flux_lead
      end type scaler_surf_bc_type
!
      type surface_boundarty_conditions
        type(scaler_surf_bc_type) :: temp
!
        type(velocity_surf_bc_type) :: velo
        type(velocity_surf_bc_type) :: vector_p
        type(vector_surf_bc_type) ::   magne
        type(vector_surf_bc_type) ::   current
!
        type(potential_surf_bc_type) :: press
        type(potential_surf_bc_type) :: magne_p
!
        type(scaler_surf_bc_type) :: comp_sf
      end type surface_boundarty_conditions
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_scaler_type(flux_sf)
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
      end subroutine alloc_surf_scaler_type
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_vector_type(flux_sf)
!
      type(vector_surf_flux_bc_type), intent(inout) :: flux_sf
!
!
      allocate( flux_sf%id_grp_sf_fix_fx(flux_sf%nmax_sf_fix_fx,3) )
      allocate( flux_sf%ist_ele_sf_fix_fx(0:flux_sf%nmax_sf_fix_fx,3) )
!
      flux_sf%ist_ele_sf_fix_fx = 0
      if (flux_sf%nmax_sf_fix_fx.gt.0) flux_sf%id_grp_sf_fix_fx = 0
!
      end subroutine alloc_surf_vector_type
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
      type(vector_surf_bc_data_type),  intent(inout) :: vector_sf
!
!
       allocate( vector_sf%id_grp_sf_dat(vector_sf%nmax_sf_dat,3) )
       if (vector_sf%nmax_sf_dat .gt. 0) vector_sf%id_grp_sf_dat = 0
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
      deallocate( flux_sf%id_grp_sf_fix_fx )
      deallocate( flux_sf%ist_ele_sf_fix_fx )
!
      end subroutine dealloc_surf_scaler_type
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_surf_vector_type(flux_sf)
!
      type(vector_surf_flux_bc_type), intent(inout) :: flux_sf
!
!
      deallocate( flux_sf%id_grp_sf_fix_fx )
      deallocate( flux_sf%ist_ele_sf_fix_fx )
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
      type(vector_surf_bc_data_type),  intent(inout) :: vector_sf
!
!
       deallocate( vector_sf%id_grp_sf_dat )
!
      end subroutine dealloc_surf_vector_dat_type
!
!-----------------------------------------------------------------------
!
      end module t_surface_bc_data
