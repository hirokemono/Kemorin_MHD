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
      use t_surface_bc_data
!
      implicit  none
!
!
      type(scaler_surf_flux_bc_type), save :: sf_bc1_grad_t
!
      type(scaler_surf_bc_data_type), save :: sf_sgs1_grad_t
!
      type(scaler_surf_bc_data_type), save :: sf_bc1_lead_gd_t
!
!
      real (kind=kreal), allocatable :: sf_apt_fix_hf(:)
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
      call alloc_surf_scaler_type(sf_bc1_grad_t)
      call alloc_surf_scaler_dat_type(sf_sgs1_grad_t)
      call alloc_surf_scaler_dat_type(sf_bc1_lead_gd_t)
!
      allocate( sf_apt_fix_hf(sf_bc1_grad_t%nitem_sf_fix_fx) )
      if (sf_bc1_grad_t%nitem_sf_fix_fx.gt.0) sf_apt_fix_hf = 0.0d0
!
      end subroutine allocate_surf_data_temp
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_temp
!
!
      call dealloc_surf_scaler_type(sf_bc1_grad_t)
      call dealloc_surf_scaler_dat_type(sf_sgs1_grad_t)
      call dealloc_surf_scaler_dat_type(sf_bc1_lead_gd_t)
!
      deallocate( sf_apt_fix_hf )
!
      end subroutine deallocate_surf_data_temp
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_temp
