!m_surf_data_composition.f90
!     module m_surf_data_composition
!.......................................................................
!
!     Written by H. Matsui
!
!      subroutine allocate_surf_data_composit
!      subroutine deallocate_surf_data_composit
!
      module m_surf_data_composition
!
      use m_precision
      use t_surface_bc_data
!
      implicit  none
!
      type(scaler_surf_flux_bc_type), save :: sf_bc1_grad_c
!
      type(scaler_surf_bc_data_type), save :: sf_sgs1_grad_c
!
      type(scaler_surf_bc_data_type), save :: sf_bc1_lead_gd_c
!
      real (kind=kreal), allocatable :: sf_apt_fix_cmg(:)
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_data_composit
!
!
      call alloc_surf_scaler_type(sf_bc1_grad_c)
      call alloc_surf_scaler_dat_type(sf_sgs1_grad_c)
      call alloc_surf_scaler_dat_type(sf_bc1_lead_gd_c)
!
      allocate( sf_apt_fix_cmg(sf_bc1_grad_c%nitem_sf_fix_fx) )
      if (sf_bc1_grad_c%nitem_sf_fix_fx.gt.0) sf_apt_fix_cmg = 0.0d0
!
      end subroutine allocate_surf_data_composit
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_composit
!
!
      call dealloc_surf_scaler_type(sf_bc1_grad_c)
      call dealloc_surf_scaler_dat_type(sf_sgs1_grad_c)
      call dealloc_surf_scaler_dat_type(sf_bc1_lead_gd_c)
!
      deallocate( sf_apt_fix_cmg )
!
      end subroutine deallocate_surf_data_composit
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_composition
