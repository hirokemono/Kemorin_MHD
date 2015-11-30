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
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_data_composit
!
!
      call alloc_surf_scaler_num(sf_bc1_grad_c)
      call alloc_surf_scaler_dat_type(sf_sgs1_grad_c)
      call alloc_surf_scaler_dat_type(sf_bc1_lead_gd_c)
!
      call alloc_surf_scaler_apt(sf_bc1_grad_c)
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
      end subroutine deallocate_surf_data_composit
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_composition
