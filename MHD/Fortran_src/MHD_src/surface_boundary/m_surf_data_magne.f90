!
!     module m_surf_data_magne
!.......................................................................
!
!     Written by H. Matsui
!
!      subroutine allocate_surf_data_magne
!      subroutine deallocate_surf_data_magne
!
      module m_surf_data_magne
!
      use m_precision
      use t_surface_bc_data
!
      implicit  none
!
!
      type(scaler_surf_flux_bc_type), save :: sf_bc1_grad_b(3)
!
      type(scaler_surf_flux_bc_type), save :: sf_bc1_norm_b
!
      type(scaler_surf_bc_data_type), save :: sf_sgs1_grad_b(3)
!
      type(scaler_surf_bc_data_type), save :: sf_bc1_lead_b(3)
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_data_magne
!
!
      call alloc_surf_vector_num(sf_bc1_grad_b)
      call alloc_surf_vector_dat_type(sf_sgs1_grad_b)
      call alloc_surf_scaler_num(sf_bc1_norm_b)
      call alloc_surf_vector_dat_type(sf_bc1_lead_b)
!
      call alloc_surf_vector_apt(sf_bc1_grad_b)
      call alloc_surf_scaler_apt(sf_bc1_norm_b)
!
      end subroutine allocate_surf_data_magne
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_magne
!
!
      call dealloc_surf_vector_type(sf_bc1_grad_b)
      call dealloc_surf_vector_dat_type(sf_sgs1_grad_b)
      call dealloc_surf_scaler_type(sf_bc1_norm_b)
      call dealloc_surf_vector_dat_type(sf_bc1_lead_b)
!
      end subroutine deallocate_surf_data_magne
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_magne
