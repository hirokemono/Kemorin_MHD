!
!     module m_surf_data_current
!.......................................................................
!
!     Written by H. Matsui
!
!      subroutine allocate_surf_data_current
!      subroutine deallocate_surf_data_current
!
      module m_surf_data_current
!
      use m_precision
      use t_surface_bc_data
!
      implicit  none
!
      type(scaler_surf_flux_bc_type), save :: sf_bc1_grad_j(3)
!
      type(scaler_surf_flux_bc_type), save :: sf_bc1_norm_j
!
      type(scaler_surf_bc_data_type), save :: sf_sgs1_grad_j(3)
!
      type(scaler_surf_bc_data_type), save :: sf_bc1_lead_j(3)
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_data_current
!
!
      call alloc_surf_vector_num(sf_bc1_grad_j)
      call alloc_surf_scaler_num(sf_bc1_norm_j)
      call alloc_surf_vector_dat_type(sf_sgs1_grad_j)
      call alloc_surf_vector_dat_type(sf_bc1_lead_j)
!
      call alloc_surf_vector_apt(sf_bc1_grad_j)
      call alloc_surf_scaler_apt(sf_bc1_norm_j)
!
      end subroutine allocate_surf_data_current
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_current
!
!
      call dealloc_surf_vector_type(sf_bc1_grad_j)
      call dealloc_surf_scaler_type(sf_bc1_norm_j)
      call dealloc_surf_vector_dat_type(sf_sgs1_grad_j)
      call dealloc_surf_vector_dat_type(sf_bc1_lead_j)
!
      end subroutine deallocate_surf_data_current
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_current
