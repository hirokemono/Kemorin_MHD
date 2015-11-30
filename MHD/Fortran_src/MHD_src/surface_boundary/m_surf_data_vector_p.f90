!
!     module m_surf_data_vector_p
!.......................................................................
!
!     Written by H. Matsui
!     Modified by H. Matsui on Feb., 2009
!
!      subroutine allocate_surf_data_vect_p
!      subroutine deallocate_surf_data_vect_p
!
      module m_surf_data_vector_p
!
      use m_precision
      use t_surface_bc_data
!
      implicit  none
!
!
      type(vector_surf_flux_bc_type), save :: sf_bc1_grad_a
!
      type(scaler_surf_flux_bc_type), save :: sf_bc1_norm_a
!
      type(vector_surf_bc_data_type), save :: sf_sgs1_grad_a
!
      type(vector_surf_bc_data_type), save :: sf_bc1_lead_a
!
      type(scaler_surf_bc_data_type), save :: sf_bc1_pvc_in_a
!
      type(scaler_surf_bc_data_type), save :: sf_bc1_pvc_out_a
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_data_vect_p
!
!
      call alloc_surf_vector_num(sf_bc1_grad_a)
      call alloc_surf_scaler_num(sf_bc1_norm_a)
      call alloc_surf_vector_dat_type(sf_sgs1_grad_a)
      call alloc_surf_vector_dat_type(sf_bc1_lead_a)
      call alloc_surf_scaler_dat_type(sf_bc1_pvc_in_a)
      call alloc_surf_scaler_dat_type(sf_bc1_pvc_out_a)
!
      call alloc_surf_vector_apt(sf_bc1_grad_a)
      call alloc_surf_scaler_apt(sf_bc1_norm_a)
!
      end subroutine allocate_surf_data_vect_p
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_vect_p
!
!
      call dealloc_surf_vector_type(sf_bc1_grad_a)
      call dealloc_surf_scaler_type(sf_bc1_norm_a)
      call dealloc_surf_vector_dat_type(sf_sgs1_grad_a)
      call dealloc_surf_vector_dat_type(sf_bc1_lead_a)
      call dealloc_surf_scaler_dat_type(sf_bc1_pvc_in_a)
      call dealloc_surf_scaler_dat_type(sf_bc1_pvc_out_a)
!
      end subroutine deallocate_surf_data_vect_p
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_vector_p
