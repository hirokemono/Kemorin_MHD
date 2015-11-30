!
!     module m_surf_data_torque
!.......................................................................
!
!     Written by H. Matsui
!
!      subroutine allocate_surf_data_velo
!      subroutine allocate_surf_data_torque
!
!      subroutine deallocate_surf_data_velo
!      subroutine deallocate_surf_data_torque
!
      module m_surf_data_torque
!
      use m_precision
      use t_surface_bc_data
!
      implicit  none
!
!
      type(vector_surf_flux_bc_type), save :: sf_bc1_grad_v
!
      type(scaler_surf_flux_bc_type), save :: sf_bc1_norm_v
!
      type(vector_surf_bc_data_type), save :: sf_sgs1_grad_v
!
      type(vector_surf_bc_data_type), save :: sf_bc1_lead_tq
!
      type(scaler_surf_bc_data_type), save :: sf_bc1_free_sph_in
!
      type(scaler_surf_bc_data_type), save :: sf_bc1_free_sph_out
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_data_velo
!
!
      call alloc_surf_vector_dat_type(sf_sgs1_grad_v)
      call alloc_surf_scaler_num(sf_bc1_norm_v)
!
      call alloc_surf_scaler_apt(sf_bc1_norm_v)
!
      end subroutine allocate_surf_data_velo
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_data_torque
!
!
      call alloc_surf_vector_num(sf_bc1_grad_v)
      call alloc_surf_vector_dat_type(sf_bc1_lead_tq)
      call alloc_surf_scaler_dat_type(sf_bc1_free_sph_in)
      call alloc_surf_scaler_dat_type(sf_bc1_free_sph_out)
!
      call alloc_surf_vector_apt(sf_bc1_grad_v)
!
      end subroutine allocate_surf_data_torque
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_velo
!
!
      call dealloc_surf_vector_dat_type(sf_sgs1_grad_v)
      call dealloc_surf_scaler_type(sf_bc1_norm_v)
!
      end subroutine deallocate_surf_data_velo
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_torque
!
      call dealloc_surf_vector_type(sf_bc1_grad_v)
      call dealloc_surf_vector_dat_type(sf_bc1_lead_tq)
      call dealloc_surf_scaler_dat_type(sf_bc1_free_sph_in)
      call dealloc_surf_scaler_dat_type(sf_bc1_free_sph_out)
!
      end subroutine deallocate_surf_data_torque
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_torque
