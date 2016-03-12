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
      type(velocity_surf_bc_type) :: Asf1_bcs
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
      call alloc_surf_vector_num(Asf1_bcs%grad)
      call alloc_surf_scaler_num(Asf1_bcs%normal)
      call alloc_surf_vector_dat_type(Asf1_bcs%sgs)
      call alloc_surf_vector_dat_type(Asf1_bcs%torque_lead)
      call alloc_surf_scaler_dat_type(Asf1_bcs%free_sph_in)
      call alloc_surf_scaler_dat_type(Asf1_bcs%free_sph_out)
!
      call alloc_surf_vector_apt(Asf1_bcs%grad)
      call alloc_surf_scaler_apt(Asf1_bcs%normal)
!
      end subroutine allocate_surf_data_vect_p
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_vect_p
!
!
      call dealloc_surf_vector_type(Asf1_bcs%grad)
      call dealloc_surf_scaler_type(Asf1_bcs%normal)
      call dealloc_surf_vector_dat_type(Asf1_bcs%sgs)
      call dealloc_surf_vector_dat_type(Asf1_bcs%torque_lead)
      call dealloc_surf_scaler_dat_type(Asf1_bcs%free_sph_in)
      call dealloc_surf_scaler_dat_type(Asf1_bcs%free_sph_out)
!
      end subroutine deallocate_surf_data_vect_p
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_vector_p
