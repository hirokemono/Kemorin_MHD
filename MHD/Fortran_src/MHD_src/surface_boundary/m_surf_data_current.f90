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
      type(vector_surf_bc_type) :: Jsf1_bcs
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
      call alloc_surf_vector_num(Jsf1_bcs%grad)
      call alloc_surf_scaler_num(Jsf1_bcs%normal)
      call alloc_surf_vector_dat_type(Jsf1_bcs%sgs)
      call alloc_surf_vector_dat_type(Jsf1_bcs%torque_lead)
!
      call alloc_surf_vector_apt(Jsf1_bcs%grad)
      call alloc_surf_scaler_apt(Jsf1_bcs%normal)
!
      end subroutine allocate_surf_data_current
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_current
!
!
      call dealloc_surf_vector_type(Jsf1_bcs%grad)
      call dealloc_surf_scaler_type(Jsf1_bcs%normal)
      call dealloc_surf_vector_dat_type(Jsf1_bcs%sgs)
      call dealloc_surf_vector_dat_type(Jsf1_bcs%torque_lead)
!
      end subroutine deallocate_surf_data_current
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_current
