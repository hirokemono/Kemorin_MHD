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
      type(vector_surf_bc_type) :: Bsf1_bcs
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
      call alloc_surf_vector_num(Bsf1_bcs%grad)
      call alloc_surf_vector_dat_type(Bsf1_bcs%sgs)
      call alloc_surf_scaler_num(Bsf1_bcs%normal)
      call alloc_surf_vector_dat_type(Bsf1_bcs%torque_lead)
!
      call alloc_surf_vector_apt(Bsf1_bcs%grad)
      call alloc_surf_scaler_apt(Bsf1_bcs%normal)
!
      end subroutine allocate_surf_data_magne
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_magne
!
!
      call dealloc_surf_vector_type(Bsf1_bcs%grad)
      call dealloc_surf_vector_dat_type(Bsf1_bcs%sgs)
      call dealloc_surf_scaler_type(Bsf1_bcs%normal)
      call dealloc_surf_vector_dat_type(Bsf1_bcs%torque_lead)
!
      end subroutine deallocate_surf_data_magne
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_magne
