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
      type(scaler_surf_bc_type) :: Tsf1_bcs
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
      call alloc_surf_scaler_num(Tsf1_bcs%flux)
      call alloc_surf_scaler_dat_type(Tsf1_bcs%sgs)
      call alloc_surf_scaler_dat_type(Tsf1_bcs%flux_lead)
!
      call alloc_surf_scaler_apt(Tsf1_bcs%flux)
!
      end subroutine allocate_surf_data_temp
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_data_temp
!
!
      call dealloc_surf_scaler_type(Tsf1_bcs%flux)
      call dealloc_surf_scaler_dat_type(Tsf1_bcs%sgs)
      call dealloc_surf_scaler_dat_type(Tsf1_bcs%flux_lead)
!
      end subroutine deallocate_surf_data_temp
!
!-----------------------------------------------------------------------
!
      end module m_surf_data_temp
