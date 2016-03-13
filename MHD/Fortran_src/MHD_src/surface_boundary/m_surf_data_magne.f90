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
      type(velocity_surf_bc_type), save :: Asf1_bcs
!
      type(vector_surf_bc_type), save :: Bsf1_bcs
!
      type(vector_surf_bc_type), save :: Jsf1_bcs
!
      type(potential_surf_bc_type), save :: Fsf1_bcs
!
      end module m_surf_data_magne
