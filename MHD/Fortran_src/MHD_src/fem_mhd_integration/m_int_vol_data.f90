!
!   module m_int_vol_data
!.......................................................................
!
!      subroutine allocate_int_vol_data(numele, max_nod_smp)
!
      module m_int_vol_data
!
      use m_precision
      use t_MHD_finite_element_mat
!
      implicit  none
!
!
!>      Work array for FEM assemble in MHD model
      type(work_MHD_fe_mat), save :: mhd_fem1_wk
! 
      end module   m_int_vol_data
