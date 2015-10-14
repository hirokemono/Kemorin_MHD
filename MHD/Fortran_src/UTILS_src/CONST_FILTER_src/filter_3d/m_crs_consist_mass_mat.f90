!
!     module m_crs_consist_mass_mat
!
!     Written by H. Matsui on Oct., 2006
!
!
!      subroutine set_consist_mass_matrix
!
      module m_crs_consist_mass_mat
!
      use m_precision
      use t_crs_matrix
!
      implicit none
!
      type(CRS_matrix), save :: mass1
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_consist_mass_matrix
!
      use m_machine_parameter
      use m_geometry_data
      use m_element_id_4_node
      use m_sorted_node
      use m_crs_matrix
      use int_consist_mass_mat_filter
!
!
!  ---------------------------------------------------
!       set CRS matrix connectivity for whole domain
!  ---------------------------------------------------
!
      if (iflag_debug.eq.1)  write(*,*) 's_set_crs_connection'
      call s_set_crs_connection(node1, neib_nod1, tbl1_crs)
!
      if (iflag_debug.eq.1)  write(*,*) 'set_idx_list_4_whole_crs'
      call set_idx_list_4_whole_crs
!
!  ---------------------------------------------------
!        cal consist mass matrix
!  ---------------------------------------------------
!
      mass1%NB_crs =  1
      call alloc_crs_mat_data(tbl1_crs, mass1)
!
      if (iflag_debug.eq.1)  write(*,*) 'int_vol_consist_mass_matrix'
      call int_vol_consist_mass_matrix(mass1)
!
      end subroutine set_consist_mass_matrix
!
!-----------------------------------------------------------------------
!
      end module m_crs_consist_mass_mat
