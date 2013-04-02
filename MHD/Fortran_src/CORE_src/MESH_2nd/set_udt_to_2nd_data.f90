!
!      module set_udt_to_2nd_data
!
!        programmed by H.Matsui on July, 2006
!
!      subroutine link_num_2nd_field_2_output
!      subroutine link_2nd_node_data_2_output
!      subroutine link_2nd_field_data_2_output
!
!      subroutine set_2nd_data_by_udt
!
      module set_udt_to_2nd_data
!
      use m_precision
!
      use m_ucd_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine link_num_2nd_field_2_output
!
      use m_2nd_geometry_param
!
      nnod_ucd = nnod_2nd
!
      nele_ucd = nele_2nd
      nnod_4_ele_ucd = nnod_4_ele_2nd
!
      end subroutine link_num_2nd_field_2_output
!
!-----------------------------------------------------------------------
!
      subroutine link_2nd_node_data_2_output
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
!
      xx_ucd =>      xx_2nd
      inod_gl_ucd => globalnodid_2nd
!
      end subroutine link_2nd_node_data_2_output
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine link_2nd_field_data_2_output
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_2nd_phys_data
!
      nnod_ucd =      nnod_2nd
      num_field_ucd = num_nod_phys_2nd_vis
      ntot_comp_ucd = ntot_nod_phys_2nd_vis
!
      istack_comp_ucd => istack_nod_comps_2nd(0:num_field_ucd)
      num_comp_ucd =>    ncomps_nod_2nd(1:num_field_ucd)
      phys_name_ucd =>   phys_nod_name_2nd(1:num_field_ucd)
!
      d_nod_ucd => d_nod_2nd(1:nnod_ucd,1:ntot_comp_ucd)
!
      end subroutine link_2nd_field_data_2_output
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_2nd_data_by_udt
!
      use m_2nd_geometry_param
      use m_2nd_phys_data
      use set_and_cal_udt_data
!
!
      call set_field_by_udt_data(nnod_2nd, num_nod_phys_2nd,            &
     &    ntot_nod_phys_2nd, ncomps_nod_2nd, phys_nod_name_2nd,         &
     &    d_nod_2nd)
!
      end subroutine set_2nd_data_by_udt
!
! -----------------------------------------------------------------------
!
      end module set_udt_to_2nd_data
