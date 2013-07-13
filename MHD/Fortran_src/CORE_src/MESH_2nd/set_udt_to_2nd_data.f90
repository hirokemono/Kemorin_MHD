!
!      module set_udt_to_2nd_data
!
!        programmed by H.Matsui on July, 2006
!
!      subroutine link_num_2nd_field_2_output(ucd)
!      subroutine link_2nd_node_data_2_output(ucd)
!      subroutine link_2nd_field_data_2_output(ucd)
!
!      subroutine set_2nd_data_by_udt(ucd)
!
      module set_udt_to_2nd_data
!
      use m_precision
      use t_ucd_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine link_num_2nd_field_2_output(ucd)
!
      use m_2nd_geometry_param
!
      type(ucd_data), intent(inout) :: ucd
!
!
      ucd%nnod = nnod_2nd
!
      ucd%nele =       nele_2nd
      ucd%nnod_4_ele = nnod_4_ele_2nd
!
      end subroutine link_num_2nd_field_2_output
!
!-----------------------------------------------------------------------
!
      subroutine link_2nd_node_data_2_output(ucd)
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
!
      type(ucd_data), intent(inout) :: ucd
!
!
      ucd%xx =>          xx_2nd
      ucd%inod_global => globalnodid_2nd
!
      end subroutine link_2nd_node_data_2_output
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine link_2nd_field_data_2_output(ucd)
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_2nd_phys_data
!
      type(ucd_data), intent(inout) :: ucd
!
!
      ucd%nnod =      nnod_2nd
      ucd%num_field = num_nod_phys_2nd_vis
      ucd%ntot_comp = ntot_nod_phys_2nd_vis
!
      ucd%num_comp =>    ncomps_nod_2nd(1:ucd%num_field)
      ucd%phys_name =>   phys_nod_name_2nd(1:ucd%num_field)
!
      ucd%d_ucd => d_nod_2nd(1:ucd%nnod,1:ucd%ntot_comp)
!
      end subroutine link_2nd_field_data_2_output
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_2nd_data_by_udt(ucd)
!
      use m_2nd_geometry_param
      use m_2nd_phys_data
      use set_and_cal_udt_data
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call set_field_by_udt_data(nnod_2nd, num_nod_phys_2nd,            &
     &    ntot_nod_phys_2nd, ncomps_nod_2nd, phys_nod_name_2nd,         &
     &    d_nod_2nd, ucd)
!
      end subroutine set_2nd_data_by_udt
!
! -----------------------------------------------------------------------
!
      end module set_udt_to_2nd_data
