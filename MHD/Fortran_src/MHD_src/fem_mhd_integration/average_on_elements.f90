!
!     module average_on_elements
!
!      Written by H.Matsui
!      Moified by H. Matsui on Sep., 2007
!
!      subroutine velocity_on_element
!      subroutine magnetic_on_element
!      subroutine filtered_magne_on_ele
!
!      subroutine vorticity_on_element
!      subroutine rot_magne_on_element
!      subroutine current_on_element
!      subroutine rot_filter_magne_on_element
!
      module average_on_elements
!
      use m_precision
!
      use m_control_parameter
      use m_int_vol_data
!
      use cal_fields_on_element
      use cal_differences_on_ele
      use clear_phys_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine velocity_on_element
!
      use m_geometry_data_MHD
      use m_node_phys_address
      use m_node_phys_data
      use m_element_phys_address
      use m_element_phys_data
!
!
      call vector_on_element(iele_fl_smp_stack, intg_point_t_evo,       &
     &    d_ele(1,iphys_ele%i_velo), d_nod(1,iphys%i_velo) )
      fld_ele1%iflag_update(iphys_ele%i_velo:iphys_ele%i_velo+2) = 1
!
      end subroutine velocity_on_element
!
! -----------------------------------------------------------------------
!
      subroutine magnetic_on_element
!
      use m_geometry_data_MHD
      use m_node_phys_address
      use m_node_phys_data
      use m_element_phys_address
      use m_element_phys_data
!
!
      call vector_on_element(iele_cd_smp_stack, intg_point_t_evo,       &
     &    d_ele(1,iphys_ele%i_magne), d_nod(1,iphys%i_magne) )
      fld_ele1%iflag_update(iphys_ele%i_magne:iphys_ele%i_magne+2) = 1
!
      end subroutine magnetic_on_element
!
! -----------------------------------------------------------------------
!
      subroutine filtered_magne_on_ele
!
      use m_geometry_data_MHD
      use m_node_phys_address
      use m_node_phys_data
      use m_element_phys_address
      use m_element_phys_data
!
!
      call vector_on_element(iele_cd_smp_stack, intg_point_t_evo,       &
     &    d_ele(1,iphys_ele%i_filter_magne),                            &
     &    d_nod(1,iphys%i_filter_magne) )
      fld_ele1%iflag_update(iphys_ele%i_filter_magne  ) = 1
      fld_ele1%iflag_update(iphys_ele%i_filter_magne+1) = 1
      fld_ele1%iflag_update(iphys_ele%i_filter_magne+2) = 1
!
!
      end subroutine filtered_magne_on_ele
!
! -----------------------------------------------------------------------
!
      subroutine vorticity_on_element
!
      use m_geometry_data_MHD
      use m_node_phys_address
      use m_node_phys_data
      use m_element_phys_address
      use m_element_phys_data
!
!
      call rotation_on_element(iele_fl_smp_stack, intg_point_t_evo,     &
     &    d_ele(1,iphys_ele%i_vort), d_nod(1,iphys%i_velo) )
      fld_ele1%iflag_update(iphys_ele%i_vort:iphys_ele%i_vort+2) = 1
!
      end subroutine vorticity_on_element
!
! -----------------------------------------------------------------------
!
      subroutine rot_magne_on_element
!
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
      use m_element_phys_address
      use m_element_phys_data
!
!
      call rotation_on_element(ele1%istack_ele_smp, intg_point_t_evo,   &
     &    d_ele(1,iphys_ele%i_magne), d_nod(1,iphys%i_vecp) )
      fld_ele1%iflag_update(iphys_ele%i_magne:iphys_ele%i_magne+2) = 1
!
      end subroutine rot_magne_on_element
!
! -----------------------------------------------------------------------
!
      subroutine current_on_element
!
      use m_geometry_data_MHD
      use m_node_phys_address
      use m_node_phys_data
      use m_element_phys_address
      use m_element_phys_data
!
!
      call rotation_on_element(iele_cd_smp_stack, intg_point_t_evo,     &
     &    d_ele(1,iphys_ele%i_current), d_nod(1,iphys%i_magne) )
      fld_ele1%iflag_update(iphys_ele%i_current  ) = 1
      fld_ele1%iflag_update(iphys_ele%i_current+1) = 1
      fld_ele1%iflag_update(iphys_ele%i_current+2) = 1
!
      end subroutine current_on_element
!
! -----------------------------------------------------------------------
!
      subroutine rot_filter_magne_on_element
!
      use m_geometry_data
      use m_node_phys_address
      use m_node_phys_data
      use m_element_phys_address
      use m_element_phys_data
!
!
      call rotation_on_element(ele1%istack_ele_smp, intg_point_t_evo,   &
     &    d_ele(1,iphys_ele%i_filter_magne),                            &
     &    d_nod(1,iphys%i_filter_vecp) )
     fld_ele1%iflag_update(iphys_ele%i_filter_vecp  ) = 1
     fld_ele1%iflag_update(iphys_ele%i_filter_vecp+1) = 1
     fld_ele1%iflag_update(iphys_ele%i_filter_vecp+2) = 1
!
      end subroutine rot_filter_magne_on_element
!
! -----------------------------------------------------------------------
!
      end module average_on_elements
