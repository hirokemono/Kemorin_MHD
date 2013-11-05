!
!      module set_crs_connectivities
!
!        programmed by H.Matsui on Oct., 2006
!
!      subroutine set_crs_connect_fluid
!      subroutine set_crs_connect_conduct
!      subroutine set_crs_connect_insulate
!
!      subroutine set_crs_connect_linear
!      subroutine set_crs_connect_linear_fl
!      subroutine set_crs_connect_linear_cd
!      subroutine set_crs_connect_linear_ins
!
!      subroutine set_crs_connect_part(nnod, iele_start, iele_end)
!
      module set_crs_connectivities
!
      use m_precision
!
      use m_geometry_parameter
!
      implicit none
!
      private :: set_crs_connect_part
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_crs_connect_fluid
!
      use m_geometry_data_MHD
!
      call set_crs_connect_part(nnod_4_ele,                             &
     &    iele_fl_start, iele_fl_end)
!
      end subroutine set_crs_connect_fluid
!
!-----------------------------------------------------------------------
!
      subroutine set_crs_connect_conduct
!
      use m_geometry_data_MHD
!
      call set_crs_connect_part(nnod_4_ele,                             &
     &    iele_cd_start, iele_cd_end)
!
      end subroutine set_crs_connect_conduct
!
!-----------------------------------------------------------------------
!
      subroutine set_crs_connect_insulate
!
      use m_geometry_data_MHD
!
      call set_crs_connect_part(nnod_4_ele,                             &
     &    iele_ins_start, iele_ins_end)
!
      end subroutine set_crs_connect_insulate
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_crs_connect_linear
!
      use m_geometry_constants
!
      integer(kind = kint), parameter :: ione = 1
!
      call set_crs_connect_part(num_t_linear, ione, numele)
!
      end subroutine set_crs_connect_linear
!
!-----------------------------------------------------------------------
!
      subroutine set_crs_connect_linear_fl
!
      use m_geometry_constants
      use m_geometry_data_MHD
!
      call set_crs_connect_part(num_t_linear,                           &
     &    iele_fl_start, iele_fl_end)
!
      end subroutine set_crs_connect_linear_fl
!
!-----------------------------------------------------------------------
!
      subroutine set_crs_connect_linear_cd
!
      use m_geometry_constants
      use m_geometry_data_MHD
!
      call set_crs_connect_part(num_t_linear,                           &
     &    iele_cd_start, iele_cd_end)
!
      end subroutine set_crs_connect_linear_cd
!
!-----------------------------------------------------------------------
!
      subroutine set_crs_connect_linear_ins
!
      use m_geometry_constants
      use m_geometry_data_MHD
!
      call set_crs_connect_part(num_t_linear,                           &
     &    iele_ins_start, iele_ins_end)
!
      end subroutine set_crs_connect_linear_ins
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_crs_connect_part(nnod, iele_start, iele_end)
!
       use m_element_id_4_node
       use m_next_node_id_4_node
!
       use set_element_id_4_node
       use set_crs_connection
!
      integer( kind=kint )  ::  nnod, iele_start, iele_end
!
!
      call set_layerd_ele_id_4_node(nnod, iele_start, iele_end)
      call const_next_nod_id_4_node
!
      call s_set_crs_connection
!
      call deallocate_iele_belonged
      call deallocate_inod_next_node
!
      end subroutine set_crs_connect_part
!
!-----------------------------------------------------------------------
!
      end module set_crs_connectivities
