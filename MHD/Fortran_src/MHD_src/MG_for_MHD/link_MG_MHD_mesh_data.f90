!link_MG_MHD_mesh_data.f90
!     module link_MG_MHD_mesh_data
!
!        programmed H.Matsui on Dec., 2008
!
!      subroutine s_link_MG_MHD_mesh_data
!
      module link_MG_MHD_mesh_data
!
      use m_precision
!
      use t_comm_table
!
      implicit none
!
      private :: link_first_comm_to_type
      private :: link_first_ele_connect_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_link_MG_MHD_mesh_data
!
      use m_solver_djds_MHD
      use m_type_AMG_data_4_MHD
      use m_type_AMG_mesh
      use m_type_AMG_data
      use t_interpolate_table
      use t_geometry_data
!
      integer(kind = kint) :: i_level
!
!
      call link_first_comm_to_type(MG_comm(0))
      call link_comm_tbl_types(DJDS_comm_fl, MG_comm_fl(0))
!
      call link_comm_tbl_types(MG_mesh(1)%mesh%nod_comm, MG_comm(1) )
      call link_comm_tbl_types(MG_MHD_mesh(1)%nod_fl_comm,              &
     &    MG_comm_fl(1))
      call link_first_ele_connect_type(MG_itp(1)%f2c%ele_org)
      call link_new_ele_connect_type(MG_mesh(1)%mesh%ele,               &
     &    MG_itp(1)%c2f%ele_org )
!
      do i_level = 2, num_MG_level
        call link_comm_tbl_types(MG_mesh(i_level)%mesh%nod_comm,        &
     &      MG_comm(i_level) )
        call link_comm_tbl_types(MG_MHD_mesh(i_level)%nod_fl_comm,      &
     &      MG_comm_fl(i_level))
!
        call link_new_ele_connect_type(MG_mesh(i_level-1)%mesh%ele,     &
     &      MG_itp(i_level)%f2c%ele_org )
        call link_new_ele_connect_type(MG_mesh(i_level)%mesh%ele,       &
     &      MG_itp(i_level)%c2f%ele_org )
      end do
!
      end subroutine s_link_MG_MHD_mesh_data
!
!  ---------------------------------------------------------------------
!
      subroutine link_first_comm_to_type(MG_comm_0)
!
      use m_nod_comm_table
!
      type(communication_table), intent(inout) :: MG_comm_0
!
!
      MG_comm_0%num_neib =    num_neib
      MG_comm_0%ntot_import = ntot_import
      MG_comm_0%ntot_export = ntot_export
!
      MG_comm_0%id_neib =>       id_neib
      MG_comm_0%num_import =>    num_import
      MG_comm_0%istack_import => istack_import
      MG_comm_0%item_import =>   item_import
      MG_comm_0%num_export =>    num_export
      MG_comm_0%istack_export => istack_export
      MG_comm_0%item_export =>   item_export
!
      end subroutine link_first_comm_to_type
!
!  ---------------------------------------------------------------------
!
      subroutine link_first_ele_connect_type(ele)
!
      use m_geometry_parameter
      use m_geometry_data
      use t_geometry_data
!
      type(element_data), intent(inout) :: ele
!
!
      ele%numele =     numele
      ele%nnod_4_ele = nnod_4_ele
!
      ele%iele_global => globalelmid
      ele%elmtyp =>      elmtyp
      ele%nodelm =>      nodelm
      ele%ie =>          ie
!
      end subroutine link_first_ele_connect_type
!
!-----------------------------------------------------------------------
!
      end module link_MG_MHD_mesh_data
