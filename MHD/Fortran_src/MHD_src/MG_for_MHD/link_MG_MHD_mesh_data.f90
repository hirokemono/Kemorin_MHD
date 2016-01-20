!link_MG_MHD_mesh_data.f90
!     module link_MG_MHD_mesh_data
!
!        programmed H.Matsui on Dec., 2008
!
!!      subroutine s_link_MG_MHD_mesh_data(nod_comm_1st, ele_1st)
!!      type(element_data), intent(inout) :: nod_comm_1st
!!        type(element_data), intent(inout) :: ele_1st
!
      module link_MG_MHD_mesh_data
!
      use m_precision
!
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_link_MG_MHD_mesh_data(nod_comm_1st, ele_1st)
!
      use m_solver_djds_MHD
      use m_type_AMG_data_4_MHD
      use m_type_AMG_mesh
      use m_type_AMG_data
      use t_comm_table
      use t_geometry_data
      use t_interpolate_table
      use interpolate_by_type
!
      type(communication_table), intent(inout) :: nod_comm_1st
      type(element_data), intent(inout) :: ele_1st
      integer(kind = kint) :: i_level
!
!
      call link_comm_tbl_types(nod_comm_1st, MG_comm(0))
      call link_comm_tbl_types(DJDS_comm_fl, MG_comm_fl(0))
!
      call link_comm_tbl_types(MG_mesh(1)%mesh%nod_comm, MG_comm(1) )
      call link_comm_tbl_types(MG_MHD_mesh(1)%nod_fl_comm,              &
     &    MG_comm_fl(1))
!
      call init_interpolate_mat_type(ele_1st, MG_itp(1)%f2c)
      call init_interpolate_mat_type                                    &
     &   (MG_mesh(1)%mesh%ele, MG_itp(1)%c2f)
!
      do i_level = 2, num_MG_level
        call link_comm_tbl_types(MG_mesh(i_level)%mesh%nod_comm,        &
     &      MG_comm(i_level) )
        call link_comm_tbl_types(MG_MHD_mesh(i_level)%nod_fl_comm,      &
     &      MG_comm_fl(i_level))
!
        call init_interpolate_mat_type                                  &
     &     (MG_mesh(i_level-1)%mesh%ele, MG_itp(i_level)%f2c)
        call init_interpolate_mat_type                                  &
     &     (MG_mesh(i_level)%mesh%ele, MG_itp(i_level)%c2f)
      end do
!
      end subroutine s_link_MG_MHD_mesh_data
!
!  ---------------------------------------------------------------------
!
      end module link_MG_MHD_mesh_data
