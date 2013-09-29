!set_djds_connect_type_MHD.f90
!      module set_djds_connect_type_MHD
!
!       Written by H. Matsui on Dec., 2008
!
!      subroutine set_MG_djds_connect_type)
!      subroutine set_MG_djds_conn_lin_type_MHD
!
      module set_djds_connect_type_MHD
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_constants
      use m_type_AMG_mesh
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_MG_djds_connect_type
!
      use t_mesh_data
      use t_geometry_data_MHD
      use t_next_node_ele_4_node
      use t_crs_connect
      use t_solver_djds
      use set_djds_connectivity_type
!
!
      integer(kind = kint) :: i_level
!
!
      do i_level = 1, num_MG_level
        if(my_rank .lt. MG_vector(i_level)%nprocs ) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &       'set_djds_layer_connect_type fluid', i_level
          call set_djds_layer_connect_type                              &
     &      (MG_MHD_mesh(i_level)%fluid%iele_start_fld,                 &
     &       MG_MHD_mesh(i_level)%fluid%iele_end_fld,                   &
     &       MG_mesh(i_level)%mesh, MG_MHD_mesh(i_level)%nod_fl_comm,   &
     &       MG_next_table(i_level), MG_CRS_table(i_level),             &
     &       MG_djds_tbl_fl(i_level) )
!
!          call set_djds_layer_connect_type                             &
!     &      (MG_MHD_mesh(i_level)%conduct%iele_start_fld,              &
!     &       MG_MHD_mesh(i_level)%conduct%iele_end_fld,                &
!     &       MG_mesh(i_level)%mesh, MG_mesh(i_level)%mesh%nod_comm,    &
!     &       MG_next_table(i_level), MG_CRS_table(i_level),            &
!     &       MG_djds_tbl_cd(i_level))
!
!          call set_djds_layer_connect_type                             &
!     &      (MG_MHD_mesh(i_level)%insulate%iele_start_fld,             &
!     &       MG_MHD_mesh(i_level)%insulate%iele_end_fld,               &
!     &       MG_mesh(i_level)%mesh,  MG_mesh(i_level)%mesh%nod_comm,   &
!     &       MG_next_table(i_level), MG_CRS_table(i_level),            &
!     &       MG_djds_tbl_ins(i_level))
        else
          if(iflag_debug .gt. 0) write(*,*)                             &
     &       'set_djds_layer_connect_type fluid', i_level
          call empty_djds_connectivity_type(MG_mesh(i_level)%mesh,      &
     &      MG_djds_tbl_fl(i_level) )
!
!          call empty_djds_connectivity_type(MG_mesh(i_level)%mesh,     &
!     &      MG_djds_tbl_cd(i_level) )
!
!          call empty_djds_connectivity_type(MG_mesh(i_level)%mesh,     &
!     &      MG_djds_tbl_ins(i_level) )
        end if
      end do
!
      end subroutine set_MG_djds_connect_type
!
!-----------------------------------------------------------------------
!
      subroutine set_MG_djds_conn_lin_type_MHD
!
      use m_geometry_constants
      use m_type_AMG_data
      use t_mesh_data
      use t_geometry_data_MHD
      use t_next_node_ele_4_node
      use t_crs_connect
      use t_solver_djds
      use set_djds_connectivity_type
!
      integer(kind = kint) :: i_level
!
!
      do i_level = 1, num_MG_level
        if(my_rank .lt. MG_vector(i_level)%nprocs) then
          if( MG_mesh(i_level)%mesh%ele%nnod_4_ele                      &
     &      .eq. num_t_linear) then
            call link_djds_connect_structs(MG_djds_tbl(i_level),        &
     &          MG_djds_tbl_l(i_level))
!
            call link_djds_connect_structs(MG_djds_tbl_fl(i_level),     &
     &          MG_djds_tbl_fll(i_level))
!
!            call link_djds_connect_structs( MG_djds_tbl_cd(i_level),   &
!     &          MG_djds_tbl_cdl(i_level))
!
!            call link_djds_connect_structs(MG_djds_tbl_ins(i_level),   &
!     &         MG_djds_tbl_insl(i_level))
!
          else
            call set_djds_layer_conn_lin_type                           &
     &        (ione, MG_mesh(i_level)%mesh%ele%numele,                  &
     &         MG_mesh(i_level)%mesh, MG_mesh(i_level)%mesh%nod_comm,   &
     &         MG_next_table(i_level), MG_CRS_table(i_level),           &
     &         MG_djds_tbl_l(i_level) )
!
            call set_djds_layer_conn_lin_type                           &
     &        (MG_MHD_mesh(i_level)%fluid%iele_start_fld,               &
     &         MG_MHD_mesh(i_level)%fluid%iele_end_fld,                 &
     &         MG_mesh(i_level)%mesh, MG_MHD_mesh(i_level)%nod_fl_comm, &
     &         MG_next_table(i_level), MG_CRS_table(i_level),           &
     &         MG_djds_tbl_fll(i_level) )
!
!            call set_djds_layer_conn_lin_type                          &
!     &        (MG_MHD_mesh(i_level)%conduct%iele_start_fld,            &
!     &         MG_MHD_mesh(i_level)%conduct%iele_end_fld,              &
!     &         MG_mesh(i_level)%mesh, MG_mesh(i_level)%mesh%nod_comm,  &
!     &         MG_next_table(i_level), MG_CRS_table(i_level),          &
!     &         MG_djds_tbl_cdl(i_level))
!
!            call set_djds_layer_conn_lin_type                          &
!     &        (MG_MHD_mesh(i_level)%insulate%iele_start_fld,           &
!     &         MG_MHD_mesh(i_level)%insulate%iele_end_fld,             &
!     &         MG_mesh(i_level)%mesh, MG_mesh(i_level)%mesh%nod_comm,  &
!     &         MG_next_table(i_level), MG_CRS_table(i_level),          &
!     &         MG_djds_tbl_insl(i_level))
          end if
        else
          call empty_djds_connectivity_type(MG_mesh(i_level)%mesh,      &
     &        MG_djds_tbl_l(i_level) )
          call empty_djds_connectivity_type(MG_mesh(i_level)%mesh,      &
     &        MG_djds_tbl_fll(i_level) )
!          call empty_djds_connectivity_type(MG_mesh(i_level)%mesh,     &
!     &        MG_djds_tbl_cdl(i_level) )
!          call empty_djds_connectivity_type(MG_mesh(i_level)%mesh,     &
!     &        MG_djds_tbl_insl(i_level) )
        end if
      end do
!
      end subroutine set_MG_djds_conn_lin_type_MHD
!
!-----------------------------------------------------------------------
!
      end module set_djds_connect_type_MHD
