!>@file   set_djds_connect_type_MHD.f90
!!@brief  module set_djds_connect_type_MHD
!!
!!@author H. Matsui
!!@date        Written by H. Matsui in Dec., 2008
!!@n      modified by H. Matsui on Nov., 2013
!
!>@brief  Construct index table for DJDS solver
!!
!!@verbatim
!!      subroutine set_MG_djds_connect_type)
!!      subroutine set_MG_djds_conn_lin_type_MHD
!!@endverbatim
!
      module set_djds_connect_type_MHD
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_constants
      use m_solver_djds_MHD
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
        if(my_rank .lt. MG_mpi(i_level)%nprocs ) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &       'set_djds_layer_connect_type fluid', i_level
          call set_djds_layer_connect_type                              &
     &      (MG_mesh(i_level)%mesh%ele%nnod_4_ele,                      &
     &       MG_MHD_mesh(i_level)%fluid%iele_start_fld,                 &
     &       MG_MHD_mesh(i_level)%fluid%iele_end_fld,                   &
     &       MG_mesh(i_level)%mesh, MG_MHD_mesh(i_level)%nod_fl_comm,   &
     &       MG_mpi(i_level), MHD1_matrices%MG_DJDS_fluid(i_level) )
        else
          if(iflag_debug .gt. 0) write(*,*)                             &
     &       'empty_djds_connectivity_type fluid', i_level
          call empty_djds_connectivity_type(MG_mesh(i_level)%mesh,      &
     &        MHD1_matrices%MG_DJDS_fluid(i_level) )
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
        if(my_rank .lt. MG_mpi(i_level)%nprocs) then
          if( MG_mesh(i_level)%mesh%ele%nnod_4_ele                      &
     &      .eq. num_t_linear) then
            call link_djds_connect_structs                              &
     &         (MHD1_matrices%MG_DJDS_table(i_level),                   &
     &          MHD1_matrices%MG_DJDS_linear(i_level))
!
            call link_djds_connect_structs                              &
     &         (MHD1_matrices%MG_DJDS_fluid(i_level),                   &
     &          MHD1_matrices%MG_DJDS_lin_fl(i_level))
          else
            call set_djds_layer_connect_type(num_t_linear,              &
     &         ione, MG_mesh(i_level)%mesh%ele%numele,                  &
     &         MG_mesh(i_level)%mesh, MG_mesh(i_level)%mesh%nod_comm,   &
     &         MG_mpi(i_level), MHD1_matrices%MG_DJDS_linear(i_level))
!
            call set_djds_layer_connect_type(num_t_linear,              &
     &         MG_MHD_mesh(i_level)%fluid%iele_start_fld,               &
     &         MG_MHD_mesh(i_level)%fluid%iele_end_fld,                 &
     &         MG_mesh(i_level)%mesh, MG_MHD_mesh(i_level)%nod_fl_comm, &
     &         MG_mpi(i_level), MHD1_matrices%MG_DJDS_lin_fl(i_level))
          end if
        else
          call empty_djds_connectivity_type(MG_mesh(i_level)%mesh,      &
     &        MHD1_matrices%MG_DJDS_linear(i_level) )
          call empty_djds_connectivity_type(MG_mesh(i_level)%mesh,      &
     &        MHD1_matrices%MG_DJDS_lin_fl(i_level) )
        end if
      end do
!
      end subroutine set_MG_djds_conn_lin_type_MHD
!
!-----------------------------------------------------------------------
!
      end module set_djds_connect_type_MHD
