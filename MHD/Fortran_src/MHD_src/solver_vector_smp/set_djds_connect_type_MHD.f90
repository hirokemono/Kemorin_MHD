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
!!      subroutine set_MG_djds_connect_type(DJDS_param, MHD_matrices)
!!      subroutine set_MG_djds_conn_lin_type_MHD                        &
!!     &         (DJDS_param, MHD_matrices)
!!        type(DJDS_poarameter), intent(in) :: DJDS_param
!!        type(MHD_MG_matrices), intent(inout) :: MHD_matrices
!!@endverbatim
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
      use t_iccg_parameter
      use t_solver_djds_MHD
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_MG_djds_connect_type(DJDS_param, MHD_matrices)
!
      use t_mesh_data
      use t_geometry_data_MHD
      use t_next_node_ele_4_node
      use t_crs_connect
      use t_solver_djds
      use set_djds_connectivity_type
!
      type(DJDS_poarameter), intent(in) :: DJDS_param
      type(MHD_MG_matrices), intent(inout) :: MHD_matrices
!
      integer(kind = kint) :: i_level
!
!
      do i_level = 1, MGCG_WK1%num_MG_level
        if(my_rank .lt. MG_mpi(i_level)%nprocs ) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &       'set_djds_layer_connect_type fluid', i_level
          call set_djds_layer_connect_type                              &
     &      (MG_mesh(i_level)%mesh%ele%nnod_4_ele,                      &
     &       MG_MHD_mesh(i_level)%fluid%iele_start_fld,                 &
     &       MG_MHD_mesh(i_level)%fluid%iele_end_fld,                   &
     &       MG_mesh(i_level)%mesh, MG_MHD_mesh(i_level)%nod_fl_comm,   &
     &       MG_mpi(i_level), DJDS_param,                               &
     &      MHD_matrices%MG_DJDS_fluid(i_level) )
        else
          if(iflag_debug .gt. 0) write(*,*)                             &
     &       'empty_djds_connectivity_type fluid', i_level
          call empty_djds_connectivity_type(MG_mesh(i_level)%mesh,      &
     &        MHD_matrices%MG_DJDS_fluid(i_level) )
        end if
      end do
!
      end subroutine set_MG_djds_connect_type
!
!-----------------------------------------------------------------------
!
      subroutine set_MG_djds_conn_lin_type_MHD                          &
     &         (DJDS_param, MHD_matrices)
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
      type(DJDS_poarameter), intent(in) :: DJDS_param
      type(MHD_MG_matrices), intent(inout), target :: MHD_matrices
!
      integer(kind = kint) :: i_level, l_endlevel
!
!
      l_endlevel = 0
      do i_level = 1, MGCG_WK1%num_MG_level
        if(my_rank .ge. MG_mpi(i_level)%nprocs) then
          l_endlevel = i_level - 1
          exit
        end if
      end do
!
      do i_level = 1, l_endlevel
        if(MG_mesh(i_level)%mesh%ele%nnod_4_ele .eq. num_t_linear)      &
     &         then
          call link_djds_connect_structs                                &
     &       (MHD_matrices%MG_DJDS_table(i_level),                      &
     &        MHD_matrices%MG_DJDS_linear(i_level))
!
          call link_djds_connect_structs                                &
     &       (MHD_matrices%MG_DJDS_fluid(i_level),                      &
     &        MHD_matrices%MG_DJDS_lin_fl(i_level))
        else
          call set_djds_layer_connect_type(num_t_linear,                &
     &        ione, MG_mesh(i_level)%mesh%ele%numele,                   &
     &        MG_mesh(i_level)%mesh, MG_mesh(i_level)%mesh%nod_comm,    &
     &        MG_mpi(i_level), DJDS_param,                              &
     &        MHD_matrices%MG_DJDS_linear(i_level))
!
          call set_djds_layer_connect_type(num_t_linear,                &
     &        MG_MHD_mesh(i_level)%fluid%iele_start_fld,                &
     &        MG_MHD_mesh(i_level)%fluid%iele_end_fld,                  &
     &        MG_mesh(i_level)%mesh, MG_MHD_mesh(i_level)%nod_fl_comm,  &
     &        MG_mpi(i_level), DJDS_param,                              &
     &        MHD_matrices%MG_DJDS_lin_fl(i_level))
        end if
      end do
!
      do i_level = l_endlevel+1, MGCG_WK1%num_MG_level
        call empty_djds_connectivity_type(MG_mesh(i_level)%mesh,        &
     &        MHD_matrices%MG_DJDS_linear(i_level) )
        call empty_djds_connectivity_type(MG_mesh(i_level)%mesh,        &
     &        MHD_matrices%MG_DJDS_lin_fl(i_level) )
      end do
!
      end subroutine set_MG_djds_conn_lin_type_MHD
!
!-----------------------------------------------------------------------
!
      end module set_djds_connect_type_MHD
