!
!     module initialize_4_MHD_AMG
!
!        programmed H.Matsui on Dec., 2008
!
!!      subroutine s_initialize_4_MHD_AMG                               &
!!     &         (nod_comm_1st, node_1st, ele_1st)
!      subroutine const_MGCG_MHD_matrices
!
      module initialize_4_MHD_AMG
!
      use m_precision
!
      use m_machine_parameter
      use m_type_AMG_mesh
      use m_type_AMG_data_4_MHD
      use m_type_AMG_data
      use calypso_mpi
!
      implicit none
!
! ---------------------------------------------------------------------
!
      contains
!
! ---------------------------------------------------------------------
!
      subroutine s_initialize_4_MHD_AMG                                 &
     &         (nod_comm_1st, node_1st, ele_1st)
!
      use t_comm_table
      use t_geometry_data
      use t_edge_data
      use t_surface_data
!
      use m_type_AMG_data
      use m_boundary_condition_IDs
      use set_layers_4_MHD_AMG
      use const_mesh_information
      use allocate_MHD_AMG_array
      use set_diffusivities_MHD_AMG
      use const_comm_tbl_type_fluid
      use const_bc_infty_surf_type
      use cal_jacobians_type
      use set_table_type_RHS_assemble
      use set_djds_connectivity_type
      use set_djds_connect_type_MHD
      use set_normal_vectors
      use int_surface_param_type
      use set_bc_id_type_data
      use set_bc_surface_data_type
      use int_type_mass_matrices
      use set_MHD_idx_4_mat_type
      use link_djds_tbl_MHD_4_MG
      use link_MG_MHD_mesh_data
      use const_element_comm_tables
!
      type(communication_table), intent(inout) :: nod_comm_1st
      type(node_data), intent(inout) :: node_1st
      type(element_data), intent(inout) :: ele_1st
!
      integer(kind = kint) :: i_level
!
!
      call split_multigrid_comms
!
      if (iflag_debug .gt. 0) write(*,*) 'alloc_iccgN_vec_type'
      MG_vector(0)%isize_solver_vect = -1
      call alloc_iccgN_vec_type(isix, node_1st%numnod,  MG_vector(0))
!
!     --------------------- 
!
      if(iflag_debug .gt. 0) write(*,*) 'alloc_MG_djds_tbl_lin_idx'
      call alloc_MG_djds_tbl_lin_idx(num_MG_level)
      if(iflag_debug .gt. 0) write(*,*) 's_link_djds_tbl_MHD_4_MG'
      call s_link_djds_tbl_MHD_4_MG(MG_djds_tbl(0), MG_djds_tbl_fl(0),  &
     &    MG_djds_tbl_l(0), MG_djds_tbl_fll(0) )
!
!     --------------------- 
!
!
      do i_level = 1, num_MG_level
        if(my_rank .lt. MG_mpi(i_level)%nprocs ) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &            'set_layers_type_4_MHD', i_level
          call set_layers_type_4_MHD(MG_mesh(i_level)%mesh,             &
     &      MG_mesh(i_level)%group,  MG_MHD_mesh(i_level) )
          if(iflag_debug .gt. 0) write(*,*)                             &
     &            's_const_mesh_types_info', i_level
          call s_const_mesh_types_info                                  &
     &       (my_rank, MG_mesh(i_level), MG_ele_mesh(i_level))
        else
          call set_empty_layers_type_4_MHD(MG_MHD_mesh(i_level) )
          call empty_mesh_types_info                                    &
     &       (MG_mesh(i_level), MG_ele_mesh(i_level))
        end if
!
        call deallocate_edge_geom_type(MG_ele_mesh(i_level)%edge)
      end do
!
!     ---------------------
!
      do i_level = 1, num_MG_level
        if(iflag_debug .gt. 0) write(*,*)                               &
     &            's_allocate_MHD_AMG_array', i_level
        call s_allocate_MHD_AMG_array(MG_mesh(i_level),                 &
     &      MG_vector(i_level), MG_FEM_mat(i_level),                    &
     &      MG_mk_MHD(i_level) )
      end do
!
!     --------------------- 
!
      do i_level = 1, num_MG_level
        if(iflag_debug .gt. 0) write(*,*)                               &
     &            's_set_diffusivities_MHD_AMG', i_level
        call s_set_diffusivities_MHD_AMG(MG_mesh(i_level)%mesh%ele,     &
     &      ak_MHD_AMG(i_level) )
        if(iflag_debug .gt. 0) write(*,*)                               &
     &            's_set_sgs_diff_array_MHD_AMG', i_level
        call s_set_sgs_diff_array_MHD_AMG(MG_mesh(i_level)%mesh%ele,    &
     &      ak_MHD_AMG(i_level) )
      end do
!
!     --------------------- 
!
      do i_level = 1, num_MG_level
        if(iflag_debug .gt. 0) write(*,*)                               &
     &            's_const_comm_tbl_type_fluid', i_level
        call s_const_comm_tbl_type_fluid(MG_mpi(i_level),               &
     &      MG_mesh(i_level)%mesh, MG_MHD_mesh(i_level) )
!
        call const_ele_comm_tbl_global_id                               &
     &     (MG_mesh(i_level)%mesh, MG_ele_mesh(i_level))
      end do
!
!     ---------------------
!
      do i_level = 1, num_MG_level
        if(my_rank .lt. MG_mpi(i_level)%nprocs ) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &            'const_bc_infinity_surf_grp', i_level
          call const_bc_infinity_surf_grp                               &
     &       (iflag_surf_infty, MG_mesh(i_level)%group%surf_grp,        &
     &        MG_mesh(i_level)%group%infty_grp)
        else
          call empty_infty_surf_type(MG_mesh(i_level)%group)
        end if
      end do
!
!     --------------------- 
!
      do i_level = 1, num_MG_level
        if(my_rank .lt. MG_mpi(i_level)%nprocs ) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &            'const_jacobian_type', i_level
          call const_jacobian_type(MG_mesh(i_level)%mesh,               &
     &        MG_mesh(i_level)%group, MG_jacobians(i_level)%jac_3d)
!
          if(iflag_debug .gt. 0) write(*,*)                             &
     &            'const_jacobian_surface_type', i_level
          call const_jacobian_surface_type(MG_mesh(i_level)%mesh,       &
     &        MG_ele_mesh(i_level), MG_jacobians(i_level)%jac_2d)
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &            'cal_jacobian_surf_grp_type', i_level
          call cal_jacobian_surf_grp_type(MG_mesh(i_level)%mesh,        &
     &        MG_ele_mesh(i_level), MG_mesh(i_level)%group,             &
     &        MG_jacobians(i_level)%jac_sf_grp)
!
!
          if(iflag_debug .gt. 0) write(*,*)                             &
     &            'const_linear_jac_3d_type', i_level
          call const_linear_jac_3d_type(MG_mesh(i_level)%mesh,          &
     &        MG_mesh(i_level)%group, MG_jacobians(i_level))
        else
          if(iflag_debug .gt. 0) write(*,*)                             &
     &            'empty_jacobian_type', i_level
          call empty_jacobian_type(MG_mesh(i_level)%mesh,               &
     &        MG_jacobians(i_level)%jac_3d)
!
          if(iflag_debug .gt. 0) write(*,*)                             &
     &            'empty_jacobian_surface_type', i_level
          call empty_jacobian_surface_type(MG_ele_mesh(i_level),        &
     &        MG_jacobians(i_level)%jac_2d)
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &            'empty_jacobian_surf_grp_type', i_level
          call empty_jacobian_surf_grp_type(MG_ele_mesh(i_level),       &
     &        MG_mesh(i_level)%group, MG_jacobians(i_level)%jac_sf_grp)
!
!
          if(iflag_debug .gt. 0) write(*,*)                             &
     &            'empty_linear_jac_3d_type', i_level
          call empty_linear_jac_3d_type(MG_mesh(i_level)%mesh,          &
     &        MG_jacobians(i_level))
        end if
      end do
!
!     --------------------- 
!
      do i_level = 1, num_MG_level
        if(my_rank .lt. MG_mpi(i_level)%nprocs ) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &            's_set_table_type_RHS_assemble', i_level
          call s_set_table_type_RHS_assemble                            &
     &       (MG_mesh(i_level)%mesh%node, MG_mesh(i_level)%mesh%ele,    &
     &        MG_next_table(i_level), MG_FEM_tbl(i_level))
        else
          if(iflag_debug .gt. 0) write(*,*)                             &
     &            'empty_table_type_RHS_assemble', i_level
          call empty_table_type_RHS_assemble                            &
     &       (MG_mesh(i_level)%mesh%node, MG_FEM_tbl(i_level),          &
     &        MG_next_table(i_level))
        end if
      end do
!
!     --------------------- 
!
      do i_level = 1, num_MG_level
        if(my_rank .lt. MG_mpi(i_level)%nprocs ) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &            's_set_djds_connectivity_type', i_level
          call s_set_djds_connectivity_type                             &
     &       (MG_mesh(i_level)%mesh, MG_mpi(i_level),                   &
     &        MG_next_table(i_level), MG_djds_tbl(i_level) )
        else
          if(iflag_debug .gt. 0) write(*,*)                             &
     &            'empty_djds_connectivity_type', i_level
          call empty_djds_connectivity_type(MG_mesh(i_level)%mesh,      &
     &        MG_djds_tbl(i_level) )
        end if
!
        call dealloc_iele_belonged(MG_next_table(i_level)%neib_ele)
        call dealloc_inod_next_node(MG_next_table(i_level)%neib_nod)
      end do
!
!     -----  set DJDS matrix connectivity
!
      if(iflag_debug .gt. 0) write(*,*) 's_link_MG_MHD_mesh_data'
      call s_link_MG_MHD_mesh_data(nod_comm_1st, ele_1st)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_MG_djds_connect_type'
      call set_MG_djds_connect_type
!
      if(iflag_debug .gt. 0) write(*,*) 'set_MG_djds_conn_lin_type_MHD'
      call set_MG_djds_conn_lin_type_MHD
!
!     --------------------- 
!
      do i_level = 1, num_MG_level
        if(iflag_debug .gt. 0) write(*,*)                               &
     &         'int_normal_4_all_surface', i_level
        call int_normal_4_all_surface(MG_ele_mesh(i_level)%surf,        &
     &      MG_jacobians(i_level)%jac_2d)
        call s_int_surface_param_type(MG_mesh(i_level)%mesh,            &
     &      MG_ele_mesh(i_level)%surf, MG_mesh(i_level)%group)
      end do
!
!     --------------------- 
!
      do i_level = 1, num_MG_level
        call s_set_bc_id_type_data( MG_mesh(i_level)%mesh,              &
     &      MG_mesh(i_level)%group, MG_MHD_mesh(i_level),               &
     &      MG_node_bc(i_level))
!
        call s_set_bc_surface_data_type(MG_mesh(i_level)%group,         &
     &      MG_surf_bc(i_level) )
      end do
!
!     --------------------- 
!
      do i_level = 1, num_MG_level
        if(iflag_debug .gt. 0) write(*,*) 's_int_type_mass_matrices'
        call s_int_type_mass_matrices( MG_mesh(i_level)%mesh,           &
     &      MG_MHD_mesh(i_level), MG_jacobians(i_level),                &
     &      MG_FEM_tbl(i_level), MG_FEM_mat(i_level),                   &
     &      MG_mk_MHD(i_level) )
      end do
!
!     ---------------------
!
      do i_level = 1, num_MG_level
        if(iflag_debug .gt. 0) write(*,*) 's_set_MHD_idx_4_mat_type'
        call s_set_MHD_idx_4_mat_type( MG_mesh(i_level)%mesh,           &
     &      MG_MHD_mesh(i_level), MG_FEM_tbl(i_level),                  &
     &      MG_djds_tbl(i_level), MG_djds_tbl_fl(i_level),              &
     &      MG_djds_const_idx(i_level), MG_djds_const_idx_fl(i_level) )
!
        if(iflag_debug .gt. 0) write(*,*) 'link_MG_djds_const_lin_idx'
        call link_MG_djds_const_lin_idx(num_MG_level)
      end do
!
!     ---------------------
!
      do i_level = 1, num_MG_level
        if(my_rank .lt. MG_mpi(i_level)%nprocs) then
          if(iflag_debug .gt. 0) write(*,*) 'alloc_MG_AMG_matrices'
          call alloc_MG_AMG_matrices( MG_mesh(i_level),                 &
     &      MG_djds_tbl(i_level), MG_djds_tbl_fl(i_level),              &
     &      MG_mat_velo(i_level), MG_mat_magne(i_level),                &
     &      MG_mat_temp(i_level),  MG_mat_d_scalar(i_level),            &
     &      MG_mat_press(i_level), MG_mat_magp(i_level) )
        else
          if(iflag_debug .gt. 0) write(*,*) 'alloc_MG_zero_matrices'
          call alloc_MG_zero_matrices                                   &
     &     (MG_mat_velo(i_level), MG_mat_magne(i_level),                &
     &      MG_mat_temp(i_level),  MG_mat_d_scalar(i_level),            &
     &      MG_mat_press(i_level), MG_mat_magp(i_level) )
        end if
      end do
!
      end subroutine s_initialize_4_MHD_AMG
!
! ---------------------------------------------------------------------
!
      subroutine const_MGCG_MHD_matrices
!
      use m_ctl_parameter_Multigrid
      use link_djds_mat_MHD_4_MG
      use set_aiccg_matrices_type
      use matrices_precond_type
!
      integer(kind = kint) :: i_level
!
!
      call s_link_djds_mat_MHD_4_MG(MG_mat_velo(0), MG_mat_magne(0),    &
     &    MG_mat_temp(0), MG_mat_d_scalar(0),                           &
     &    MG_mat_press(0), MG_mat_magp(0))
!
      do i_level = 1, num_MG_level
        if(my_rank .lt. MG_mpi(i_level)%nprocs) then
          if (iflag_debug.eq.1) write(*,*) 'set MG matrices', i_level
          call s_set_aiccg_matrices_type(MG_mesh(i_level),              &
     &      MG_ele_mesh(i_level),  MG_MHD_mesh(i_level),                &
     &      MG_node_bc(i_level), MG_surf_bc(i_level),                   &
     &      MG_djds_tbl(i_level), MG_djds_tbl_fl(i_level),              &
     &      MG_djds_tbl_cd(i_level), MG_djds_tbl_l(i_level),            &
     &      MG_djds_tbl_fll(i_level), MG_jacobians(i_level),            &
     &      ak_MHD_AMG(i_level),  MG_FEM_tbl(i_level),                  &
     &      MG_djds_const_idx(i_level), MG_djds_const_idx_fl(i_level),  &
     &      MG_djds_const_idx_cd(i_level),                              &
     &      MG_djds_const_idx_l(i_level),                               &
     &      MG_djds_const_idx_fll(i_level), MG_filter_MHD(i_level),     &
     &      MG_mk_MHD(i_level), MG_FEM_mat(i_level),                    &
     &      MG_mat_velo(i_level), MG_mat_magne(i_level),                &
     &      MG_mat_temp(i_level), MG_mat_d_scalar(i_level),             &
     &      MG_mat_press(i_level), MG_mat_magp(i_level) )
        end if
      end do
!
      do i_level = 1, num_MG_level
        if(my_rank .lt. MG_mpi(i_level)%nprocs) then
          if (iflag_debug.gt.0) write(*,*) 'preconditioning', i_level
          call s_matrices_precond_type(PRECOND_MG,                      &
     &      MG_djds_tbl(i_level), MG_djds_tbl_fl(i_level),              &
     &      MG_djds_tbl_l(i_level),  MG_djds_tbl_fll(i_level),          &
     &      MG_mat_velo(i_level), MG_mat_magne(i_level),                &
     &      MG_mat_temp(i_level), MG_mat_d_scalar(i_level),             &
     &      MG_mat_press(i_level), MG_mat_magp(i_level) )
        end if
      end do
!
      end subroutine const_MGCG_MHD_matrices
!
! ---------------------------------------------------------------------
!
      end module initialize_4_MHD_AMG
