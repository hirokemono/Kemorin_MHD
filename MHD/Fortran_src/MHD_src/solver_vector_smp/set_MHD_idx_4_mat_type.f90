!set_MHD_idx_4_mat_type.f90
!      module set_MHD_idx_4_mat_type
!
!      programmed by H.Matsui on March, 2009
!
!!      subroutine s_set_MHD_idx_4_mat_type                             &
!!     &         (evo_V, evo_B, evo_A, evo_T, evo_C,                    &
!!     &          mesh, MHD_mesh, rhs_tbl,                              &
!!     &          djds_tbl, djds_tbl_fl, djds_tbl_lin, djds_tbl_fll,    &
!!     &          MG_mat_q, MG_mat_fl_q, MG_mat_full_cd_q,              &
!!     &          MG_mat_linear, MG_mat_fl_l)
!!        type(time_evolution_params), intent(in) :: evo_V, evo_B, evo_A
!!        type(time_evolution_params), intent(in) :: evo_T, evo_C
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(DJDS_ordering_table),  intent(inout) :: djds_tbl
!!        type(DJDS_ordering_table),  intent(inout) :: djds_tbl_fl
!!        type(DJDS_ordering_table),  intent(inout) :: djds_tbl_cd
!!        type(DJDS_ordering_table),  intent(inout) :: djds_tbl_ins
!!        type(table_mat_const), intent(inout) :: MG_mat_q
!!        type(table_mat_const), intent(inout) :: MG_mat_fl_q
!!        type(table_mat_const), intent(inout) :: MG_mat_full_cd_q
!!        type(table_mat_const), intent(inout) :: MG_mat_linear
!!        type(table_mat_const), intent(inout) :: MG_mat_fl_l
!
      module set_MHD_idx_4_mat_type
!
      use m_precision
      use m_geometry_constants
!
      use t_time_stepping_parameter
      use t_mesh_data
      use t_geometry_data
      use t_geometry_data_MHD
      use t_table_FEM_const
      use t_solver_djds
!
      implicit none
!
      private :: set_part_index_list_4_djds
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_MHD_idx_4_mat_type                               &
     &         (evo_V, evo_B, evo_A, evo_T, evo_C,                      &
     &          mesh, MHD_mesh, rhs_tbl,                                &
     &          djds_tbl, djds_tbl_fl, djds_tbl_lin, djds_tbl_fll,      &
     &          MG_mat_q, MG_mat_fl_q, MG_mat_full_cd_q,                &
     &          MG_mat_linear, MG_mat_fl_l)
!
      use set_idx_4_mat_type
!
      type(time_evolution_params), intent(in) :: evo_V, evo_B, evo_A
      type(time_evolution_params), intent(in) :: evo_T, evo_C
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(DJDS_ordering_table),  intent(inout) :: djds_tbl
      type(DJDS_ordering_table),  intent(inout) :: djds_tbl_fl
      type(DJDS_ordering_table),  intent(inout) :: djds_tbl_lin
      type(DJDS_ordering_table),  intent(inout) :: djds_tbl_fll
!
      type(table_mat_const), intent(inout) :: MG_mat_q
      type(table_mat_const), intent(inout) :: MG_mat_fl_q
      type(table_mat_const), intent(inout) :: MG_mat_full_cd_q
      type(table_mat_const), intent(inout) :: MG_mat_linear
      type(table_mat_const), intent(inout) :: MG_mat_fl_l
!
!
      write(*,*) 'alloc_type_marix_list'
      call alloc_type_marix_list(mesh%ele%nnod_4_ele, rhs_tbl,          &
     &    MG_mat_q)
      write(*,*) 'set_whole_index_list_4_djds'
      call set_whole_index_list_4_djds                                  &
     &   (mesh%node, mesh%ele, rhs_tbl, djds_tbl, MG_mat_q)
!
      call set_index_list_4_mat_etr_l(mesh%node, mesh%ele,              &
     &    rhs_tbl, djds_tbl_lin, MG_mat_q, MG_mat_linear)
!
      if    (evo_V%iflag_scheme .ne. id_no_evolution                    &
     &  .or. evo_T%iflag_scheme .ne. id_no_evolution                    &
     &  .or. evo_C%iflag_scheme .ne. id_no_evolution) then
        write(*,*) 'alloc_type_marix_list'
        call set_index_list_4_mat_fl(mesh%node, mesh%ele,               &
     &      MHD_mesh%fluid, rhs_tbl, djds_tbl_fl, MG_mat_fl_q)
        call set_index_list_4_mat_fl_l(mesh%node, mesh%ele,             &
     &      MHD_mesh%fluid, rhs_tbl, djds_tbl_fll,                      &
     &      MG_mat_fl_q, MG_mat_fl_l)
      end if
!
      if    (evo_B%iflag_scheme .ne. id_no_evolution                    &
     &  .or. evo_A%iflag_scheme .eq. id_Crank_nicolson_cmass) then
        call set_index_list_4_mat_fl(mesh%node, mesh%ele, &
     &      MHD_mesh%conduct, rhs_tbl, djds_tbl, MG_mat_full_cd_q)
      end if
!
      call dealloc_type_4_djds_table(djds_tbl)
      call dealloc_type_4_djds_table(djds_tbl_fl)
      if (mesh%ele%nnod_4_ele .ne. num_t_linear) then
        call dealloc_type_4_djds_table(djds_tbl_lin)
        call dealloc_type_4_djds_table(djds_tbl_fll)
      end if
!
      end subroutine s_set_MHD_idx_4_mat_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_fl                                &
     &         (node, ele, fluid, rhs_tbl, djds_tbl_fl, MG_mat_fl_q)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
!
      type(table_mat_const), intent(inout) :: MG_mat_fl_q
!
!
      call alloc_type_marix_list                                        &
     &   (ele%nnod_4_ele, rhs_tbl, MG_mat_fl_q)
      call set_part_index_list_4_djds(node, ele, fluid, rhs_tbl,        &
     &    djds_tbl_fl, MG_mat_fl_q)
!
      end subroutine set_index_list_4_mat_fl
!
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_etr_l(node, ele, rhs_tbl,         &
     &         djds_tbl_lin, MG_mat_q, MG_mat_linear)
!
      use set_idx_4_mat_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(DJDS_ordering_table), intent(in) :: djds_tbl_lin
      type(table_mat_const), intent(in) :: MG_mat_q
!
      type(table_mat_const), intent(inout) :: MG_mat_linear
!
!
      call alloc_type_marix_list(num_t_linear, rhs_tbl, MG_mat_linear)
!
      if (ele%nnod_4_ele .ne. num_t_linear) then
        call set_whole_index_list_4_djds(node, ele,                     &
     &      rhs_tbl, djds_tbl_lin, MG_mat_linear)
      else
        MG_mat_linear%idx_4_mat = MG_mat_q%idx_4_mat
      end if
!
      end subroutine set_index_list_4_mat_etr_l
!
!-----------------------------------------------------------------------
!
      subroutine set_index_list_4_mat_fl_l(node, ele, fluid,            &
     &          rhs_tbl, djds_tbl_fll, MG_mat_fl_q, MG_mat_fl_l)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fll
      type(table_mat_const), intent(in) :: MG_mat_fl_q
!
      type(table_mat_const), intent(inout) :: MG_mat_fl_l
!
!
      call alloc_type_marix_list(num_t_linear, rhs_tbl, MG_mat_fl_l)
!
      if (ele%nnod_4_ele .ne. num_t_linear) then
        call set_part_index_list_4_djds(node, ele, fluid, rhs_tbl,      &
     &      djds_tbl_fll, MG_mat_fl_l)
      else
        MG_mat_fl_l%idx_4_mat = MG_mat_fl_q%idx_4_mat
      end if
!
      end subroutine set_index_list_4_mat_fl_l
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_part_index_list_4_djds(node, ele,                  &
     &          fluid, rhs_tbl, djds_part_tbl, MG_mat_tbl)
!
      use m_machine_parameter
      use set_idx_4_mat_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(DJDS_ordering_table), intent(in) :: djds_part_tbl
!
      type(table_mat_const), intent(inout) :: MG_mat_tbl
!
      integer(kind = kint) :: mat_num, k2
      integer(kind = kint) :: iproc, iele, inum, iconn
      integer(kind = kint) :: inn, ist, ied, in
!
!
!$omp parallel private(k2)
      do k2 = 1, MG_mat_tbl%nnod_1ele
!
!$omp do private(iproc,inum,inn,ist,ied,in,iele,iconn,mat_num)
        do iproc = 1, np_smp
          do inum = 1, rhs_tbl%inod_ele_max
            inn = inum + rhs_tbl%inod_ele_max*(iproc-1)
            ist = rhs_tbl%nod_stack_smp(inn-1)+1
            ied = rhs_tbl%nod_stack_smp(inn)
!
            do in = ist, ied
              iele =  rhs_tbl%iele_sort_smp(in)
              iconn = rhs_tbl%iconn_sort_smp(in)
!
              if (iele.ge.fluid%iele_start_fld                          &
     &          .and. iele.le.fluid%iele_end_fld) then
                call set_DJDS_off_diag_type                             &
     &             (node%numnod, node%internal_node, djds_part_tbl,     &
     &              ele%ie(iele,iconn), ele%ie(iele,k2), mat_num)
                MG_mat_tbl%idx_4_mat(in,k2) = mat_num
              else
                MG_mat_tbl%idx_4_mat(in,k2) = 0
              end if
!
            end do
          end do
        end do
!$omp end do nowait
!
      end do
!$omp end parallel
!
      end subroutine set_part_index_list_4_djds
!
!-----------------------------------------------------------------------
!
      end module set_MHD_idx_4_mat_type
