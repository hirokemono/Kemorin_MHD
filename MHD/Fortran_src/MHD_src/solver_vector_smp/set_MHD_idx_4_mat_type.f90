!set_MHD_idx_4_mat_type.f90
!      module set_MHD_idx_4_mat_type
!
!      programmed by H.Matsui on March, 2009
!
!      subroutine s_set_MHD_idx_4_mat_type(mesh, MHD_mesh, rhs_tbl,     &
!     &          djds_tbl, djds_tbl_fl, djds_const, djds_const_fl)
!        type(mesh_geometry), intent(in) :: mesh
!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(DJDS_ordering_table),  intent(inout) :: djds_tbl
!        type(DJDS_ordering_table),  intent(inout) :: djds_tbl_fl
!        type(DJDS_ordering_table),  intent(inout) :: djds_tbl_cd
!        type(DJDS_ordering_table),  intent(inout) :: djds_tbl_ins
!        type(table_mat_const), intent(inout) :: djds_const
!        type(table_mat_const), intent(inout) :: djds_const_fl
!        type(table_mat_const), intent(inout) :: djds_const_cd
!        type(table_mat_const), intent(inout) :: djds_const_ins
!        type(table_mat_const), intent(inout) :: djds_const_full_cd
!
!      subroutine set_MHD_idx_linear_mat_type(mesh, MHD_mesh, rhs_tbl,  &
!     &          djds_tbl, djds_tbl_fl, djds_const, djds_const_fl)
!        type(mesh_geometry), intent(in) :: mesh
!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(DJDS_ordering_table),  intent(inout) :: djds_tbl
!        type(DJDS_ordering_table),  intent(inout) :: djds_tbl_fl
!        type(DJDS_ordering_table),  intent(inout) :: djds_tbl_cd
!        type(DJDS_ordering_table),  intent(inout) :: djds_tbl_ins
!        type(table_mat_const), intent(inout) :: djds_const
!        type(table_mat_const), intent(inout) :: djds_const_fl
!        type(table_mat_const), intent(inout) :: djds_const_cd
!        type(table_mat_const), intent(inout) :: djds_const_ins
!        type(table_mat_const), intent(inout) :: djds_const_full_cd
!
      module set_MHD_idx_4_mat_type
!
      use m_precision
!
      use t_mesh_data
      use t_geometry_data_MHD
      use t_table_FEM_const
      use t_solver_djds
!
      use set_idx_4_mat_type
!
      implicit none
!
      private :: set_idx_4_mat_type_part
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_MHD_idx_4_mat_type(mesh, MHD_mesh, rhs_tbl,      &
     &          djds_tbl, djds_tbl_fl, djds_const, djds_const_fl)
!
      use m_control_parameter
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(DJDS_ordering_table),  intent(inout) :: djds_tbl
      type(DJDS_ordering_table),  intent(inout) :: djds_tbl_fl
!
      type(table_mat_const), intent(inout) :: djds_const
      type(table_mat_const), intent(inout) :: djds_const_fl
!
!
      write(*,*) 'alloc_type_marix_list'
      call alloc_type_marix_list(mesh%ele%nnod_4_ele, rhs_tbl,          &
     &    djds_const)
      write(*,*) 'set_idx_4_mat_type_whole'
      call set_idx_4_mat_type_whole(mesh%ele%nnod_4_ele, mesh, rhs_tbl, &
     &    djds_tbl, djds_const)
!
!
      write(*,*) 'alloc_type_marix_list'
      call alloc_type_marix_list(mesh%ele%nnod_4_ele, rhs_tbl,          &
     &      djds_const_fl)
      write(*,*) 'set_idx_4_mat_type_part'
      call set_idx_4_mat_type_part(mesh, MHD_mesh%fluid, rhs_tbl,       &
     &       djds_tbl_fl, djds_const_fl)
!
!
!        call alloc_type_marix_list(mesh%ele%nnod_4_ele, rhs_tbl,       &
!     &      djds_const_cd)
!        call alloc_type_marix_list(mesh%ele%nnod_4_ele, rhs_tbl,       &
!     &      djds_const_full_cd)
!        call alloc_type_marix_list(mesh%ele%nnod_4_ele, rhs_tbl,       &
!     &      djds_const_ins)
!
!        call set_idx_4_mat_type_part(mesh, MHD_mesh%conduct,           &
!     &      rhs_tbl, djds_tbl_cd, djds_const_cd)
!
!        call set_idx_4_mat_type_part(mesh, MHD_mesh%conduct,           &
!     &      rhs_tbl, djds_tbl, djds_const_full_cd)
!
!        call set_idx_4_mat_type_part(mesh, MHD_mesh%insulate,          &
!     &      rhs_tbl, djds_tbl_ins, djds_const_ins)
!
      write(*,*) 'dealloc_type_4_djds_table djds_tbl'
      call dealloc_type_4_djds_table(djds_tbl)
      write(*,*) 'dealloc_type_4_djds_table djds_tbl_fl'
      call dealloc_type_4_djds_table(djds_tbl_fl)
!      call dealloc_type_4_djds_table(djds_tbl_cd)
!      call dealloc_type_4_djds_table(djds_tbl_ins)
!
      end subroutine s_set_MHD_idx_4_mat_type
!
!-----------------------------------------------------------------------
!
      subroutine set_MHD_idx_linear_mat_type(mesh, MHD_mesh, rhs_tbl,   &
     &          djds_tbl, djds_tbl_fl, djds_const, djds_const_fl)
!
      use m_control_parameter
      use m_geometry_constants
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(DJDS_ordering_table),  intent(inout) :: djds_tbl
      type(DJDS_ordering_table),  intent(inout) :: djds_tbl_fl
!
      type(table_mat_const), intent(inout) :: djds_const
      type(table_mat_const), intent(inout) :: djds_const_fl
!
!
      call alloc_type_marix_list(num_t_linear, rhs_tbl,                 &
     &    djds_const)
      call set_idx_4_mat_type_whole(num_t_linear, mesh, rhs_tbl,        &
     &    djds_tbl, djds_const)
!
!
      call alloc_type_marix_list(num_t_linear, rhs_tbl,                 &
     &      djds_const_fl)
      call set_idx_4_mat_type_part(mesh, MHD_mesh%fluid,                &
     &      rhs_tbl, djds_tbl_fl, djds_const_fl)
!
!
!        call alloc_type_marix_list(num_t_linear, rhs_tbl,              &
!     &      djds_const_cd)
!        call alloc_type_marix_list(num_t_linear, rhs_tbl,              &
!     &      djds_const_full_cd)
!        call alloc_type_marix_list(num_t_linear, rhs_tbl,              &
!     &      djds_const_ins)
!
!        call set_idx_4_mat_type_part(mesh, MHD_mesh%conduct,           &
!     &      rhs_tbl, djds_tbl_cd, djds_const_cd)
!
!        call set_idx_4_mat_type_part(mesh, MHD_mesh%conduct,           &
!     &      rhs_tbl, djds_tbl, djds_const_full_cd)
!
!        call set_idx_4_mat_type_part(mesh,  MHD_mesh%insulate,         &
!     &     rhs_tbl, djds_tbl_ins, djds_const_ins)
!
      call dealloc_type_4_djds_table(djds_tbl)
      call dealloc_type_4_djds_table(djds_tbl_fl)
!      call dealloc_type_4_djds_table(djds_tbl_cd)
!      call dealloc_type_4_djds_table(djds_tbl_ins)
!
      end subroutine set_MHD_idx_linear_mat_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_idx_4_mat_type_part(mesh, fluid, rhs_tbl,          &
     &          djds_part_tbl, mat_tbl)
!
      use m_machine_parameter
!
      type(mesh_geometry), intent(in) :: mesh
      type(field_geometry_data), intent(in) :: fluid
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(DJDS_ordering_table), intent(in) :: djds_part_tbl
!
      type(table_mat_const), intent(inout) :: mat_tbl
!
      integer(kind = kint) :: nod1, nod2, mat_num, k2
      integer(kind = kint) :: iproc, iele, inum, iconn
      integer(kind = kint) :: inn, ist, ied, in
!
!
      do k2 = 1, mesh%ele%nnod_4_ele
!
        do iproc = 1, np_smp
          do inum = 1, rhs_tbl%inod_ele_max
!
            inn = inum + rhs_tbl%inod_ele_max*(iproc-1)
            ist = rhs_tbl%nod_stack_smp(inn-1)+1
            ied = rhs_tbl%nod_stack_smp(inn)
!
            do in = ist, ied
!
              iele =  rhs_tbl%iele_sort_smp(in)
              iconn = rhs_tbl%iconn_sort_smp(in)
              nod1 = mesh%ele%ie(iele,iconn)
              nod2 = mesh%ele%ie(iele,k2)
!
              if (iele.ge.fluid%iele_start_fld                          &
     &          .and. iele.le.fluid%iele_end_fld) then
                call set_DJDS_off_diag_type                             &
     &             (mesh%node%numnod, mesh%node%internal_node,          &
     &              djds_part_tbl, nod1, nod2, mat_num )
                mat_tbl%idx_4_mat(in,k2) = mat_num
              else
                mat_tbl%idx_4_mat(in,k2) = 0
              end if
!
            end do
          end do
        end do
!
      end do
!
      end subroutine set_idx_4_mat_type_part
!
!-----------------------------------------------------------------------
!
      end module set_MHD_idx_4_mat_type
