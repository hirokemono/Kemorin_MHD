!cal_delta_z_4_z_filter.f90
!      module cal_delta_z_4_z_filter
!
!      Written by H. Matsui
!
!!      subroutine elapsed_label_4_Zfilter
!!      subroutine cal_delta_z                                          &
!!     &         (CG_param, DJDS_param, nod_comm, node, ele, edge,      &
!!     &          spf_1d, g_FEM, jac_1d, tbl_crs, mat_crs, SR_sig, SR_r)
!!        type(CG_poarameter), intent(inout) :: CG_param
!!        type(DJDS_poarameter), intent(in) :: DJDS_param
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(inout) :: node
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge
!!        type(edge_shape_function), intent(in) :: spf_1d
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_1d), intent(in) :: jac_1d
!!        type(CRS_matrix_connect), intent(inout) :: tbl_crs
!!        type(CRS_matrix), intent(inout) :: mat_crs
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!
      module cal_delta_z_4_z_filter
!
      use m_precision
      use m_work_time
!
      use t_comm_table
      use t_geometry_data
      use t_edge_data
      use t_iccg_parameter
      use t_crs_connect
      use t_crs_matrix
!
      use t_shape_functions
      use t_fem_gauss_int_coefs
      use t_jacobian_1d
      use t_solver_SR
      use t_work_time
!
      implicit none
!
      logical :: flag_Zfilte_time = .FALSE.
      integer(kind = kint) :: ist_elapsed_Zfilter = 0
      integer(kind = kint) :: ied_elapsed_Zfilter = 0
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine elapsed_label_4_Zfilter
!
      integer(kind = kint), parameter :: num_append = 4
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_Zfilter, ied_elapsed_Zfilter)
!
      elps1%labels(ist_elapsed_Zfilter+1)= 'Solver initialization time'
      elps1%labels(ist_elapsed_Zfilter+2)= 'Solver precondition time  '
      elps1%labels(ist_elapsed_Zfilter+3)= 'Solver iteration time     '
      elps1%labels(ist_elapsed_Zfilter+4)= 'Solver communication time '
!
      flag_Zfilte_time = .TRUE.
!
      end subroutine elapsed_label_4_Zfilter
!
!-----------------------------------------------------------------------
!
      subroutine add_z_solver_elapsed(INITtime, PRECtime,               &
     &                                COMPtime, COMMtime, elps)
!
      real(kind = kreal), intent(in) :: INITtime, PRECtime
      real(kind = kreal), intent(in) :: COMPtime, COMMtime
!
      type(elapsed_time_data), intent(inout) :: elps
!
      elps%elapsed(ist_elapsed_Zfilter+1)                               &
     &          = elps%elapsed(ist_elapsed_Zfilter+1) + INITtime
      elps%elapsed(ist_elapsed_Zfilter+2)                               &
     &          = elps%elapsed(ist_elapsed_Zfilter+2) + PRECtime
      elps%elapsed(ist_elapsed_Zfilter+3)                               &
     &          = elps%elapsed(ist_elapsed_Zfilter+3) + COMPtime
      elps%elapsed(ist_elapsed_Zfilter+4)                               &
     &          = elps%elapsed(ist_elapsed_Zfilter+4) + COMMtime
      end subroutine add_z_solver_elapsed
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine cal_delta_z                                            &
     &         (CG_param, DJDS_param, nod_comm, node, ele, edge,        &
     &          spf_1d, g_FEM, jac_1d, tbl_crs, mat_crs, SR_sig, SR_r)
!
      use m_int_edge_data
      use m_commute_filter_z
!
      use calcs_by_LUsolver
      use solve_by_mass_z
      use int_edge_mass_mat_z_filter
      use set_matrices_4_z_filter
!
      type(CG_poarameter), intent(inout) :: CG_param
      type(DJDS_poarameter), intent(in) :: DJDS_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(inout) :: node
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(edge_shape_function), intent(in) :: spf_1d
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_1d), intent(in) :: jac_1d
!
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
      type(CRS_matrix), intent(inout) :: mat_crs
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      real(kind = kreal), allocatable :: rhs_dz(:)
!
      real(kind = kreal) :: INITtime, PRECtime
      real(kind = kreal) :: COMPtime, COMMtime
      integer(kind = kint) :: num_int
!
!
      num_int = i_int_z_filter
!
      call int_edge_mass_matrix(node%numnod, ele%numele, edge,          &
     &                          num_int, g_FEM, jac_1d)
!      flag_mass = .FALSE.
!
      allocate(rhs_dz(node%numnod))
      rhs_dz(1:node%numnod) = 0.0d0
!
      if(flag_mass) then
        call allocate_consist_mass_crs(node%numnod, tbl_crs)
        call set_consist_mass_mat(node%numnod)
!
        call allocate_delta_z(node%numnod, ele%numele)
        call int_edge_vart_width(node%numnod, ele%numele, edge,         &
      &                          num_int, g_FEM, jac_1d, rhs_dz)
!
        rhs_mk_crs(1:node%numnod) = rhs_dz(1:node%numnod)
        call set_consist_mass_mat(node%numnod)
!
        write(*,*) mat_crs%METHOD_crs
        if(mat_crs%METHOD_crs .eq. 'LU') then
          call solve_delta_z_etc_LU(node%numnod, rhs_dz, delta_z)
        else
          write(*,*) 'solve_crs_by_mass_z'
          call solve_crs_by_mass_z(CG_param, DJDS_param, nod_comm,      &
     &        node, tbl_crs, mat_crs, SR_sig, SR_r,                     &
     &        INITtime, PRECtime, COMPtime, COMMtime)
          if(flag_Zfilte_time) call add_z_solver_elapsed                &
     &                            (INITtime, PRECtime,                  &
     &                             COMPtime, COMMtime, elps1)
!
!$omp parallel workshare
          delta_z(1:node%numnod) = sol_mk_crs(1:node%numnod)
!$omp end parallel workshare
        end if
!
        write(*,*) 'int_edge_diff_vart_w'
        call int_edge_diff_vart_w(node, ele, edge, num_int,             &
     &                            spf_1d, g_FEM, jac_1d, delta_dz)
        rhs_mk_crs(1:node%numnod) = rhs_dz(1:node%numnod)

        if(mat_crs%METHOD_crs .eq. 'LU') then
          call solve_delta_z_etc_LU(node%numnod, rhs_dz, delta_dz)
        else
          write(*,*) 'solve_crs_by_mass_z2'
          call solve_crs_by_mass_z2(CG_param, DJDS_param, nod_comm,     &
     &        node, tbl_crs, mat_crs, SR_sig, SR_r,                     &
     &        INITtime, PRECtime, COMPtime, COMMtime)
          if(flag_Zfilte_time) call add_z_solver_elapsed                &
     &                            (INITtime, PRECtime,                  &
     &                             COMPtime, COMMtime, elps1)
!
!$omp parallel workshare
          delta_dz(1:node%numnod) = sol_mk_crs(1:node%numnod)
!$omp end parallel workshare
        end if
!
        call int_edge_d2_vart_w(node, ele, edge, num_int, spf_1d,       &
      &                         g_FEM, jac_1d, rhs_dz)
!        call int_edge_d2_vart_w2(node, ele, edge, num_int, spf_1d,     &
!     &                           g_FEM, jac_1d, rhs_dz)
        rhs_mk_crs(1:node%numnod) = rhs_dz(1:node%numnod)

        if(mat_crs%METHOD_crs .eq. 'LU') then
          call solve_delta_z_etc_LU(node%numnod, rhs_dz, d2_dz)
        else
          write(*,*) 'solve_crs_by_mass_z2'
          call solve_crs_by_mass_z2(CG_param, DJDS_param, nod_comm,     &
     &        node, tbl_crs, mat_crs, SR_sig, SR_r,                     &
     &        INITtime, PRECtime, COMPtime, COMMtime)
!
          if(flag_Zfilte_time) call add_z_solver_elapsed                &
     &                            (INITtime, PRECtime,                  &
     &                             COMPtime, COMMtime, elps1)
!
!$omp parallel workshare
          d2_dz(1:node%numnod) = sol_mk_crs(1:node%numnod)
!$omp end parallel workshare
        end if
!
        d2_dz(1:node%numnod) = rhs_dz(1:node%numnod) * mk(1:node%numnod)
      else
        call allocate_delta_z(node%numnod, ele%numele)
        call int_edge_vart_width(node%numnod, ele%numele, edge,         &
     &                           num_int, g_FEM, jac_1d, rhs_dz)
        delta_z(1:node%numnod)                                          &
     &           = rhs_dz(1:node%numnod) * mk(1:node%numnod)
!
        call int_edge_diff_vart_w(node, ele, edge, num_int,             &
     &                            spf_1d, g_FEM, jac_1d, delta_dz)
        delta_dz(1:node%numnod)                                         &
     &            = rhs_dz(1:node%numnod) * mk(1:node%numnod)
!
        call int_edge_d2_vart_w(node, ele, edge, num_int, spf_1d,       &
     &                          g_FEM, jac_1d, rhs_dz)
        d2_dz(1:node%numnod) = rhs_dz(1:node%numnod) * mk(1:node%numnod)
      end if
      deallocate(rhs_dz)
!
      end subroutine cal_delta_z
!
!   --------------------------------------------------------------------
!
      end module cal_delta_z_4_z_filter
