!>@file   cal_delta_z_4_z_filter.f90
!!        module cal_delta_z_4_z_filter
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief get vertical spacing for vertical filter
!!
!!@verbatim
!!      subroutine elapsed_label_4_Zfilter
!!      subroutine cal_delta_z(CG_param, DJDS_param,                    &
!!     &          nod_comm, node, ele, edge, spf_1d, g_FEM, jac_1d,     &
!!     &          z_int_edge, n_int, dz_plane, tbl_crs, mat_crs,        &
!!     &          SR_sig, SR_r)
!!        type(CG_poarameter), intent(inout) :: CG_param
!!        type(DJDS_poarameter), intent(in) :: DJDS_param
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(inout) :: node
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge
!!        type(edge_shape_function), intent(in) :: spf_1d
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_1d), intent(in) :: jac_1d
!!        type(z_int_edge_data), intent(in) :: z_int_edge
!!        integer(kind = kint), intent(in) :: n_int
!!        type(edge_z_width), intent(inout) :: dz_plane
!!        type(CRS_matrix_connect), intent(inout) :: tbl_crs
!!        type(CRS_matrix), intent(inout) :: mat_crs
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
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
      logical, private :: flag_mass = .TRUE.
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
      subroutine cal_delta_z(CG_param, DJDS_param,                      &
     &          nod_comm, node, ele, edge, spf_1d, g_FEM, jac_1d,       &
     &          z_int_edge, n_int, dz_plane, tbl_crs, mat_crs,          &
     &          SR_sig, SR_r)
!
      use t_vert_edge_width
      use t_consist_z_mass_crs
      use t_z_int_edge_data
!
      use calcs_by_LUsolver
      use solve_by_mass_z
      use int_edge_z_spacing
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
      type(z_int_edge_data), intent(in) :: z_int_edge
      integer(kind = kint), intent(in) :: n_int
!
      type(edge_z_width), intent(inout) :: dz_plane
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
      type(CRS_matrix), intent(inout) :: mat_crs
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      type(consist_z_mass_crs) :: zmass
      real(kind = kreal), allocatable :: rhs_dz(:)
!
      real(kind = kreal) :: INITtime, PRECtime
      real(kind = kreal) :: COMPtime, COMMtime
!
!
!      flag_mass = .FALSE.
!
      allocate(rhs_dz(node%numnod))
      rhs_dz(1:node%numnod) = 0.0d0
!
      if(flag_mass) then
        call alloc_consist_mass_crs(node%numnod, tbl_crs, zmass)
        call set_consist_mass_mat(node%numnod, z_int_edge%mk_ele,       &
     &                            zmass)
!
        call alloc_edge_vert_width(node%numnod, ele%numele, dz_plane)
        call int_edge_vert_width(node%numnod, ele%numele, edge,         &
     &                           n_int, g_FEM, jac_1d, rhs_dz)
!
        call set_consist_mass_mat(node%numnod, z_int_edge%mk_ele,       &
     &                            zmass)
!
        write(*,*) mat_crs%METHOD_crs
        if(mat_crs%METHOD_crs .eq. 'LU') then
          call solve_delta_z_etc_LU(node%numnod, z_int_edge%mk_ele,     &
     &                              rhs_dz, dz_plane%delta_z_n)
        else
          write(*,*) 'solve_crs_by_mass_z'
          call solve_crs_by_mass_z(CG_param, DJDS_param, nod_comm,      &
     &        node, zmass, tbl_crs, mat_crs, SR_sig, SR_r,              &
     &         rhs_dz, dz_plane%delta_z_n,                              &
     &        INITtime, PRECtime, COMPtime, COMMtime)
          if(flag_Zfilte_time) call add_z_solver_elapsed                &
     &                            (INITtime, PRECtime,                  &
     &                             COMPtime, COMMtime, elps1)
        end if
!
        write(*,*) 'int_edge_diff_vert_w'
        call int_edge_diff_vert_w(node, ele, edge, n_int,               &
     &      spf_1d, g_FEM, jac_1d, dz_plane%delta_z_n, rhs_dz)

        if(mat_crs%METHOD_crs .eq. 'LU') then
          call solve_delta_z_etc_LU(node%numnod, z_int_edge%mk_ele,     &
     &                              rhs_dz, dz_plane%delta_dz_n)
        else
          write(*,*) 'solve_crs_by_mass_z2'
          call solve_crs_by_mass_z2                                     &
     &       (CG_param, DJDS_param, nod_comm, node, tbl_crs, mat_crs,   &
     &        SR_sig, SR_r, rhs_dz, dz_plane%delta_dz_n,                &
     &        INITtime, PRECtime, COMPtime, COMMtime)
          if(flag_Zfilte_time) call add_z_solver_elapsed                &
     &                            (INITtime, PRECtime,                  &
     &                             COMPtime, COMMtime, elps1)
        end if
!
        call int_edge_d2_vert_w                                         &
      &    (node, ele, edge, n_int, spf_1d, g_FEM, jac_1d,              &
      &     dz_plane%delta_z_n, dz_plane%delta_dz_n, rhs_dz)
!        call int_edge_d2_vert_w2(node, ele, edge, n_int,               &
!     &      spf_1d, g_FEM, jac_1d, dz_plane%delta_dz_n, rhs_dz)

        if(mat_crs%METHOD_crs .eq. 'LU') then
          call solve_delta_z_etc_LU(node%numnod, z_int_edge%mk_ele,     &
     &                              rhs_dz, dz_plane%d2_dz_n)
        else
          write(*,*) 'solve_crs_by_mass_z2'
          call solve_crs_by_mass_z2                                     &
     &       (CG_param, DJDS_param, nod_comm, node, tbl_crs, mat_crs,   &
     &        SR_sig, SR_r, rhs_dz, dz_plane%d2_dz_n,                   &
     &        INITtime, PRECtime, COMPtime, COMMtime)
!
          if(flag_Zfilte_time) call add_z_solver_elapsed                &
     &                            (INITtime, PRECtime,                  &
     &                             COMPtime, COMMtime, elps1)
        end if
        call dealloc_consist_mass_crs(zmass)
!
!$omp parallel workshare
        dz_plane%d2_dz_n(1:node%numnod)                                 &
     &       = rhs_dz(1:node%numnod) * z_int_edge%mk_z(1:node%numnod)
!$omp end parallel workshare
      else
        call alloc_edge_vert_width(node%numnod, ele%numele, dz_plane)
        call int_edge_vert_width(node%numnod, ele%numele, edge,         &
     &                           n_int, g_FEM, jac_1d, rhs_dz)
        dz_plane%delta_z_n(1:node%numnod)                               &
     &      = rhs_dz(1:node%numnod) * z_int_edge%mk_z(1:node%numnod)
!
        call int_edge_diff_vert_w(node, ele, edge, n_int,               &
     &      spf_1d, g_FEM, jac_1d, dz_plane%delta_z_n, rhs_dz)
        dz_plane%delta_dz_n(1:node%numnod)                              &
     &      = rhs_dz(1:node%numnod) * z_int_edge%mk_z(1:node%numnod)
!
        call int_edge_d2_vert_w                                         &
     &     (node, ele, edge, n_int, spf_1d, g_FEM, jac_1d,              &
     &      dz_plane%delta_z_n, dz_plane%delta_dz_n, rhs_dz)
!
!$omp parallel workshare
        dz_plane%d2_dz_n(1:node%numnod)                                 &
     &       = rhs_dz(1:node%numnod) * z_int_edge%mk_z(1:node%numnod)
!$omp end parallel workshare
      end if
      deallocate(rhs_dz)
!
      end subroutine cal_delta_z
!
!   --------------------------------------------------------------------
!
      end module cal_delta_z_4_z_filter
