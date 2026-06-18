!>@file   solve_by_mass_z.f90
!!        module solve_by_mass_z
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief Solve matrix
!!
!!@verbatim
!!      subroutine solve_crs_by_mass_z(CG_param, DJDS_param,            &
!!     &          nod_comm, node, zmass, tbl_crs, mat_crs,              &
!!     &          SR_sig, SR_r, rhs_mk_crs, sol_mk_crs,                 &
!!     &          INITtime, PRECtime, COMPtime, COMMtime)
!!      subroutine solve_crs_by_mass_z2                                 &
!!     &        (CG_param, DJDS_param, nod_comm, node, tbl_crs, mat_crs,&
!!     &         SR_sig, SR_r, rhs_mk_crs, sol_mk_crs,                  &
!!     &         INITtime, PRECtime, COMPtime, COMMtime)
!!        type(CG_poarameter), intent(inout) :: CG_param
!!        type(DJDS_poarameter), intent(in) :: DJDS_param
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(inout) :: node
!!        type(consist_z_mass_crs), intent(in) :: zmass
!!        type(CRS_matrix_connect), intent(inout) :: tbl_crs
!!        type(CRS_matrix), intent(inout) :: mat_crs
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        real(kind = kreal), intent(inout) :: sol_mk_crs(node%numnod)
!!        real(kind = kreal), intent(inout) :: INITtime, PRECtime
!!        real(kind = kreal), intent(inout) :: COMPtime, COMMtime
!!@endverbatim
!
      module solve_by_mass_z
!
      use m_precision
!
      use t_geometry_data
      use t_comm_table
      use t_iccg_parameter
      use t_solver_djds
      use t_crs_connect
      use t_crs_matrix
      use t_solver_SR
!
      implicit none
!
      type(DJDS_ordering_table), save :: djds_tbl1
      type(DJDS_MATRIX), save :: djds_mat1
!
      private :: djds_tbl1, djds_mat1
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine solve_crs_by_mass_z(CG_param, DJDS_param,              &
     &          nod_comm, node, zmass, tbl_crs, mat_crs,                &
     &          SR_sig, SR_r, rhs_mk_crs, sol_mk_crs,                   &
     &          INITtime, PRECtime, COMPtime, COMMtime)
!
      use calypso_mpi
      use t_consist_z_mass_crs
      use solve_precond_DJDS
      use copy_matrix_2_djds_array
!
      type(CG_poarameter), intent(inout) :: CG_param
      type(DJDS_poarameter), intent(in) :: DJDS_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(inout) :: node
      type(consist_z_mass_crs), intent(in) :: zmass
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
      type(CRS_matrix), intent(inout) :: mat_crs
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      real(kind = kreal), intent(in) ::    rhs_mk_crs(node%numnod)
      real(kind = kreal), intent(inout) :: sol_mk_crs(node%numnod)
      real(kind = kreal), intent(inout) :: INITtime, PRECtime
      real(kind = kreal), intent(inout) :: COMPtime, COMMtime
!
      integer(kind = kint) :: itr_res, ierr
!
!
      mat_crs%NB_crs = 1
      call alloc_crs_mat_data(tbl_crs, mat_crs)
!
      mat_crs%B_crs(1:node%numnod) = rhs_mk_crs(1:node%numnod)
      mat_crs%D_crs(1:node%numnod) = zmass%d_mk_crs(1:node%numnod)
      mat_crs%AL_crs(1:tbl_crs%ntot_l)                                  &
     &                             = zmass%al_mk_crs(1:tbl_crs%ntot_l)
      mat_crs%AU_crs(1:tbl_crs%ntot_u)                                  &
     &                             = zmass%au_mk_crs(1:tbl_crs%ntot_u)
!
      call transfer_crs_2_djds_matrix(node, nod_comm,                   &
     &    tbl_crs, mat_crs, CG_param, DJDS_param, djds_tbl1, djds_mat1)
      call solve_by_djds_solver11(node, nod_comm, CG_param,             &
     &    mat_crs, djds_tbl1, djds_mat1, SR_sig, SR_r, itr_res, ierr,   &
     &    INITtime, PRECtime, COMPtime, COMMtime)
      if (my_rank.eq.0) write (*,*) itr_res, "  iters"
!
      sol_mk_crs(1:node%numnod) = mat_crs%X_crs(1:node%numnod)
!
      call dealloc_crs_mat_data(mat_crs)
!
      end subroutine solve_crs_by_mass_z
!
!  ---------------------------------------------------------------------
!
      subroutine solve_crs_by_mass_z2                                   &
     &        (CG_param, DJDS_param, nod_comm, node, tbl_crs, mat_crs,  &
     &         SR_sig, SR_r, rhs_mk_crs, sol_mk_crs,                    &
     &         INITtime, PRECtime, COMPtime, COMMtime)
!
      use calypso_mpi
      use m_machine_parameter
!
      use copy_matrix_2_djds_array
      use solver_DJDS11_struct
      use solve_precond_DJDS
!
      type(CG_poarameter), intent(inout) :: CG_param
      type(DJDS_poarameter), intent(in) :: DJDS_param
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(inout) :: node
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
      type(CRS_matrix), intent(inout) :: mat_crs
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      real(kind = kreal), intent(in) ::    rhs_mk_crs(node%numnod)
      real(kind = kreal), intent(inout) :: sol_mk_crs(node%numnod)
      real(kind = kreal), intent(inout) :: INITtime, PRECtime
      real(kind = kreal), intent(inout) :: COMPtime, COMMtime
!
      integer(kind = kint) :: itr_res, ierr
!
!
      mat_crs%NB_crs = 1
      write(*,*) 'alloc_crs_mat_data'
      call alloc_crs_mat_data(tbl_crs, mat_crs)
!
      mat_crs%B_crs(1:node%numnod) = rhs_mk_crs(1:node%numnod)
!
      djds_mat1%NB = mat_crs%NB_crs
      call transfer_crs_2_djds_matrix(node, nod_comm,                   &
     &    tbl_crs, mat_crs, CG_param, DJDS_param, djds_tbl1, djds_mat1)
!
      call solve_by_djds_solverNN(node, nod_comm, CG_param,             &
     &    mat_crs, djds_tbl1, djds_mat1, SR_sig, SR_r, itr_res, ierr,   &
     &    INITtime, PRECtime, COMPtime, COMMtime)
      if (my_rank.eq.0) write (*,*) itr_res, "  iters"
!
      sol_mk_crs(1:node%numnod) = mat_crs%X_crs(1:node%numnod)
!
      write(*,*) 'dealloc_crs_mat_data'
      call dealloc_crs_mat_data(mat_crs)
!
      end subroutine solve_crs_by_mass_z2
!
!  ---------------------------------------------------------------------
!
      end module solve_by_mass_z
