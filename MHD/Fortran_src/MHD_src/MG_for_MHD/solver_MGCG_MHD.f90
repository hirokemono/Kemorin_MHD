!>@file   solver_MGCG_MHD.f90
!!@brief  module solver_MGCG_MHD
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2008
!!@date Modified in Nov., 2013
!
!>@brief  Wrapper for linear solvers for MHD dynmamo
!!
!!@verbatim
!!      subroutine init_MGCG_MHD(node)
!!      subroutine solver_MGCG_vector                                   &
!!     &         (node, DJDS_comm, DJDS_tbl, num_MG_level,              &
!!     &          MG_itp, MG_comm, MG_DJDS_tbl, MG_DJDS_mat,            &
!!     &          METHOD, PRECOND, eps, itr, MG_vector, b_vec, x_vec)
!!      subroutine solver_MGCG_scalar                                   &
!!     &         (node, DJDS_comm, DJDS_tbl, mat_DJDS, num_MG_level,    &
!!     &          MG_itp, MG_comm, MG_DJDS_tbl, MG_DJDS_mat11,          &
!!     &          METHOD, PRECOND, eps, itr, MG_vector, b_vec, x_vec)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: DJDS_comm
!!        type(DJDS_ordering_table), intent(in) :: DJDS_tbl
!!        type(DJDS_MATRIX), intent(in) :: mat_DJDS
!!        integer(kind = kint), intent(in) :: num_MG_level
!!        type(MG_itp_table), intent(in) :: MG_itp(num_MG_level)
!!        type(communication_table), intent(in)                         &
!!     &                      :: MG_comm(0:num_MG_level)
!!        type(DJDS_ordering_table), intent(in)                         &
!!     &                      :: MG_DJDS_tbl(0:num_MG_level)
!!        type(DJDS_MATRIX), intent(in) :: MG_DJDS_mat(0:num_MG_level)
!!        type(vectors_4_solver), intent(inout)                         &
!!     &                       :: MG_vector(0:num_MG_level)
!!@endverbatim
!
      module   solver_MGCG_MHD
!
      use m_precision
!
      use m_control_parameter
      use m_machine_parameter
      use m_work_time
      use m_ctl_parameter_Multigrid
      use calypso_mpi
!
      use t_geometry_data
      use t_vector_for_solver
      use t_interpolate_table
      use t_solver_djds
!
      implicit  none
!
      integer(kind = kint), parameter, private :: iterPREmax = 1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_MGCG_MHD(node)
!
      use m_iccg_parameter
      use solver_DJDS11_struct
      use solver_DJDS33_struct
      use solver_VMGCG11_DJDS_SMP
      use solver_VMGCG33_DJDS_SMP
      use skip_comment_f
!
      type(node_data), intent(in) :: node
      integer(kind = kint) :: ierr
!
!
      METHOD = method_4_solver
      if (cmp_no_case(METHOD, 'MGCG')) then
        call init_VMGCG11_DJDS_SMP(node%numnod, np_smp,                 &
            precond_4_solver,  METHOD_MG, PRECOND_MG, iterPREmax)
      else
        call init_DJDS11_struct(node%numnod, np_smp, METHOD,            &
            precond_4_solver, ierr)
      end if
!
!
      if(     iflag_t_evo_4_velo .ge.   id_Crank_nicolson               &
     &   .or. iflag_t_evo_4_vect_p .ge. id_Crank_nicolson               &
     &   .or. iflag_t_evo_4_magne .ge.  id_Crank_nicolson) then
        METHOD = method_4_velo
        if (cmp_no_case(METHOD, 'MGCG')) then
          call init_VMGCG33_DJDS_SMP(node%numnod, np_smp,               &
              precond_4_crank,  METHOD_MG, PRECOND_MG, iterPREmax)
        else
          call init33_DJDS_struct(node%numnod, np_smp, METHOD,          &
              precond_4_crank, ierr)
        end if
      end if
!
      end subroutine init_MGCG_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine solver_MGCG_vector                                     &
     &         (node, DJDS_comm, DJDS_tbl, num_MG_level,                &
     &          MG_itp, MG_comm, MG_DJDS_tbl, MG_DJDS_mat,              &
     &          METHOD, PRECOND, eps, itr, MG_vector, b_vec, x_vec)
!
      use solver_DJDS33_struct
      use solver_VMGCG33_DJDS_SMP
      use skip_comment_f
!
      character(len=kchara), intent(in) :: METHOD, PRECOND
      real(kind = kreal), intent(in) :: eps
      integer(kind = kint), intent(inout) :: itr
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: DJDS_comm
      type(DJDS_ordering_table), intent(in) :: DJDS_tbl
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_itp(num_MG_level)
      type(communication_table), intent(in)                             &
     &                      :: MG_comm(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &                      :: MG_DJDS_tbl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: MG_DJDS_mat(0:num_MG_level)
!
      type(vectors_4_solver), intent(inout)                             &
     &                       :: MG_vector(0:num_MG_level)
      real(kind = kreal), intent(inout) :: b_vec(3*node%numnod)
      real(kind = kreal), intent(inout) :: x_vec(3*node%numnod)
!
      integer(kind = kint) :: ierr, itr_res
!
!
      if(iflag_debug.gt.0)  write(*,*) 'METHOD for vector: ',          &
     &                                  trim(METHOD)
! 
      ierr = i_debug
!
      call start_eleps_time(5)
      if (cmp_no_case(METHOD, 'MGCG')) then
        call VMGCG33_DJDS_SMP(num_MG_level, MG_comm,                    &
     &      MG_itp, MG_DJDS_tbl, MG_DJDS_mat, MG_vector,                &
     &      np_smp, node%numnod, b_vec(1), x_vec(1), itr,               &
     &      itr_MG_mid, itr_MG_lowest, eps, EPS_MG,                     &
     &      PRECOND, METHOD_MG, PRECOND_MG, ierr, iterPREmax)
      else
        call solve33_DJDS_struct(np_smp, DJDS_comm, DJDS_tbl,           &
     &      MG_DJDS_mat(0), node%numnod, b_vec(1), x_vec(1),            &
     &      METHOD, PRECOND, ierr, eps, itr, itr_res)
      end if
!
      call end_eleps_time(5)
      if(iflag_debug .gt. 0) write(12,*) ' Iteration counts:', itr_res
!
      end subroutine solver_MGCG_vector
!
! ----------------------------------------------------------------------
!
      subroutine solver_MGCG_scalar                                     &
     &         (node, DJDS_comm, DJDS_tbl, mat_DJDS, num_MG_level,      &
     &          MG_itp, MG_comm, MG_DJDS_tbl, MG_DJDS_mat11,            &
     &          METHOD, PRECOND, eps, itr, MG_vector, b_vec, x_vec)
!
      use solver_DJDS11_struct
      use solver_VMGCG11_DJDS_SMP
!      use solver_CG
      use skip_comment_f
!
      character(len=kchara), intent(in) :: METHOD, PRECOND
      real(kind = kreal), intent(in) :: eps
      integer(kind = kint), intent(inout) :: itr
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: DJDS_comm
      type(DJDS_ordering_table), intent(in) :: DJDS_tbl
      type(DJDS_MATRIX), intent(in) :: mat_DJDS
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_itp(num_MG_level)
      type(communication_table), intent(in)                             &
     &                      :: MG_comm(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &                      :: MG_DJDS_tbl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: MG_DJDS_mat11(0:num_MG_level)
!
      type(vectors_4_solver), intent(inout)                             &
     &                        :: MG_vector(0:num_MG_level)
      real(kind = kreal), intent(inout) :: b_vec(node%numnod)
      real(kind = kreal), intent(inout) :: x_vec(node%numnod)
!
      integer(kind = kint) :: ierr, itr_res
!
!
      if (iflag_debug.eq.1) write(*,*) 'METHOD for scalar: ',           &
     &                                  trim(METHOD)
!
      call start_eleps_time(5)
      ierr = i_debug
!
!      call CG                                                          &
!     &   ( node%internal_node, node%numnod, ntot_l, ntot_u,            &
!     &     d_crs, al_crs, istack_l_crs, item_l_crs, au_crs,            &
!     &     istack_u_crs, item_u_crs, b_vec(1), x_vec(1),               &
!     &     precond_4_solver,1.0d0, 1.0d0, eps_4_temp_crank,            &
!     &     itr, ierr, my_rank,                                         &
!     &     DJDS_comm%num_neib, DJDS_comm%id_neib,                      &
!     &     DJDS_comm%istack_import, DJDS_comm%item_import,             &
!     &     DJDS_comm%istack_export, DJDS_comm%item_export, 1)
!
      if (cmp_no_case(METHOD, 'MGCG')) then
        call VMGCG11_DJDS_SMP(num_MG_level, MG_comm,                    &
     &      MG_itp, MG_DJDS_tbl, MG_DJDS_mat11, MG_vector, np_smp,      &
     &      node%numnod, b_vec(1), x_vec(1), itr,                       &
     &      itr_MG_mid, itr_MG_lowest, eps, EPS_MG,                     &
     &      PRECOND, METHOD_MG, PRECOND_MG, ierr, iterPREmax)
      else
        call solve_DJDS11_struct(np_smp, DJDS_comm, DJDS_tbl,           &
     &      mat_DJDS, node%numnod, b_vec(1), x_vec(1),                  &
     &      METHOD, PRECOND, ierr, eps, itr, itr_res)
      end if
!
      call end_eleps_time(5)
      if (iflag_debug .gt. 0) write(12,*) ' iteration counts', itr_res
!
      end subroutine solver_MGCG_scalar
!
! ----------------------------------------------------------------------
!
      end module solver_MGCG_MHD
