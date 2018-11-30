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
!!      subroutine init_MGCG33_MHD(node, METHOD, PRECOND, MG_param)
!!      subroutine init_MGCG11_MHD(node, METHOD, PRECOND, MG_param)
!!
!!      subroutine solver_MGCG_vector(node, MG_param, num_MG_level,     &
!!     &          MG_itp, MG_comm, MG_DJDS_tbl, MG_DJDS_mat,            &
!!     &          METHOD, PRECOND, eps, itr, MG_vector, b_vec, x_vec)
!!      subroutine solver_MGCG_scalar(node, MG_param, num_MG_level,     &
!!     &          MG_itp, MG_comm, MG_DJDS_tbl, MG_DJDS_mat11,          &
!!     &          METHOD, PRECOND, eps, itr, MG_vector, b_vec, x_vec)
!!        type(node_data), intent(in) :: node
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(MGCG_parameter), intent(in) :: MG_param
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
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use calypso_mpi
!
      use t_physical_property
      use t_geometry_data
      use t_vector_for_solver
      use t_interpolate_table
      use t_solver_djds
      use t_MGCG_parameter
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
      subroutine init_MGCG33_MHD(node, METHOD, PRECOND, MG_param)
!
      use solver_DJDS11_struct
      use solver_DJDS33_struct
      use solver_VMGCG11_DJDS_SMP
      use solver_VMGCG33_DJDS_SMP
      use skip_comment_f
!
      character(len=kchara), intent(in) :: METHOD, PRECOND
      type(MGCG_parameter), intent(in) :: MG_param
      type(node_data), intent(in) :: node
!
      integer(kind = kint) :: ierr
!
!
      if (cmp_no_case(METHOD, 'MGCG')) then
        call init_VMGCG33_DJDS_SMP(node%numnod, np_smp, PRECOND,        &
            MG_param%METHOD_MG, MG_param%PRECOND_MG, iterPREmax)
      else
        call init33_DJDS_struct(node%numnod, np_smp, METHOD,            &
            PRECOND, ierr)
      end if
!
      end subroutine init_MGCG33_MHD
!
! ----------------------------------------------------------------------
!
      subroutine init_MGCG11_MHD(node, METHOD, PRECOND, MG_param)
!
      use solver_DJDS11_struct
      use solver_DJDS33_struct
      use solver_VMGCG11_DJDS_SMP
      use solver_VMGCG33_DJDS_SMP
      use skip_comment_f
!
      character(len=kchara), intent(in) :: METHOD, PRECOND
      type(MGCG_parameter), intent(in) :: MG_param
      type(node_data), intent(in) :: node
!
      integer(kind = kint) :: ierr
!
!
      if (cmp_no_case(METHOD, 'MGCG')) then
        call init_VMGCG11_DJDS_SMP(node%numnod, np_smp, PRECOND,        &
            MG_param%METHOD_MG, MG_param%PRECOND_MG, iterPREmax)
      else
        call init_DJDS11_struct(node%numnod, np_smp, METHOD,            &
            PRECOND, ierr)
      end if
!
      end subroutine init_MGCG11_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine solver_MGCG_vector(node, MG_param, num_MG_level,       &
     &          MG_itp, MG_comm, MG_DJDS_tbl, MG_DJDS_mat,              &
     &          METHOD, PRECOND, eps, itr, MG_vector, b_vec, x_vec)
!
      use solver_DJDS33_struct
      use solver_VMGCG33_DJDS_SMP
      use skip_comment_f
!
      type(MGCG_parameter), intent(in) :: MG_param
      character(len=kchara), intent(in) :: METHOD, PRECOND
      real(kind = kreal), intent(in) :: eps
      integer(kind = kint), intent(in) :: itr
!
      type(node_data), intent(in) :: node
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
      if(iflag_FMHD_time) call start_elapsed_time(ist_elapsed_FMHD+1)
      if (cmp_no_case(METHOD, 'MGCG')) then
        call VMGCG33_DJDS_SMP(num_MG_level, MG_comm,                    &
     &      MG_itp, MG_DJDS_tbl, MG_DJDS_mat, MG_vector,                &
     &      np_smp, node%numnod, b_vec(1), x_vec(1), itr, itr_res,      &
     &      MG_param%MID_ITR, MG_param%MIN_ITR, eps, MG_param%EPS_MG,   &
     &      PRECOND, MG_param%METHOD_MG, MG_param%PRECOND_MG,           &
     &      ierr, iterPREmax)
      else
        call solve33_DJDS_struct(np_smp, MG_comm(0), MG_DJDS_tbl(0),    &
     &      MG_DJDS_mat(0), node%numnod, b_vec(1), x_vec(1),            &
     &      METHOD, PRECOND, ierr, eps, itr, itr_res)
      end if
!
      if(iflag_FMHD_time) call end_elapsed_time(ist_elapsed_FMHD+1)
      if(iflag_debug .gt. 0) write(12,*) ' Iteration counts:', itr_res
!
      end subroutine solver_MGCG_vector
!
! ----------------------------------------------------------------------
!
      subroutine solver_MGCG_scalar(node, MG_param, num_MG_level,       &
     &          MG_itp, MG_comm, MG_DJDS_tbl, MG_DJDS_mat11,            &
     &          METHOD, PRECOND, eps, itr, MG_vector, b_vec, x_vec)
!
      use solver_DJDS11_struct
      use solver_VMGCG11_DJDS_SMP
!      use solver_CG
      use skip_comment_f
!
      type(MGCG_parameter), intent(in) :: MG_param
      character(len=kchara), intent(in) :: METHOD, PRECOND
      real(kind = kreal), intent(in) :: eps
      integer(kind = kint), intent(in) :: itr
!
      type(node_data), intent(in) :: node
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
      if(iflag_FMHD_time) call start_elapsed_time(ist_elapsed_FMHD+1)
      ierr = i_debug
!
!      call CG                                                          &
!     &   ( node%internal_node, node%numnod, ntot_l, ntot_u,            &
!     &     d_crs, al_crs, istack_l_crs, item_l_crs, au_crs,            &
!     &     istack_u_crs, item_u_crs, b_vec(1), x_vec(1),               &
!     &     precond_4_solver,1.0d0, 1.0d0, FEM_prm%eps_4_temp_crank,    &
!     &     itr, ierr, my_rank,                                         &
!     &     MG_comm(0)%num_neib, MG_comm(0)%id_neib,                    &
!     &     MG_comm(0)%istack_import, MG_comm(0)%item_import,           &
!     &     MG_comm(0)%istack_export, MG_comm(0)%item_export, 1)
!
      if (cmp_no_case(METHOD, 'MGCG')) then
        call VMGCG11_DJDS_SMP(num_MG_level, MG_comm,                    &
     &      MG_itp, MG_DJDS_tbl, MG_DJDS_mat11, MG_vector, np_smp,      &
     &      node%numnod, b_vec(1), x_vec(1), itr, itr_res,              &
     &      MG_param%MID_ITR, MG_param%MIN_ITR, eps, MG_param%EPS_MG,   &
     &      PRECOND, MG_param%METHOD_MG, MG_param%PRECOND_MG,           &
     &      ierr, iterPREmax)
      else
        call solve_DJDS11_struct(np_smp, MG_comm(0), MG_DJDS_tbl(0),    &
     &      MG_DJDS_mat11(0), node%numnod, b_vec(1), x_vec(1),          &
     &      METHOD, PRECOND, ierr, eps, itr, itr_res)
      end if
!
      if(iflag_FMHD_time) call end_elapsed_time(ist_elapsed_FMHD+1)
      if (iflag_debug .gt. 0) write(12,*) ' iteration counts', itr_res
!
      end subroutine solver_MGCG_scalar
!
! ----------------------------------------------------------------------
!
      end module solver_MGCG_MHD
