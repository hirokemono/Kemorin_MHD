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
!!      subroutine solver_MGCG_velo
!!     &         (node, DJDS_comm_fl, DJDS_fluid, Vmat_DJDS,            &
!!     &          num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fl,     &
!!     &          MG_mat_velo, MG_vector, b_vec, x_vec)
!!      subroutine solver_MGCG_press
!!     &         (node, DJDS_comm_fl, DJDS_fl_l, Pmat_DJDS,             &
!!     &          num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fll,    &
!!     &          MG_mat_press, MG_vector, b_vec, x_vec)
!!      subroutine solver_MGCG_magne
!!     &         (node, DJDS_comm_etr, DJDS_entire, Bmat_DJDS,          &
!!     &          num_MG_level, MG_itp, MG_comm, MG_djds_tbl,           &
!!     &          MG_mat_magne, MG_vector, b_vec, x_vec)
!!      subroutine solver_MGCG_magne_p
!!     &         (node, DJDS_comm_etr, DJDS_linear, Fmat_DJDS,          &
!!     &          num_MG_level, MG_itp, MG_comm, MG_djds_tbl_l,         &
!!     &          MG_mat_magp, MG_vector, b_vec, x_vec)
!!      subroutine solver_MGCG_temp
!!     &         (node, DJDS_comm_fl, DJDS_fluid, Tmat_DJDS,            &
!!     &          num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fl,     &
!!     &          MG_mat_temp, MG_vector, b_vec, x_vec)
!!      subroutine solver_MGCG_d_scalar
!!     &         (node, DJDS_comm_fl, DJDS_fluid, Cmat_DJDS,            &
!!     &          num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fl,     &
!!     &          MG_mat_d_scalar, MG_vector, b_vec, x_vec)
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
     &         (node, DJDS_comm, DJDS_tbl, mat_DJDS, num_MG_level,      &
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
      type(DJDS_MATRIX), intent(in) :: mat_DJDS
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
      call start_eleps_time(5)
      ierr = i_debug
!
      if (cmp_no_case(METHOD, 'MGCG')) then
        call VMGCG33_DJDS_SMP(num_MG_level, MG_comm,                    &
     &      MG_itp, MG_DJDS_tbl, MG_DJDS_mat, MG_vector,                &
     &      np_smp, node%numnod, b_vec(1), x_vec(1), itr,               &
     &      itr_MG_mid, itr_MG_lowest, eps, EPS_MG,                     &
     &      PRECOND, METHOD_MG, PRECOND_MG, ierr, iterPREmax)
      else
        call solve33_DJDS_struct(np_smp, DJDS_comm, DJDS_tbl,           &
     &      mat_DJDS, node%numnod, b_vec(1), x_vec(1),                  &
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
      subroutine solver_MGCG_press                                      &
     &         (node, DJDS_comm_fl, DJDS_fl_l, Pmat_DJDS,               &
     &          num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fll,      &
     &          MG_mat_press, MG_vector, b_vec, x_vec)
!
      use m_iccg_parameter
      use solver_DJDS11_struct
      use solver_VMGCG11_DJDS_SMP
      use skip_comment_f
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: DJDS_comm_fl
      type(DJDS_ordering_table), intent(in) :: DJDS_fl_l
      type(DJDS_MATRIX), intent(in) :: Pmat_DJDS
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_itp(num_MG_level)
      type(communication_table), intent(in)                             &
     &                      :: MG_comm_fl(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &                      :: MG_djds_tbl_fll(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: MG_mat_press(0:num_MG_level)
!
      type(vectors_4_solver), intent(inout)                             &
     &                        :: MG_vector(0:num_MG_level)
      real(kind = kreal), intent(inout) :: b_vec(node%numnod)
      real(kind = kreal), intent(inout) :: x_vec(node%numnod)
!
      integer(kind = kint) :: ierr
!
!
       METHOD = method_4_solver
      if (iflag_debug.eq.1) then
        write(*,*) 'METHOD for pressure: ', trim(method_4_solver)
      end if
!
      call start_eleps_time(5)
      ierr = i_debug
!
      if (cmp_no_case(METHOD, 'MGCG')) then
        call VMGCG11_DJDS_SMP(num_MG_level, MG_comm_fl,                 &
     &      MG_itp, MG_djds_tbl_fll, MG_mat_press, MG_vector, np_smp,   &
     &      node%numnod, b_vec(1), x_vec(1), itr,                       &
     &      itr_MG_mid, itr_MG_lowest, eps, EPS_MG,                     &
     &      precond_4_solver, METHOD_MG, PRECOND_MG, ierr, iterPREmax)
      else
        call solve_DJDS11_struct(np_smp, DJDS_comm_fl, DJDS_fl_l,       &
     &      Pmat_DJDS, node%numnod, b_vec(1), x_vec(1),                 &
     &      method_4_solver, precond_4_solver, ierr, eps, itr, itr_res)
      end if
!
      call end_eleps_time(5)
       if (my_rank .eq. 0 ) then
         write(12,*) ' iteration_4_pressure:', itr_res
       end if
!
      end subroutine solver_MGCG_press
!
! ----------------------------------------------------------------------
!
      subroutine solver_MGCG_magne_p                                    &
     &         (node, DJDS_comm_etr, DJDS_linear, Fmat_DJDS,            &
     &          num_MG_level, MG_itp, MG_comm, MG_djds_tbl_l,           &
     &          MG_mat_magp, MG_vector, b_vec, x_vec)
!
      use m_iccg_parameter
      use solver_DJDS11_struct
      use solver_VMGCG11_DJDS_SMP
      use skip_comment_f
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: DJDS_comm_etr
      type(DJDS_ordering_table), intent(in) :: DJDS_linear
      type(DJDS_MATRIX), intent(in) :: Fmat_DJDS
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_itp(num_MG_level)
      type(communication_table), intent(in)                             &
     &                      :: MG_comm(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &                      :: MG_djds_tbl_l(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: MG_mat_magp(0:num_MG_level)
!
      type(vectors_4_solver), intent(inout)                             &
     &                        :: MG_vector(0:num_MG_level)
      real(kind = kreal), intent(inout) :: b_vec(node%numnod)
      real(kind = kreal), intent(inout) :: x_vec(node%numnod)
!
      integer(kind = kint) :: ierr
!
!
      METHOD = method_4_solver
      if (iflag_debug.eq.1) then
        write(*,*) 'METHOD for scalar potential: ',                     &
     &              trim(method_4_solver)
      end if
!
      call start_eleps_time(5)
      ierr = i_debug
!
      if (cmp_no_case(METHOD, 'MGCG')) then
        call VMGCG11_DJDS_SMP(num_MG_level, MG_comm,                    &
     &      MG_itp, MG_djds_tbl_l, MG_mat_magp, MG_vector, np_smp,      &
     &      node%numnod, b_vec(1), x_vec(1), itr,                       &
     &      itr_MG_mid, itr_MG_lowest, eps, EPS_MG,                     &
     &      precond_4_solver, METHOD_MG, PRECOND_MG, ierr, iterPREmax)
      else
        call solve_DJDS11_struct(np_smp, DJDS_comm_etr, DJDS_linear,    &
     &      Fmat_DJDS, node%numnod, b_vec(1), x_vec(1),                 &
     &      method_4_solver, precond_4_solver, ierr, eps, itr, itr_res)
      end if
!
      call end_eleps_time(5)
      if (my_rank .eq. 0 ) then
        write(12,*) ' iteration_4_mag_potential:', itr_res
      end if
!
      end subroutine solver_MGCG_magne_p
!
! ----------------------------------------------------------------------
!
      subroutine solver_MGCG_temp                                       &
     &         (node, DJDS_comm_fl, DJDS_fluid, Tmat_DJDS,              &
     &          num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fl,       &
     &          MG_mat_temp, MG_vector, b_vec, x_vec)
!
      use m_iccg_parameter
      use solver_DJDS11_struct
      use solver_VMGCG11_DJDS_SMP
!      use solver_CG
      use skip_comment_f
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: DJDS_comm_fl
      type(DJDS_ordering_table), intent(in) :: DJDS_fluid
      type(DJDS_MATRIX), intent(in) :: Tmat_DJDS
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_itp(num_MG_level)
      type(communication_table), intent(in)                             &
     &                      :: MG_comm_fl(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &                      :: MG_djds_tbl_fl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: MG_mat_temp(0:num_MG_level)
!
      type(vectors_4_solver), intent(inout)                             &
     &                       :: MG_vector(0:num_MG_level)
      real(kind = kreal), intent(inout) :: b_vec(node%numnod)
      real(kind = kreal), intent(inout) :: x_vec(node%numnod)
!
      integer(kind = kint) :: ierr
!
!
      METHOD = method_4_solver
      if (iflag_debug.eq.1) then
        write(*,*) 'METHOD for temp: ', trim(method_4_solver)
      end if
!
      call start_eleps_time(5)
      ierr = i_debug
!
!      call CG                                                          &
!     &   ( internal_node, node%numnod, ntot_l, ntot_u,                 &
!     &     d_crs, al_crs, istack_l_crs, item_l_crs, au_crs,            &
!     &     istack_u_crs, item_u_crs, b_vec(1), x_vec(1),               &
!     &     precond_4_solver,1.0d0, 1.0d0, eps_4_temp_crank,            &
!     &     itr, ierr, my_rank,                                         &
!     &     DJDS_comm_fl%num_neib, DJDS_comm_fl%id_neib,                &
!     &     DJDS_comm_fl%istack_import, DJDS_comm_fl%item_import,       &
!     &     DJDS_comm_fl%istack_export, DJDS_comm_fl%item_export, 1)
!       call deallocate_check_djds_array
!
      if (cmp_no_case(METHOD, 'MGCG')) then
        call VMGCG11_DJDS_SMP(num_MG_level, MG_comm_fl,                 &
     &      MG_itp, MG_djds_tbl_fl, MG_mat_temp, MG_vector, np_smp,     &
     &      node%numnod, b_vec(1), x_vec(1), itr,                       &
     &      itr_MG_mid, itr_MG_lowest, eps_4_temp_crank, EPS_MG,        &
     &      precond_4_solver, METHOD_MG, PRECOND_MG, ierr, iterPREmax)
      else
        call solve_DJDS11_struct(np_smp, DJDS_comm_fl, DJDS_fluid,      &
     &      Tmat_DJDS, node%numnod, b_vec(1), x_vec(1),                 &
     &      method_4_solver, precond_4_solver, ierr, eps_4_temp_crank,  &
     &      itr, itr_res)
      end if
!
      call end_eleps_time(5)
      if (my_rank .eq. 0 ) then
        write(12,*) ' iteration_4_temp.:', itr_res
      end if
!
      end subroutine solver_MGCG_temp
!
! ----------------------------------------------------------------------
!
      subroutine solver_MGCG_d_scalar                                   &
     &         (node, DJDS_comm_fl, DJDS_fluid, Cmat_DJDS,              &
     &          num_MG_level, MG_itp, MG_comm_fl, MG_djds_tbl_fl,       &
     &          MG_mat_d_scalar, MG_vector, b_vec, x_vec)
!
      use m_iccg_parameter
      use solver_DJDS11_struct
      use solver_VMGCG11_DJDS_SMP
      use skip_comment_f
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: DJDS_comm_fl
      type(DJDS_ordering_table), intent(in) :: DJDS_fluid
      type(DJDS_MATRIX), intent(in) :: Cmat_DJDS
!
      integer(kind = kint), intent(in) :: num_MG_level
      type(MG_itp_table), intent(in) :: MG_itp(num_MG_level)
      type(communication_table), intent(in)                             &
     &                      :: MG_comm_fl(0:num_MG_level)
      type(DJDS_ordering_table), intent(in)                             &
     &                      :: MG_djds_tbl_fl(0:num_MG_level)
      type(DJDS_MATRIX), intent(in) :: MG_mat_d_scalar(0:num_MG_level)
!
      type(vectors_4_solver), intent(inout)                             &
     &                       :: MG_vector(0:num_MG_level)
      real(kind = kreal), intent(inout) :: b_vec(node%numnod)
      real(kind = kreal), intent(inout) :: x_vec(node%numnod)
!
      integer(kind = kint) :: ierr
!
!
      METHOD = method_4_solver
      if (iflag_debug.eq.1) then
        write(*,*) 'METHOD for scalar: ', trim(method_4_solver)
      end if
!
      call start_eleps_time(5)
      ierr = i_debug
!
!       write(50+my_rank,*) 'inod, b_vec(inod), x_vec(inod)'
!       do inod = 1, node%numnod
!        write(50+my_rank,*) inod, b_vec(inod), x_vec(inod)
!       end do
!
      if (cmp_no_case(METHOD, 'MGCG')) then
        call VMGCG11_DJDS_SMP(num_MG_level, MG_comm_fl,                 &
     &      MG_itp, MG_djds_tbl_fl, MG_mat_d_scalar, MG_vector, np_smp, &
     &      node%numnod, b_vec(1), x_vec(1), itr,                       &
     &      itr_MG_mid, itr_MG_lowest, eps_4_comp_crank, EPS_MG,        &
     &      precond_4_solver, METHOD_MG, PRECOND_MG, ierr, iterPREmax)
      else
        call solve_DJDS11_struct(np_smp, DJDS_comm_fl, DJDS_fluid,      &
     &      Cmat_DJDS, node%numnod, b_vec(1), x_vec(1),                 &
     &      method_4_solver, precond_4_solver, ierr,                    &
     &      eps_4_comp_crank, itr, itr_res)
      end if
!
      call end_eleps_time(5)
      if (my_rank .eq. 0 ) then
        write(12,*) ' iteration for composition:', itr_res
      end if
!
      end subroutine solver_MGCG_d_scalar
!
! ----------------------------------------------------------------------
!
      end module solver_MGCG_MHD
