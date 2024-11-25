!
!     module cal_sol_deltax_by_consist
!
!     Written by H. Matsui on Nov., 2006
!     Modified by H. Matsui on Apr., 2008
!
!!      subroutine init_elapsed_solver_4_filter
!!      subroutine cal_sol_dx_by_consist(nd_dx, node, nod_comm, tbl_crs,&
!!     &          f_l, gfil_p, mass, dx_nod, v_sol, SR_sig, SR_r)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(CRS_matrix_connect), intent(in) :: tbl_crs
!!        type(finite_ele_mat_node), intent(in) :: f_l
!!        type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
!!        type(CRS_matrix), intent(inout) :: mass
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      module cal_sol_deltax_by_consist
!
      use m_precision
      use m_machine_parameter
!
      use t_comm_table
      use t_geometry_data
      use t_finite_element_mat
      use t_crs_matrix
      use t_vector_for_solver
!
      implicit none
!
      integer(kind=kint), parameter :: nset = 1
      integer(kind=kint) :: itr_res, imonitor_solve
      private :: itr_res, nset, imonitor_solve
!
      logical :: flag_FILTER_SOLVER_time = .FALSE.
      integer(kind = kint) :: ist_elapsed_SOLVER = 0
      integer(kind = kint) :: ied_elapsed_SOLVER = 0
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_elapsed_solver_4_filter
!
      use m_work_time
!
      integer(kind = kint), parameter :: num_append = 3
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_SOLVER, ied_elapsed_SOLVER)
!
      elps1%labels(ist_elapsed_SOLVER+ 1) = 'Solver Preconditioning   '
      elps1%labels(ist_elapsed_SOLVER+ 2) = 'Solver iteration time    '
      elps1%labels(ist_elapsed_SOLVER+ 3) = 'Solver communicatio time '
!
      flag_FILTER_SOLVER_time = .TRUE.
!
      end subroutine init_elapsed_solver_4_filter
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sol_dx_by_consist(nd_dx, node, nod_comm, tbl_crs,  &
     &          f_l, gfil_p, mass, dx_nod, v_sol, SR_sig, SR_r)
!
      use calypso_mpi
      use t_ctl_params_4_gen_filter
      use t_solver_SR
      use m_work_time
!
      use solver
!
      integer (kind = kint), intent(in) :: nd_dx
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(finite_ele_mat_node), intent(in) :: f_l
!
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
      type(CRS_matrix), intent(inout) :: mass
      real(kind= kreal), intent(inout) :: dx_nod(node%numnod)
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!      integer (kind = kint) :: inod
      real(kind = kreal) :: PRECtime, COMPtime, COMMtime
      integer(kind = kint) :: ierr
!
!
      call init_solver(ierr)
!
      mass%INTARRAY_crs(1) =  gfil_p%itr_elesize
      mass%REALARRAY_crs(1) = gfil_p%eps_elesize
      mass%REALARRAY_crs(2) = gfil_p%sigma_diag_elesize
      mass%REALARRAY_crs(3) = gfil_p%sigma_elesize
!
!
      imonitor_solve = i_debug
!
!$omp parallel workshare
      v_sol%x_vec(1:node%numnod) = f_l%ff(1:node%numnod,nd_dx)
      v_sol%b_vec(1:node%numnod) = f_l%ff(1:node%numnod,nd_dx)
!$omp end parallel workshare
!
!       write(50+my_rank,*) 'div_b'
!       do inod=1, node%numnod
!         write(50+my_rank,*) v_sol%b_vec(inod)
!       end do
!
      if (my_rank .eq. 0 ) then
          write(*,*) 'solver_in', nd_dx,                                &
     &              gfil_p%method_elesize, gfil_p%precond_elesize
      end if
!
      call solve(node%internal_node, node%numnod,                       &
     &             tbl_crs%ntot_l, tbl_crs%ntot_u, mass%D_crs,          &
     &             mass%AL_crs, tbl_crs%istack_l, tbl_crs%item_l,       &
     &             mass%AU_crs, tbl_crs%istack_u, tbl_crs%item_u,       &
     &             v_sol%b_vec(1), v_sol%x_vec(1), nset,                &
     &             nod_comm%num_neib, nod_comm%id_neib,                 &
     &             nod_comm%istack_import, nod_comm%item_import,        &
     &             nod_comm%istack_export, nod_comm%item_export,        &
     &             itr_res, imonitor_solve,                             &
     &             gfil_p%method_elesize, gfil_p%precond_elesize,       &
     &             mass%INTARRAY_crs, mass%REALARRAY_crs, SR_sig, SR_r, &
     &             PRECtime, COMPtime, COMMtime)
!
      if(flag_FILTER_SOLVER_time) then
        elps1%elapsed(ist_elapsed_SOLVER+1)                             &
     &     = elps1%elapsed(ist_elapsed_SOLVER+1) + PRECtime
        elps1%elapsed(ist_elapsed_SOLVER+2)                             &
     &     = elps1%elapsed(ist_elapsed_SOLVER+2) + COMPtime
        elps1%elapsed(ist_elapsed_SOLVER+3)                             &
     &     = elps1%elapsed(ist_elapsed_SOLVER+3) + COMMtime
      end if
!
      if(my_rank .eq. 0) then
        write(*,*) ' iteration finish:', itr_res
      end if
!
!$omp parallel workshare
      dx_nod(1:node%numnod) = v_sol%x_vec(1:node%numnod)
!$omp end parallel workshare
!
      end subroutine cal_sol_dx_by_consist
!
!  ---------------------------------------------------------------------
!
      end module cal_sol_deltax_by_consist
