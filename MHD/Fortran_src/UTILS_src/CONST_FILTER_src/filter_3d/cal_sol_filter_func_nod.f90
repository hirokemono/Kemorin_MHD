!
!      module cal_sol_filter_func_nod
!
      module cal_sol_filter_func_nod
!
!     Written by H. Matsui on Nov., 2006
!
      use m_precision
!
      use m_parallel_var_dof
      use m_machine_parameter
!
      implicit none
!
      integer(kind=kint), parameter :: nset = 1
      integer(kind=kint) :: itr_res, imonitor_solve
      private :: itr_res, nset, imonitor_solve
!
      private :: cal_sol_filter_func_CG, cal_filter_func_nod_lu
      private :: cal_det_4_filter_func_lu
!
!      subroutine s_cal_sol_filter_func_nod(inod, ierr)
!      subroutine cal_det_4_filter_func_nod(inod, ierr)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_sol_filter_func_nod(inod, ierr)
!
      use copy_2_crs_matrix_4_filter
      use m_ctl_params_4_gen_filter
!
      integer(kind= kint), intent(in) :: inod
      integer(kind = kint), intent(inout) :: ierr
!
      call s_copy_2_crs_matrix_4_filter
      call cal_filter_func_nod_lu(ierr)
!
      if (ierr .gt. 0) return
!
      if (id_solver_type.eq.1) then
        call cal_sol_filter_func_CG(inod)
!      else
!        call cal_filter_func_nod_lu(ierr)
      end if
!
      end subroutine s_cal_sol_filter_func_nod
!
!-----------------------------------------------------------------------
!
      subroutine cal_det_4_filter_func_nod(ierr)
!
      use m_matrix_4_filter
!
      integer(kind = kint), intent(inout) :: ierr
      integer(kind= kint) :: i
!
!
      call cal_det_4_filter_func_lu(ierr)
!
      vec_norm = 0.0d0
      do i = 1, mat_size
        vec_norm = vec_norm + vec_mat(i)*vec_mat(i)
      end do
      vec_norm = sqrt(vec_norm)
      ratio_vec_mat=  vec_norm / det_mat
!
      end subroutine cal_det_4_filter_func_nod
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_sol_filter_func_CG(inod)
!
      use m_ctl_params_4_gen_filter
      use m_matrix_4_filter
      use m_crs_matrix_4_filter
      use copy_2_crs_matrix_4_filter
      use solver_single
!
      integer(kind = kint), intent(in) :: inod
!
!
      INTARRAY(1) = itr
      REALARRAY(1) = eps
      REALARRAY(2) = sigma_diag
      REALARRAY(3) = sigma
      imonitor_solve = i_debug
!
!
      call init_solver(ierr)
!
!      x_sol = 0.0d0
!
        if (my_rank .eq. 0 ) then
          write(*,*) 'solver no_mpi in', method, precond
        end if
!
        call solve(n_inter_crs, n_crs, npl_crs, npu_crs, diag_mat,      &
     &             al_mat, istack_l_crs(0), item_l_crs,                 &
     &             au_mat, istack_u_crs(0), item_u_crs, vec_mat, x_sol, &
     &             nset, my_rank, itr_res, imonitor_solve,              &
     &             method, precond, INTARRAY, REALARRAY )
!
         if (imonitor_solve .ne. 0 ) then
           write(12,*) ' iteration failed at :', my_rank, inod
         end if
!
         x_sol(mat_size+1:max_mat_size) = 0.0d0
!
      end subroutine cal_sol_filter_func_CG
!
!-----------------------------------------------------------------------
!
      subroutine cal_filter_func_nod_lu(ierr2)
!
      use m_parallel_var_dof
      use m_ctl_params_4_gen_filter
!
      use m_matrix_4_filter
      use m_ludcmp
!
      integer(kind = kint), intent(inout) :: ierr2
!
!
      x_sol = vec_mat
!
      call cal_det_4_filter_func_lu(ierr2)
!
      if (det_mat .lt. minimum_det_mat) then
        ierr2 = 10
        return
      end if
!
      call lubksb(a_mat, mat_size, max_mat_size, indx_mat, x_sol)
!
      x_sol(mat_size+1:max_mat_size) = 0.0d0
      ierr2 = 0
!
      end subroutine cal_filter_func_nod_lu
!
!-----------------------------------------------------------------------
!
      subroutine cal_det_4_filter_func_lu(ierr)
!
      use m_ctl_params_4_gen_filter
!
      use m_matrix_4_filter
      use m_ludcmp
!
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind= kint) :: i
!
!
      call ludcmp(a_mat, mat_size, max_mat_size, indx_mat, det_mat)
!
      do i = 1, mat_size
        det_mat = det_mat * a_mat(i,i)
      end do
!
      if (abs(det_mat) .lt. 1.0d-8 .and. mat_size .gt. 11) then
        ierr = 1
      end if
!
      end subroutine cal_det_4_filter_func_lu
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      end module cal_sol_filter_func_nod
