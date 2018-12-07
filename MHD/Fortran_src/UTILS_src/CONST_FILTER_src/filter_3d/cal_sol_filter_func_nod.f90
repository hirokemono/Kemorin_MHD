!
!      module cal_sol_filter_func_nod
!
!     Written by H. Matsui on Nov., 2006
!
!!      subroutine s_cal_sol_filter_func_nod(inod, ierr)
!!      subroutine cal_det_4_filter_func_nod(inod, ierr)
!
      module cal_sol_filter_func_nod
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
!
      use t_crs_connect
      use t_crs_matrix
!
      implicit none
!
      private :: cal_sol_filter_func_CG, cal_filter_func_nod_lu
      private :: cal_det_4_filter_func_lu
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_sol_filter_func_nod(inod, ierr)
!
      use m_crs_matrix_4_filter
      use copy_2_crs_matrix_4_filter
      use m_ctl_params_4_gen_filter
!
      integer(kind= kint), intent(in) :: inod
!
      integer(kind = kint), intent(inout) :: ierr
!
      call s_copy_2_crs_matrix_4_filter(fil_tbl_crs, fil_mat_crs)
      call cal_filter_func_nod_lu(ierr)
!
      if (ierr .gt. 0) return
!
      if (id_solver_type.eq.1) then
        call cal_sol_filter_func_CG(inod, fil_tbl_crs, fil_mat_crs)
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
      subroutine cal_sol_filter_func_CG(inod, fil_tbl_crs, fil_mat_crs)
!
      use m_ctl_params_4_gen_filter
      use m_matrix_4_filter
      use copy_2_crs_matrix_4_filter
      use solver_single
!
      integer(kind = kint), intent(in) :: inod
      type(CRS_matrix_connect), intent(in) :: fil_tbl_crs
      type(CRS_matrix), intent(inout) :: fil_mat_crs
!
      integer(kind = kint) :: imonitor_solve
      integer(kind = kint) :: ierr
!
!
      fil_mat_crs%PRESET_crs = 1
      fil_mat_crs%INTARRAY_crs(1) = itr
      fil_mat_crs%REALARRAY_crs(1) = eps
      fil_mat_crs%REALARRAY_crs(2) = sigma_diag
      fil_mat_crs%REALARRAY_crs(3) = sigma
      imonitor_solve = i_debug
!
!
      call init_solver(ierr)
!
!      x_sol = 0.0d0
!
        if (my_rank .eq. 0 ) then
          write(*,*) 'solver no_mpi in', fil_mat_crs%METHOD_crs,        &
     &              fil_mat_crs%PRECOND_crs
        end if
!
        call solve(fil_tbl_crs%ntot_d, fil_tbl_crs%ntot_d,              &
     &    fil_tbl_crs%ntot_l, fil_tbl_crs%ntot_u, fil_mat_crs%D_crs,    &
     &    fil_mat_crs%AL_crs, fil_tbl_crs%istack_l, fil_tbl_crs%item_l, &
     &    fil_mat_crs%AU_crs, fil_tbl_crs%istack_u, fil_tbl_crs%item_u, &
     &    vec_mat, x_sol, fil_mat_crs%PRESET_crs, my_rank,              &
     &    fil_mat_crs%ITERactual, imonitor_solve,                       &
     &    fil_mat_crs%METHOD_crs, fil_mat_crs%PRECOND_crs,              &
     &    fil_mat_crs%INTARRAY_crs, fil_mat_crs%REALARRAY_crs)
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
      use calypso_mpi
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
