!
!      module cal_sol_filter_func_nod
!
!     Written by H. Matsui on Nov., 2006
!
!!      subroutine s_cal_sol_filter_func_nod                            &
!!     &         (inod, gfil_p, fil_mat, ierr)
!!        type(ctl_params_4_gen_filter), intent(in) :: gfil_p
!!        type(matrix_4_filter), intent(inout) :: fil_mat
!!      subroutine cal_det_4_filter_func_nod(fil_mat, fil_mat, ierr)
!!        type(matrix_4_filter), intent(inout) :: fil_mat
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
      use t_matrix_4_filter
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
      subroutine s_cal_sol_filter_func_nod                              &
     &         (inod, gfil_p, fil_mat, ierr)
!
      use m_crs_matrix_4_filter
      use t_ctl_params_4_gen_filter
      use copy_2_crs_matrix_4_filter
!
      integer(kind= kint), intent(in) :: inod
      type(ctl_params_4_gen_filter), intent(in) :: gfil_p
!
      type(matrix_4_filter), intent(inout) :: fil_mat
      integer(kind = kint), intent(inout) :: ierr
!
      call s_copy_2_crs_matrix_4_filter                                 &
     &   (fil_mat, fil_tbl_crs, fil_mat_crs)
      call cal_filter_func_nod_lu                                       &
     &   (gfil_p%minimum_det_mat, fil_mat, ierr)
!
      if (ierr .gt. 0) return
!
      if(gfil_p%id_solver_type .eq. 1) then
        call cal_sol_filter_func_CG(inod, gfil_p, fil_tbl_crs,         &
     &      fil_mat%max_mat_size, fil_mat%mat_size, fil_mat%vec_mat,   &
     &      fil_mat%x_sol, fil_mat_crs)
!      else
!        call cal_filter_func_nod_lu                                   &
!     &     (gfil_p%minimum_det_mat, fil_mat, ierr)
      end if
!
      end subroutine s_cal_sol_filter_func_nod
!
!-----------------------------------------------------------------------
!
      subroutine cal_det_4_filter_func_nod(fil_mat, ierr)
!
      type(matrix_4_filter), intent(inout) :: fil_mat
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind= kint) :: i
!
!
      call cal_det_4_filter_func_lu(fil_mat, ierr)
!
      fil_mat%vec_norm = 0.0d0
      do i = 1, fil_mat%mat_size
        fil_mat%vec_norm = fil_mat%vec_norm + fil_mat%vec_mat(i)**2
      end do
      fil_mat%vec_norm = sqrt(fil_mat%vec_norm)
      fil_mat%ratio_vec_mat=  fil_mat%vec_norm / fil_mat%det_mat
!
      end subroutine cal_det_4_filter_func_nod
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_sol_filter_func_CG(inod, gfil_p, fil_tbl_crs,      &
     &          max_size, mat_size, vec_mat, x_sol, fil_mat_crs)
!
      use t_ctl_params_4_gen_filter
      use copy_2_crs_matrix_4_filter
      use solver_single
!
      integer(kind = kint), intent(in) :: inod
      type(ctl_params_4_gen_filter), intent(in) :: gfil_p
      type(CRS_matrix_connect), intent(in) :: fil_tbl_crs
!
      integer(kind = kint), intent(in) :: mat_size, max_size
      real(kind = kreal), intent(inout) :: vec_mat(max_size)
      real(kind = kreal), intent(inout) :: x_sol(max_size)
      type(CRS_matrix), intent(inout) :: fil_mat_crs
!
      integer(kind = kint) :: imonitor_solve
      integer(kind = kint) :: ierr
!
!
      fil_mat_crs%PRESET_crs = 1
      fil_mat_crs%INTARRAY_crs(1) =  gfil_p%itr
      fil_mat_crs%REALARRAY_crs(1) = gfil_p%eps
      fil_mat_crs%REALARRAY_crs(2) = gfil_p%sigma_diag
      fil_mat_crs%REALARRAY_crs(3) = gfil_p%sigma
      imonitor_solve = i_debug
!
!
      call init_solver(ierr)
!
!      x_sol = 0.0d0
!
      if (my_rank .eq. 0 ) then
        write(*,*) 'solver no_mpi in', fil_mat_crs%METHOD_crs,          &
     &              fil_mat_crs%PRECOND_crs
      end if
!
      call solve(fil_tbl_crs%ntot_d, fil_tbl_crs%ntot_d,                &
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
      x_sol(mat_size+1:max_size) = 0.0d0
!
      end subroutine cal_sol_filter_func_CG
!
!-----------------------------------------------------------------------
!
      subroutine cal_filter_func_nod_lu                                 &
     &         (minimum_det_mat, fil_mat, ierr2)
!
      use calypso_mpi
!
      use m_ludcmp
!
      real(kind = kreal), intent(in) ::  minimum_det_mat
      type(matrix_4_filter), intent(inout) :: fil_mat
      integer(kind = kint), intent(inout) :: ierr2
!
!
      fil_mat%x_sol = fil_mat%vec_mat
!
      call cal_det_4_filter_func_lu(fil_mat, ierr2)
!
      if(fil_mat%det_mat .lt. minimum_det_mat) then
        ierr2 = 10
        return
      end if
!
      call lubksb(fil_mat%a_mat, fil_mat%mat_size,                      &
     &    fil_mat%max_mat_size, fil_mat%indx_mat, fil_mat%x_sol)
!
      fil_mat%x_sol(fil_mat%mat_size+1:fil_mat%max_mat_size) = 0.0d0
      ierr2 = 0
!
      end subroutine cal_filter_func_nod_lu
!
!-----------------------------------------------------------------------
!
      subroutine cal_det_4_filter_func_lu(fil_mat, ierr)
!
      use m_ludcmp
!
      type(matrix_4_filter), intent(inout) :: fil_mat
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind= kint) :: i
!
!
      call ludcmp(fil_mat%a_mat, fil_mat%mat_size,                      &
     &    fil_mat%max_mat_size, fil_mat%indx_mat, fil_mat%det_mat)
!
      do i = 1, fil_mat%mat_size
        fil_mat%det_mat = fil_mat%det_mat * fil_mat%a_mat(i,i)
      end do
!
      if (abs(fil_mat%det_mat) .lt. 1.0d-8                              &
     &   .and. fil_mat%mat_size .gt. 11)  ierr = 1
!
      end subroutine cal_det_4_filter_func_lu
!
!-----------------------------------------------------------------------
!
      end module cal_sol_filter_func_nod
